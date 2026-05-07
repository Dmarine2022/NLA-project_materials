
#Load required libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr) 
library(corrplot)
library(ggcorrplot)
library(rLakeAnalyzer)
library(ggplot2)
library(patchwork)
library(janitor)
library(party)
library(cowplot)
theme_set(theme_cowplot())
library(mgcv)
library(fitdistrplus)
library(multcompView)
library(scales)
library(vegan)
library(stringr)
library(maps)
library(factoextra)


#Load in rawdata from Github or you can can download the rawdata and change the path in our codes below 
##you will need to repeat this loading of raw data for year 2017 and 2012 data
#see code line 575 for loading 2017 data
#And line 1211 for 2012 data



#2022 DATA
##NLA22_waterchem data
WaterChem2022 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla22_waterchem_wide.csv")

##NLA22_Toxin data
toxin2022 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla22_algaltoxins.csv")

##NLA22_Secchi data
secchi2022 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla22_secchi.csv")

##NLA22_landscape data
landscape2022 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2022_landscape_wide_0.csv")

##NLA22_profile data
profile2022 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2022_profile_wide.csv")


##NLA22_siteinfo data
siteinfo2022 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla22_siteinfo.csv")
names(siteinfo2022)
#NLA22_Phytoplankton data
phytoplanktoncount2022_data <- read.csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2022_phytoplanktoncount_wide.csv")  



#pIVOT WHERE NECESSARY##################

#Toxin2022 Pivot wide
toxin2022_wide <- toxin2022 %>% 
  pivot_wider(
    names_from = ANALYTE,
    values_from = RESULT
  )


#CALCULATIONS####
#sECCHI CALCULATION (Average)#####
secchi2022_cal <- secchi2022 %>% 
  mutate(Secchi = (DISAPPEARS + REAPPEARS)/2)


#Select specific columns i.e. relevant columns for my work####
#WaterChem
#2022
WaterChem2022_subset <- WaterChem2022 %>%
  dplyr::select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLA_RESULT,
                CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NITRATE_N_RESULT, 
                NITRATE_NITRITE_N_RESULT, NITRITE_N_RESULT, NTL_DISS_RESULT, NTL_RESULT, PH_RESULT, POTASSIUM_RESULT,
                PTL_DISS_RESULT, PTL_RESULT, SODIUM_RESULT, SULFATE_RESULT, TURB_RESULT
  )

#Select Toxin
#2022
names(toxin2022_wide)
toxin2022_subset <- toxin2022_wide %>% 
  dplyr::select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, MICX, CYLSPER)


# Select SECCHI
#2022
names(secchi2022_cal)

secchi2022_sebset <- secchi2022_cal %>% 
  dplyr::select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, Secchi)


# Select landscape (elevation)
#2022 select for Elevation

landscape2022_sebset <- landscape2022 %>% 
  dplyr::select(UNIQUE_ID, SITE_ID, ELEV, ELEV_MAX, ELEV_MIN) #Note : no DATE_COL and VISIT_NO in landscape Data

# select profile
#1-depth-averaged values across all depths
mean_profiles_alldepths <- profile2022 %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_mean = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_mean = mean(OXYGEN, na.rm = TRUE),
    pH_mean = mean(PH, na.rm = TRUE),
    Conductivity_mean = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#2-calculate mean of temp, DO, pH, conductivity at the top 1m
mean_profiles_top1m <- profile2022 %>%
  filter(DEPTH <= 1) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_top1m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_top1m = mean(OXYGEN, na.rm = TRUE),
    pH_top1m = mean(PH, na.rm = TRUE),
    Conductivity_top1m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#3-compute the mean between 1 and 2 meters
mean_profiles_1to2m <- profile2022 %>%
  filter(DEPTH >= 1, DEPTH <= 2) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_1to2m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_1to2m = mean(OXYGEN, na.rm = TRUE),
    pH_1to2m = mean(PH, na.rm = TRUE),
    Conductivity_1to2m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )
#4- compute the mean between 2 and 4 meters
mean_profiles_2to4m <- profile2022 %>%
  filter(DEPTH >= 2, DEPTH <= 4) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_1to2m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_1to2m = mean(OXYGEN, na.rm = TRUE),
    pH_1to2m = mean(PH, na.rm = TRUE),
    Conductivity_1to2m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )
#5- compute the mean below 4 meters
mean_profiles_below4m <- profile2022 %>%
  filter(DEPTH >= 4) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_below4m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_below4m = mean(OXYGEN, na.rm = TRUE),
    pH_below4m = mean(PH, na.rm = TRUE),
    Conductivity_below4m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )
#6- compute the mean below 5 meters
mean_profiles_below5m <- profile2022 %>%
  filter(DEPTH >= 5) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_below5m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_below5m = mean(OXYGEN, na.rm = TRUE),
    pH_below5m = mean(PH, na.rm = TRUE),
    Conductivity_below5m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )


# select siteinfo
#2022

names(siteinfo2022)
siteinfo2022_subset <- siteinfo2022 %>% 
  dplyr::select(UNIQUE_ID, SITE_ID, AREA_HA, ELEVATION,LAKE_ORGN, LAT_DD83, LON_DD83, INDEX_SITE_DEPTH, AG_ECO9) #note: we have elevation in landscape data too



# Select and calculation on phytoplankton data

# We define PTOX taxa based on Chapman & Foss (2020); Chorus & Welker (2021).
ptox_taxa <- c("ANABAENOPSIS", "ANABAENA", "APHANIZOMENON", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM", "CUSPIDOTHRIX",
               "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DESMONOSTOC",  "DOLICHOSPERMUM", "FISCHERELLA", "GEITLERINEMA", 
               "GLOEOTRICHIA", "HAPALOSIPHON", "LEPTOLYNGBYA", "PLECTONEMA", "LIMNOTHRIX", "MERISMOPEDIA", "MICROCOLEUS",
               "PHORMIDIUM", "MICROCYSTIS", "MICROSEIRA", "LYNGBYA", "NOSTOC", "OSCILLATORIA", "PLANKTOTHRIX", "PSEUDANABAENA",
               "RADIOCYSTIS", "RIVULARIA", "ROMERIA", "SCYTONEMA", "SNOWELLA", "SPHAEROSPERMOPSIS", "STENOMITOS", "SYNECHOCOCCUS",
               "SYNECHOCYSTIS", "TOLYPOTHRIX", "TRICHODESMIUM", "TRICHORMUS", "UMEZAKIA", "WORONICHINIA")

ptox_mic_taxa <- c("ANABAENOPSIS", "ANABAENA", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM","DESMONOSTOC",  "DOLICHOSPERMUM", "FISCHERELLA", "GEITLERINEMA", 
                   "GLOEOTRICHIA", "HAPALOSIPHON", "LEPTOLYNGBYA", "LIMNOTHRIX", "MERISMOPEDIA", "MICROCOLEUS","PHORMIDIUM", "MICROCYSTIS", "NOSTOC", "OSCILLATORIA", 
                   "PLANKTOTHRIX", "PSEUDANABAENA", "RADIOCYSTIS", "RIVULARIA", "ROMERIA", "SCYTONEMA", "SNOWELLA", "SPHAEROSPERMOPSIS", "STENOMITOS", "SYNECHOCOCCUS",
                   "SYNECHOCYSTIS", "TOLYPOTHRIX", "TRICHODESMIUM", "TRICHORMUS", "WORONICHINIA")

ptox_CYL_taxa <- c("ANABAENA", "APHANIZOMENON","CHRYSOSPORUM",
                   "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DOLICHOSPERMUM",
                   "MICROSEIRA", "LYNGBYA", "OSCILLATORIA","SPHAEROSPERMOPSIS","UMEZAKIA")


phyto2022_summary <- phytoplanktoncount2022_data %>%
  group_by(SITE_ID, DATE_COL) %>%
  summarise(
    # Total biovolume calculations
    total_phytoplankton_biovolume = sum(BIOVOLUME, na.rm = TRUE),
    total_cyanobacteria_biovolume = sum(BIOVOLUME[ALGAL_GROUP == "CYANOBACTERIA"], na.rm = TRUE),
    
    # Total density calculations
    total_phytoplankton_density = sum(DENSITY, na.rm = TRUE),
    total_cyanobacteria_density = sum(DENSITY[ALGAL_GROUP == "CYANOBACTERIA"], na.rm = TRUE),
    
    # Total abundance calculations
    total_phytoplankton_abundance = sum(ABUNDANCE, na.rm = TRUE),
    total_cyanobacteria_abundance = sum(ABUNDANCE[ALGAL_GROUP == "CYANOBACTERIA"], na.rm = TRUE),
    
    # PTOX Biovolume Calculation: Which is the Sum of biovolume where "TARGET_TAXON" matches "PTOX taxa"
    ##To ensure that any species containing the name "Anabaena" for example (including "Anabaena oscillarioides" etc) is captured, we modify the code to use pattern matching with grepl()
    ##grepl(pattern, TARGET_TAXON, ignore.case = TRUE)
    ##Checks if each TARGET_TAXON contains any word from ptox_taxa.
    ##Example: "Anabaena oscillarioides" matches "Anabaena".
    ##BIOVOLUME[grepl(...)]
    ##Filters BIOVOLUME only where the taxon contains a PTOX keyword.
    PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE),   #selects only the BIOVOLUME values where TARGET_TAXON matches a taxon in ptox_taxa.
    MIC_PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_mic_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE), 
    CYL_PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_CYL_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE) 
  ) %>%
  mutate(
    percent_cyanobacteria_biovolume = (total_cyanobacteria_biovolume / total_phytoplankton_biovolume) * 100,
    percent_cyanobacteria_density = (total_cyanobacteria_density / total_phytoplankton_density) * 100,
    percent_cyanobacteria_abundance = (total_cyanobacteria_abundance / total_phytoplankton_abundance) * 100,
    percent_PTOX_biovolume = (PTOX_biovolume / total_cyanobacteria_biovolume) * 100,   # % PTOX biovolume relative to total_cyanobacteria_biovolume
    percent_MIC_PTOX_biovolume = (MIC_PTOX_biovolume / total_cyanobacteria_biovolume) * 100,
    percent_CYL_PTOX_biovolume = (CYL_PTOX_biovolume / total_cyanobacteria_biovolume) * 100
  )


####################################################################################

###TO DEAL WITH BDL (non detect-ND),we use half detection limit#######
##Toxin MDL MICX = 0.1 ug/L; CYLSPER = 0.05 ug/L
#2022
toxin2022_DL <- toxin2022_subset %>% 
  mutate(MICX = ifelse(is.na(MICX), 0.1 / 2, MICX),
         CYLSPER = ifelse(is.na(CYLSPER), 0.05 / 2, CYLSPER)
  )
head(toxin2022_DL) #CHECK HEAD
colSums(is.na(toxin2022_DL)) #cHECK NA count again


###JOIN SEBSET DATA#####
##To combine the datasets####
#2022
combined_data <- left_join(WaterChem2022_subset, toxin2022_DL, 
                           by = c("UNIQUE_ID", "SITE_ID", "DATE_COL", "VISIT_NO"))
#join secchi
combined_data2 <- left_join(combined_data, secchi2022_sebset,
                            by = c("UNIQUE_ID", "SITE_ID", "DATE_COL", "VISIT_NO"))
#join landscape
combined_data3 <- left_join(combined_data2, landscape2022_sebset,
                            by = c("UNIQUE_ID", "SITE_ID"))
#Join mean_profiles_top1m
combined_data4 <- left_join(combined_data3, mean_profiles_top1m,       #Note: Consider joining the "mean_profiles_alldepths" later
                            by = c("UNIQUE_ID", "SITE_ID", "DATE_COL", "VISIT_NO"))

#join site information
combined_data5 <- left_join(combined_data4, siteinfo2022_subset,
                            by = c("UNIQUE_ID", "SITE_ID"))

combined_data5b <- left_join(combined_data4, siteinfo2022_subset, 
                             by = c("UNIQUE_ID", "SITE_ID"),
                             relationship = "many-to-many") #note

View(combined_data5b)

#######################
#TN-TP
# TN:TP ratio by weight

# Convert NTL from mg/L to µg/L
combined_data5b <- combined_data5b %>%
  mutate(
    NTL_ugL = NTL_RESULT * 1000,  # Convert NTL to µg/L
    TN_TP_RATIO = NTL_ugL / PTL_RESULT  # Compute TN:TP ratio
  )
##############################################
#Join phyto data

combined_data6 <- left_join(combined_data5b, phyto2022_summary, 
                            by = c("SITE_ID", "DATE_COL"),
                            relationship = "many-to-many") #note
names(combined_data6)



################################################Diversity indices 

library(vegan)
# Ensure the data structure is correct
str(phytoplanktoncount2022_data)

# Aggregate abundance by taxon and sample ("SITE_ID")
phyto_wide <- phytoplanktoncount2022_data %>%
  group_by(SITE_ID, TARGET_TAXON) %>%
  summarise(Abundance = sum(ABUNDANCE, na.rm = TRUE)) %>%
  pivot_wider(names_from = TARGET_TAXON, values_from = Abundance, values_fill = 0) %>%
  ungroup()

# Convert to matrix format for diversity calculations
phyto_matrix <- as.matrix(phyto_wide[,-1])  # Remove Sample_ID column

# Shannon Diversity Index
shannon_index <- diversity(phyto_matrix, index = "shannon")

# Simpson Diversity Index
simpson_index <- diversity(phyto_matrix, index = "simpson")

# Evenness (Shannon Index / log(S), where S = number of species)
evenness <- shannon_index / log(specnumber(phyto_matrix))

# Create results dataframe
diversity_results <- data.frame(
  SITE_ID = phyto_wide$SITE_ID,
  Shannon_Index = shannon_index,
  Simpson_Index = simpson_index,
  Evenness = evenness
)

# Print results
print(diversity_results)

########################################################Join phyto data with diversity_results
#Join phyto data with diversity_results

combined_data7A <- left_join(combined_data6, diversity_results, 
                             by = c("SITE_ID"),
                             relationship = "many-to-many") #note
names(combined_data7A)
#####################################################################################
#####################################################################################Nitrate/nitrite Mg/l combined
#combine/sum the three columns NITRATE_N, NITRATE_NITRITE_N, and NITRITE_N into a new column called NITRATE_NITRITE

combined_data7A$NITRATE_NITRITE <- rowSums(
  combined_data7A[, c("NITRATE_N_RESULT", "NITRATE_NITRITE_N_RESULT", "NITRITE_N_RESULT")],
  na.rm = TRUE
)
names(combined_data7A)
############################################################################################
################################################################################
#Group by Taxonomy (Genus)

library(dplyr)
library(stringr)

# Group by genus (first part of the species name) and sum the biovolume
phytoplankton_grouped <- phytoplanktoncount2022_data %>%
  mutate(
    Genus = word(TARGET_TAXON, 1)  # Extract the first word as genus
  ) %>%
  group_by(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, Genus) %>%
  summarize(Total_Biovolume = sum(BIOVOLUME, na.rm = TRUE), .groups = "drop")

# Check the structure
str(phytoplankton_grouped)

#MAKE WIDER
phytoplankton_wide_GROUP <- phytoplankton_grouped %>%
  pivot_wider(
    id_cols = c(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO),
    names_from = Genus,
    values_from = Total_Biovolume,
    values_fill = 0
  )

names(phytoplankton_wide_GROUP)

selected_phytoplankton_wide_GROUP <- phytoplankton_wide_GROUP %>%
  dplyr::select(SITE_ID, ANABAENOPSIS, ANABAENA, APHANIZOMENON, APHANOCAPSA, ARTHROSPIRA, CHRYSOSPORUM, CUSPIDOTHRIX, #rEMOVE SITE ID TO RUN CORRELATION TEST
                RAPHIDIOPSIS, CYLINDROSPERMOPSIS, DOLICHOSPERMUM, GEITLERINEMA,    #NO DESMONOSTOC, FISCHERELLA, HAPALOSIPHON, PLECTONEMA, MICROCOLEUS Genus
                GLOEOTRICHIA, LEPTOLYNGBYA, LIMNOTHRIX, MERISMOPEDIA, 
                PHORMIDIUM, MICROCYSTIS, LYNGBYA, NOSTOC, OSCILLATORIA, PLANKTOTHRIX, PSEUDANABAENA, #No MICROSEIRA, RIVULARIA, SCYTONEMA, STENOMITOS, TOLYPOTHRIX, TRICHODESMIUM,
                RADIOCYSTIS, ROMERIA, SNOWELLA, SPHAEROSPERMOPSIS, SYNECHOCOCCUS,           #No TRICHORMUS, UMEZAKIA,
                SYNECHOCYSTIS,  WORONICHINIA)

#################################
#create new combined data to select lat and long

names(combined_data7A)

selected_data_combine_NEW <- dplyr::select(
  combined_data7A, SITE_ID, UNIQUE_ID, LAT_DD83, LON_DD83, MICX, CYLSPER, CHLA_RESULT, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, PTOX_biovolume, percent_cyanobacteria_biovolume,
  percent_PTOX_biovolume, MIC_PTOX_biovolume, CYL_PTOX_biovolume, percent_MIC_PTOX_biovolume, percent_CYL_PTOX_biovolume, Shannon_Index, Simpson_Index, Evenness, Temp_top1m, Secchi, pH_top1m, PH_RESULT, ELEVATION, INDEX_SITE_DEPTH, 
  AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NTL_RESULT, PTL_RESULT,
  NTL_DISS_RESULT, PTL_DISS_RESULT, SODIUM_RESULT, TURB_RESULT, SULFATE_RESULT, POTASSIUM_RESULT, AREA_HA, LAKE_ORGN, AG_ECO9, NITRATE_NITRITE, TN_TP_RATIO
)

###############################################################################################################
# Merge datasets by UNIQUE_ID
combined_data_pca_NEW <- selected_data_combine_NEW %>%
  left_join(selected_phytoplankton_wide_GROUP, by = "SITE_ID", relationship = "many-to-many")
#################################################################################################################
# Create a new salinity column as the sum of selected ion concentrations
combined_data_pca_NEW$salinity <- rowSums(combined_data_pca_NEW[, c("CALCIUM_RESULT", 
                                                                    "MAGNESIUM_RESULT", 
                                                                    "SODIUM_RESULT", 
                                                                    "POTASSIUM_RESULT", 
                                                                    "CHLORIDE_RESULT",
                                                                    "SULFATE_RESULT")], 
                                          na.rm = TRUE)

names(combined_data_pca_NEW)
##############################################################
########################################################################################################### CREATE TROPHIC CLASSIFICATION

combined_data_pca_NEW <- combined_data_pca_NEW %>%
  filter(!is.na(CHLA_RESULT)) %>%
  mutate(
    trophic_status = case_when(
      CHLA_RESULT <= 2 ~ "Oligotrophic",
      CHLA_RESULT > 2 & CHLA_RESULT <= 7 ~ "Mesotrophic",
      CHLA_RESULT > 7 & CHLA_RESULT <= 30 ~ "Eutrophic",
      CHLA_RESULT > 30 ~ "Hypereutrophic",
      TRUE ~ NA_character_
    )
  )

# Ensure the trophic_status is a factor with the correct order
combined_data_pca_NEW <- combined_data_pca_NEW %>%
  mutate(trophic_status = factor(trophic_status,
                                 levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic")))

names(combined_data_pca_NEW)
#######################################################JOIN combined_data_pca_NEW WITH Thermocline Depth and ZMAX data
###################################################Calculate Thermocline Depth per Site
library(dplyr)

# Ensure numeric types
profile2022 <- profile2022 %>%
  mutate(DEPTH = as.numeric(DEPTH),
         TEMPERATURE = as.numeric(TEMPERATURE)) %>%
  filter(!is.na(DEPTH) & !is.na(TEMPERATURE))  # Remove rows with missing values

# Function to calculate thermocline depth
get_thermocline <- function(depth, temp) {
  if (length(depth) < 2) return(NA)
  ord <- order(depth)
  d <- depth[ord]
  t <- temp[ord]
  d_diff <- diff(d)
  t_diff <- diff(t)
  gradients <- abs(t_diff / d_diff)
  # Thermocline is where gradient is maximum
  if (length(gradients) == 0) return(NA)
  return(d[which.max(gradients)])
}

# Calculate thermocline per site
thermocline_per_site <- profile2022 %>%
  group_by(SITE_ID) %>%
  summarise(thermocline_depth = get_thermocline(DEPTH, TEMPERATURE),
            zmax = max(DEPTH, na.rm = TRUE)) %>%
  ungroup()

# View results
head(thermocline_per_site)

############################################################## verify Thermocline Depth WITH rLakeAnalyzer
# Install rLakeAnalyzer if not already installed
#install.packages("rLakeAnalyzer")  # if not on CRAN, install via GitHub: devtools::install_github("UW-FHL/rLakeAnalyzer")
#Note that in very shallow lakes thermocline may return NA.

# Load the package
library(rLakeAnalyzer)
library(dplyr)

# Clean and filter data
profile_clean <- profile2022 %>%
  filter(!is.na(DEPTH), !is.na(TEMPERATURE)) %>%
  mutate(DEPTH = as.numeric(DEPTH),
         TEMPERATURE = as.numeric(TEMPERATURE))

# Function to calculate thermocline depth per site using rLakeAnalyzer
get_thermocline_rLake <- function(df) {
  df <- df[order(df$DEPTH), ]  # ensure increasing depth
  tryCatch({
    thermo.depth(wtr = df$TEMPERATURE, depths = df$DEPTH)
  }, error = function(e) NA)
}

# Apply per SITE_ID
thermocline_rLake <- profile_clean %>%
  group_by(SITE_ID) %>%
  summarise(thermocline_depth = get_thermocline_rLake(cur_data_all()),
            zmax = max(DEPTH, na.rm = TRUE)) %>%
  ungroup()

# View result
head(thermocline_rLake)

#REMOVE ZMAX and remane thermocline_depth as thermocline_depth_rLake
thermocline_rLake2 <- profile_clean %>%
  group_by(SITE_ID) %>%
  summarise(thermocline_depth_rLake = get_thermocline_rLake(cur_data_all())) %>%
  ungroup()

###############################################################COMBINE THE TWO THERMO CALCULATION

# Merge datasets 
thermocline_depth_data <- thermocline_per_site %>%
  left_join(thermocline_rLake2 , by = "SITE_ID")

#######################################################JOIN combined_data_pca_NEW WITH Thermocline Depth and ZMAX data
########################### Merge datasets 
combined_data_pca_NEW <- combined_data_pca_NEW %>%
  left_join(thermocline_depth_data, by = "SITE_ID")

names(combined_data_pca_NEW)

##############################
#cHECK RELATIONSHIPS BETWEN INDEX SITE DEPTH AND THE NEW ZMAX CALCULATED

Zmax_compare2022 <- combined_data_pca_NEW %>%
  ggplot(aes(x = INDEX_SITE_DEPTH, y = zmax)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2, size = 9,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw(base_size = 16) +
  labs(title = "(A) NLA data 2022") +
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  )
Zmax_compare2022


#RHO=0.99 WITH P LESS THAN 0.0001

combined_data_pca_NEW %>%
  ggplot(aes(x = thermocline_depth, y = CYLSPER)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw()

###########################################################################CHECK 2022 selected data BELOW
names(combined_data_pca_NEW)



#######################################################################2017####repeat processes done in 2022 for 2017
#2017 DATA 
##NLA17_waterchem data
WaterChem2017 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla_2017_water_chemistry_chla-data.csv")

##NLA17_Toxin data
toxin2017 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla_2017_algal_toxin-data.csv")

##NLA17_Secchi data
secchi2017 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla_2017_secchi-data.csv")

##NLA17_profile data
#file too large to load using raw from Github it always download on the local machine. 
profile2017 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla_2017_profile-data.csv")

##NLA17_siteinfo data
siteinfo2017 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla_2017_site_information-data.csv")

#NLA17_Phytoplankton data
phytoplanktoncount2017_data <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla_2017_phytoplankton_count-data.csv")  

#############################################################

#pIVOT WHERE NECESSARY##################

#Pivot waterchem data (remember chl-a is inside)
#Note: BATCH_ID in the ANALYTE is null, but causing error. bELOW ARE STEPS TAKEN TO RESOLVE THE ISSUE.
# Check for instances where ANALYTE is "BATCH_ID"
WaterChem2017 %>% 
  filter(ANALYTE == "BATCH_ID")
# Remove rows where ANALYTE is "BATCH_ID"
WaterChem2017 <- WaterChem2017 %>% 
  filter(ANALYTE != "BATCH_ID")


# Aggregate by taking the mean of RESULT observations 
WaterChem2017b <- WaterChem2017 %>%
  group_by(UID, SITE_ID, DATE_COL, ANALYTE) %>%
  summarize(RESULT = mean(RESULT, na.rm = TRUE), .groups = "drop")

str(WaterChem2017b)
summary(WaterChem2017b)

#PIVORT WIDER
WaterChem2017_wideb <- WaterChem2017b %>% 
  pivot_wider(
    names_from = ANALYTE,
    values_from = RESULT,
    names_repair = "unique"
  )

# Check the structure of DATE_COL in both datasets
str(WaterChem2017_wideb$DATE_COL)
##################################################
# Check unique date formats
unique(WaterChem2017_wideb$DATE_COL)
##################################################
# Convert "30-May-17" format to Date
WaterChem2017_wideb$DATE_COL <- dmy(WaterChem2017_wideb$DATE_COL)
# Check the conversion
head(WaterChem2017_wideb$DATE_COL)


#Toxin2017 Pivot wide
#Note: Code identifying sample type: MICX=Microcystin, CYLSPER=Cylindrospermopsin, MICZ=Microcystin in legacy bottle type"
toxin2017_wide <- toxin2017 %>% 
  pivot_wider(
    names_from = ANALYTE,
    values_from = RESULT
  )
##################################################
#CHECK DATES
str(toxin2017_wide$DATE_COL)
# Convert "5/30/2017" format to Date
toxin2017_wide$DATE_COL <- mdy(toxin2017_wide$DATE_COL)
# Check the conversion
head(toxin2017_wide$DATE_COL)
str(toxin2017_wide$DATE_COL)
###################################################

#CALCULATIONS####
#sECCHI CALCULATION (Average)#####
secchi2017_cal <- secchi2017 %>% 
  mutate(Secchi = (DISAPPEARS + REAPPEARS)/2)


##################################################
#CHECK DATES
str(secchi2017_cal$DATE_COL)
# Convert "5/30/2017" format to Date
secchi2017_cal$DATE_COL <- mdy(secchi2017_cal$DATE_COL)
# Check the conversion
head(secchi2017_cal$DATE_COL)
str(secchi2017_cal$DATE_COL)
###################################################


#Select specific columns i.e. relevant columns for my work####
#WaterChem
#2017

names(WaterChem2017_wideb)
WaterChem2017_subset <- WaterChem2017_wideb %>%         #Note the waterchem data here is the aggregated one
  dplyr::select(UID, SITE_ID, DATE_COL, AMMONIA_N, ANC, CALCIUM, CHLA, #Visit has been taken off
                CHLORIDE, COLOR, COND, DOC, MAGNESIUM, NITRATE_N, 
                NITRATE_NITRITE_N, NITRITE_N, NTL, PH, POTASSIUM,
                PTL, SODIUM, SULFATE, TURB)


#Select Toxin
#2017
names(toxin2017_wide)
toxin2017_subset <- toxin2017_wide %>% 
  dplyr::select(UID, SITE_ID, DATE_COL, VISIT_NO, MICX, CYLSPER)


# Select SECCHI
#2017
names(secchi2017_cal)  

secchi2017_sebset <- secchi2017_cal %>% 
  dplyr::select(UID, SITE_ID, DATE_COL, VISIT_NO, Secchi, INDEX_SITE_DEPTH)



#DATE FOR PROFILE DATA
##################################################
#CHECK DATES
str(profile2017$DATE_COL)
# Convert "30-May-17" format to Date
profile2017$DATE_COL <- dmy(profile2017$DATE_COL)
# Check the conversion
head(profile2017$DATE_COL)
str(profile2017$DATE_COL)
###################################################

# select profile
names(profile2017)
#1-depth-averaged values across all depths
mean_profiles2017_alldepths <- profile2017 %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_mean = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_mean = mean(OXYGEN, na.rm = TRUE),
    pH_mean = mean(PH, na.rm = TRUE),
    Conductivity_mean = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#2-calculate mean of temp, DO, pH, conductivity at the top 1m
mean_profiles2017_top1m <- profile2017 %>%
  filter(DEPTH <= 1) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_top1m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_top1m = mean(OXYGEN, na.rm = TRUE),
    pH_top1m = mean(PH, na.rm = TRUE),
    Conductivity_top1m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#3-compute the mean between 1 and 2 meters
mean_profiles2017_1to2m <- profile2017 %>%
  filter(DEPTH >= 1, DEPTH <= 2) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_1to2m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_1to2m = mean(OXYGEN, na.rm = TRUE),
    pH_1to2m = mean(PH, na.rm = TRUE),
    Conductivity_1to2m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#4- compute the mean between 2 and 4 meters
mean_profiles2017_2to4m <- profile2017 %>%
  filter(DEPTH >= 2, DEPTH <= 4) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_1to2m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_1to2m = mean(OXYGEN, na.rm = TRUE),
    pH_1to2m = mean(PH, na.rm = TRUE),
    Conductivity_1to2m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#5- compute the mean below 4 meters
mean_profiles2017_below4m <- profile2017 %>%
  filter(DEPTH >= 4) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_below4m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_below4m = mean(OXYGEN, na.rm = TRUE),
    pH_below4m = mean(PH, na.rm = TRUE),
    Conductivity_below4m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#6- compute the mean below 5 meters
mean_profiles2017_below5m <- profile2017 %>%
  filter(DEPTH >= 5) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_below5m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_below5m = mean(OXYGEN, na.rm = TRUE),
    pH_below5m = mean(PH, na.rm = TRUE),
    Conductivity_below5m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

# select siteinfo
#2017 -(INDEX_SITE_DEPTH not found in site information data but is available in secchi data)
names(siteinfo2017) #DATE_COL Column is mainly empty in the original data.
siteinfo2017_subset <- siteinfo2017 %>% 
  dplyr::select(UNIQUE_ID, SITE_ID, AREA_HA, ELEVATION,LAKE_ORGN, AG_ECO9, LAT_DD83, LON_DD83) #UID was showing many NA(i.e. empty cells) so we use the UNIQUE_ID here instead. Joining using SITE_D alone. 


# Select and calculation on phytoplankton data (note: CUSPIDOTHRIX-ANA &SAX)
##################################################
#First -CHECK DATES
str(phytoplanktoncount2017_data$DATE_COL)
# Convert "5/30/2017" format to Date
phytoplanktoncount2017_data$DATE_COL <- mdy(phytoplanktoncount2017_data$DATE_COL)
# Check the conversion
head(phytoplanktoncount2017_data$DATE_COL)
str(phytoplanktoncount2017_data$DATE_COL)
###################################################

# We define PTOX taxa based on Chapman & Foss (2020); Chorus & Welker (2021).
ptox_taxa <- c("ANABAENOPSIS", "ANABAENA", "APHANIZOMENON", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM", "CUSPIDOTHRIX",
               "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DESMONOSTOC",  "DOLICHOSPERMUM", "FISCHERELLA", "GEITLERINEMA", 
               "GLOEOTRICHIA", "HAPALOSIPHON", "LEPTOLYNGBYA", "PLECTONEMA", "LIMNOTHRIX", "MERISMOPEDIA", "MICROCOLEUS",
               "PHORMIDIUM", "MICROCYSTIS", "MICROSEIRA", "LYNGBYA", "NOSTOC", "OSCILLATORIA", "PLANKTOTHRIX", "PSEUDANABAENA",
               "RADIOCYSTIS", "RIVULARIA", "ROMERIA", "SCYTONEMA", "SNOWELLA", "SPHAEROSPERMOPSIS", "STENOMITOS", "SYNECHOCOCCUS",
               "SYNECHOCYSTIS", "TOLYPOTHRIX", "TRICHODESMIUM", "TRICHORMUS", "UMEZAKIA", "WORONICHINIA")

ptox_mic_taxa <- c("ANABAENOPSIS", "ANABAENA", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM","DESMONOSTOC",  "DOLICHOSPERMUM", "FISCHERELLA", "GEITLERINEMA", 
                   "GLOEOTRICHIA", "HAPALOSIPHON", "LEPTOLYNGBYA", "LIMNOTHRIX", "MERISMOPEDIA", "MICROCOLEUS","PHORMIDIUM", "MICROCYSTIS", "NOSTOC", "OSCILLATORIA", 
                   "PLANKTOTHRIX", "PSEUDANABAENA", "RADIOCYSTIS", "RIVULARIA", "ROMERIA", "SCYTONEMA", "SNOWELLA", "SPHAEROSPERMOPSIS", "STENOMITOS", "SYNECHOCOCCUS",
                   "SYNECHOCYSTIS", "TOLYPOTHRIX", "TRICHODESMIUM", "TRICHORMUS", "WORONICHINIA")

ptox_CYL_taxa <- c("ANABAENA", "APHANIZOMENON","CHRYSOSPORUM",
                   "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DOLICHOSPERMUM",
                   "MICROSEIRA", "LYNGBYA", "OSCILLATORIA","SPHAEROSPERMOPSIS","UMEZAKIA")


#

names(phytoplanktoncount2017_data)
phyto2017_summary <- phytoplanktoncount2017_data %>%
  group_by(SITE_ID, DATE_COL) %>%
  summarise(
    # Total biovolume calculations
    total_phytoplankton_biovolume = sum(BIOVOLUME, na.rm = TRUE),
    total_cyanobacteria_biovolume = sum(BIOVOLUME[ALGAL_GROUP == "BLUE-GREEN ALGAE"], na.rm = TRUE), #2017 DATA USED BLUE-GREEN ALGAE instead of CYANOBACTERIA
    
    # Total density calculations
    total_phytoplankton_density = sum(DENSITY, na.rm = TRUE),
    total_cyanobacteria_density = sum(DENSITY[ALGAL_GROUP == "BLUE-GREEN ALGAE"], na.rm = TRUE),
    
    # Total abundance calculations
    total_phytoplankton_abundance = sum(ABUNDANCE, na.rm = TRUE),
    total_cyanobacteria_abundance = sum(ABUNDANCE[ALGAL_GROUP == "BLUE-GREEN ALGAE"], na.rm = TRUE),
    
    # PTOX Biovolume Calculation: Which is the Sum of biovolume where "TARGET_TAXON" matches "PTOX taxa"
    ##To ensure that any species containing the name "Anabaena" for example (including "Anabaena oscillarioides" etc) is captured, we modify the code to use pattern matching with grepl()
    ##grepl(pattern, TARGET_TAXON, ignore.case = TRUE)
    ##Checks if each TARGET_TAXON contains any word from ptox_taxa.
    ##Example: "Anabaena oscillarioides" matches "Anabaena".
    ##BIOVOLUME[grepl(...)]
    ##Filters BIOVOLUME only where the taxon contains a PTOX keyword.
    PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE),  #selects only the BIOVOLUME values where TARGET_TAXON matches a taxon in ptox_taxa.
    MIC_PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_mic_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE), 
    CYL_PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_CYL_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE) 
  ) %>%
  mutate(
    percent_cyanobacteria_biovolume = (total_cyanobacteria_biovolume / total_phytoplankton_biovolume) * 100,
    percent_cyanobacteria_density = (total_cyanobacteria_density / total_phytoplankton_density) * 100,
    percent_cyanobacteria_abundance = (total_cyanobacteria_abundance / total_phytoplankton_abundance) * 100,
    percent_PTOX_biovolume = (PTOX_biovolume / total_cyanobacteria_biovolume) * 100,  # % PTOX biovolume relative to total_cyanobacteria_biovolume
    percent_MIC_PTOX_biovolume = (MIC_PTOX_biovolume / total_cyanobacteria_biovolume) * 100,
    percent_CYL_PTOX_biovolume = (CYL_PTOX_biovolume / total_cyanobacteria_biovolume) * 100
  )

# View
print(phyto2017_summary)




###TO DEAL WITH BDL (non detect-ND),we use half detection limit#######
##Toxin MDL MICX = 0.1 ug/L; CYLSPER = 0.05 ug/L
#2017
toxin2017_DL <- toxin2017_subset %>% 
  mutate(MICX = ifelse(is.na(MICX), 0.1 / 2, MICX),
         CYLSPER = ifelse(is.na(CYLSPER), 0.05 / 2, CYLSPER)
  )
head(toxin2017_DL) #CHECK HEAD
colSums(is.na(toxin2017_DL)) #cHECK NA count again


###JOIN SEBSET DATA#####
##To combine the datasets####
#2017
combined_data1_NLA2017 <- left_join(WaterChem2017_subset, toxin2017_DL, #much better after dates are in the same format
                                    by = c("UID", "SITE_ID", "DATE_COL"))

#join secchi
combined_data2_NLA2017 <- left_join(combined_data1_NLA2017, secchi2017_sebset,
                                    by = c("UID", "SITE_ID", "DATE_COL"))

#Join mean_profiles_top1m
combined_data3_NLA2017 <- left_join(combined_data2_NLA2017, mean_profiles2017_top1m,       #Note: Consider joining the "mean_profiles_alldepths" later
                                    by = c("UID", "SITE_ID", "DATE_COL"))




#join site information

combined_data4b_NLA2017 <- left_join(combined_data3_NLA2017, siteinfo2017_subset, #Joining using SITE_D alone. 
                                     by = c("SITE_ID"),
                                     relationship = "many-to-many") #note

#view the newly combined dataset columns
View(combined_data4b_NLA2017)

#Join phyto data

combined_data5_NLA2017 <- left_join(combined_data4b_NLA2017, phyto2017_summary, 
                                    by = c("SITE_ID", "DATE_COL"),
                                    relationship = "many-to-many") #note
#Check names
names(combined_data5_NLA2017)
#view the newly combined dataset columns
View(combined_data5_NLA2017)
###############################################################Add #TN-TP
#######################
#TN-TP
# TN:TP ratio by weight

# Convert NTL from mg/L to µg/L
combined_data5_NLA2017b <- combined_data5_NLA2017 %>%
  mutate(
    NTL_ugL = NTL * 1000,  # Convert NTL to µg/L
    TN_TP_RATIO = NTL_ugL / PTL  # Compute TN:TP ratio
  )

#check names again
names(combined_data5_NLA2017)
names(combined_data5_NLA2017b) ##NTL_ugL and TN-TP new coulmns

##############################################################################
################################################Diversity indices 
# Ensure the data structure is correct
str(phytoplanktoncount2017_data)

# Aggregate abundance by taxon and sample ("SITE_ID")
phyto_wide <- phytoplanktoncount2017_data %>%
  group_by(SITE_ID, TARGET_TAXON) %>%
  summarise(Abundance = sum(ABUNDANCE, na.rm = TRUE)) %>%
  pivot_wider(names_from = TARGET_TAXON, values_from = Abundance, values_fill = 0) %>%
  ungroup()

# Convert to matrix format for diversity calculations
phyto_matrix <- as.matrix(phyto_wide[,-1])  # Remove Sample_ID column

# Shannon Diversity Index
shannon_index <- diversity(phyto_matrix, index = "shannon")

# Simpson Diversity Index
simpson_index <- diversity(phyto_matrix, index = "simpson")

# Evenness (Shannon Index / log(S), where S = number of species)
evenness <- shannon_index / log(specnumber(phyto_matrix))

# Create results dataframe
diversity_results <- data.frame(
  SITE_ID = phyto_wide$SITE_ID,
  Shannon_Index = shannon_index,
  Simpson_Index = simpson_index,
  Evenness = evenness
)

# Print results
print(diversity_results)

########################################################Join phyto data with diversity_results
#Join phyto data with diversity_results

combined_data7A_2017 <- left_join(combined_data5_NLA2017b, diversity_results, 
                                  by = c("SITE_ID"),
                                  relationship = "many-to-many") #note
names(combined_data7A_2017)
#####################################################################################Nitrate/nitrite Mg/l combined
#combine/sum the three columns NITRATE_N, NITRATE_NITRITE_N, and NITRITE_N into a new column called NITRATE_NITRITE

combined_data7A_2017$NITRATE_NITRITE <- rowSums(
  combined_data7A_2017[, c("NITRATE_N", "NITRATE_NITRITE_N", "NITRITE_N")],
  na.rm = TRUE
)
names(combined_data7A_2017)

############################################################################################
################################################################################
#Group by Taxonomy (Genus)


names(phytoplanktoncount2017_data)
# Group by genus (first part of the species name) and sum the biovolume
phytoplankton_grouped <- phytoplanktoncount2017_data %>%
  mutate(
    Genus = word(TARGET_TAXON, 1)  # Extract the first word as genus
  ) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO, Genus) %>%  #use UID instead of UNIQUE_ID
  summarize(Total_Biovolume = sum(BIOVOLUME, na.rm = TRUE), .groups = "drop")

# Check the structure
str(phytoplankton_grouped)

#MAKE WIDER
phytoplankton_wide_GROUP <- phytoplankton_grouped %>%
  pivot_wider(
    id_cols = c(UID, SITE_ID, DATE_COL, VISIT_NO),
    names_from = Genus,
    values_from = Total_Biovolume,
    values_fill = 0
  )

names(phytoplankton_wide_GROUP)
##selected_phytoplankton_wide_GROUP (to join with toxin-with site ID)
selected_phytoplankton_wide_GROUP <- phytoplankton_wide_GROUP %>%                                    #2017 has no ARTHROSPIRA, CHRYSOSPORUM, GEITLERINEMA, SPHAEROSPERMOPSIS,SYNECHOCYSTIS,
  dplyr::select(SITE_ID, ANABAENOPSIS, ANABAENA, APHANIZOMENON, APHANOCAPSA, CUSPIDOTHRIX, #rEMOVE SITE ID TO RUN CORRELATION TEST
                RAPHIDIOPSIS, CYLINDROSPERMOPSIS, DOLICHOSPERMUM,   #NO DESMONOSTOC, FISCHERELLA, HAPALOSIPHON, PLECTONEMA, MICROCOLEUS Genus
                GLOEOTRICHIA, LEPTOLYNGBYA, LIMNOTHRIX, MERISMOPEDIA, 
                PHORMIDIUM, MICROCYSTIS, LYNGBYA, NOSTOC, OSCILLATORIA, PLANKTOTHRIX, PSEUDANABAENA, #No MICROSEIRA, RIVULARIA, SCYTONEMA, STENOMITOS, TOLYPOTHRIX, TRICHODESMIUM,
                RADIOCYSTIS, ROMERIA, SNOWELLA, SYNECHOCOCCUS,           #No TRICHORMUS, UMEZAKIA,
                WORONICHINIA)    


#################################
#create new combined data to select lat and long

names(combined_data7A_2017)

selected_data_combine2017_NEW <- dplyr::select(
  combined_data7A_2017, SITE_ID, UID, LAT_DD83, LON_DD83, MICX, CYLSPER, CHLA, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, PTOX_biovolume, percent_cyanobacteria_biovolume,
  percent_PTOX_biovolume, MIC_PTOX_biovolume, CYL_PTOX_biovolume, percent_MIC_PTOX_biovolume, percent_CYL_PTOX_biovolume, Shannon_Index, Simpson_Index, Evenness, Temp_top1m, Secchi, 
  pH_top1m, PH, ELEVATION, INDEX_SITE_DEPTH, AMMONIA_N, ANC, CALCIUM, CHLORIDE, COLOR, COND, DOC, MAGNESIUM, NTL, PTL,
  SODIUM, TURB, SULFATE, POTASSIUM, AREA_HA, LAKE_ORGN, AG_ECO9, NITRATE_NITRITE, TN_TP_RATIO
)

###############################################################################################################
# Merge datasets by UNIQUE_ID
combined_data_pca_2017_NEW <- selected_data_combine2017_NEW %>%
  left_join(selected_phytoplankton_wide_GROUP, by = "SITE_ID", relationship = "many-to-many")
#################################################################################################################
# Create a new salinity column as the sum of selected ion concentrations
combined_data_pca_2017_NEW$salinity <- rowSums(combined_data_pca_2017_NEW[, c("CALCIUM", 
                                                                              "MAGNESIUM", 
                                                                              "SODIUM", 
                                                                              "POTASSIUM", 
                                                                              "CHLORIDE",
                                                                              "SULFATE")], 
                                               na.rm = TRUE)

names(combined_data_pca_2017_NEW)
##############################################################
########################################################################################################### CREATE TROPHIC CLASSIFICATION

combined_data_pca_2017_NEW <- combined_data_pca_2017_NEW %>%
  filter(!is.na(CHLA)) %>%
  mutate(
    trophic_status = case_when(
      CHLA <= 2 ~ "Oligotrophic",
      CHLA > 2 & CHLA <= 7 ~ "Mesotrophic",
      CHLA > 7 & CHLA <= 30 ~ "Eutrophic",
      CHLA > 30 ~ "Hypereutrophic",
      TRUE ~ NA_character_
    )
  )

# Ensure the trophic_status is a factor with the correct order
combined_data_pca_2017_NEW <- combined_data_pca_2017_NEW %>%
  mutate(trophic_status = factor(trophic_status,
                                 levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic")))

names(combined_data_pca_2017_NEW)

#######################################################JOIN combined_data_pca_2017_NEW WITH Thermocline Depth and ZMAX data
###################################################first: Calculate Thermocline Depth per Site


# Ensure numeric types
profile2017 <- profile2017 %>%
  mutate(DEPTH = as.numeric(DEPTH),
         TEMPERATURE = as.numeric(TEMPERATURE)) %>%
  filter(!is.na(DEPTH) & !is.na(TEMPERATURE))  # Remove rows with missing values

# Function to calculate thermocline depth
get_thermocline <- function(depth, temp) {
  if (length(depth) < 2) return(NA)
  ord <- order(depth)
  d <- depth[ord]
  t <- temp[ord]
  d_diff <- diff(d)
  t_diff <- diff(t)
  gradients <- abs(t_diff / d_diff)
  # Thermocline is where gradient is maximum
  if (length(gradients) == 0) return(NA)
  return(d[which.max(gradients)])
}

# Calculate thermocline per site
thermocline_per_site_2017 <- profile2017 %>%
  group_by(SITE_ID) %>%
  summarise(thermocline_depth = get_thermocline(DEPTH, TEMPERATURE),
            zmax = max(DEPTH, na.rm = TRUE)) %>%
  ungroup()

# View results
head(thermocline_per_site_2017)


############################################################## verify Thermocline Depth WITH rLakeAnalyzer
# Install rLakeAnalyzer if not already installed
#install.packages("rLakeAnalyzer")  # if not on CRAN, install via GitHub: devtools::install_github("UW-FHL/rLakeAnalyzer")
#Note that in very shallow lakes thermocline may return NA.

# Clean and filter data
profile_clean_2017 <- profile2017 %>%
  filter(!is.na(DEPTH), !is.na(TEMPERATURE)) %>%
  mutate(DEPTH = as.numeric(DEPTH),
         TEMPERATURE = as.numeric(TEMPERATURE))

# Function to calculate thermocline depth per site using rLakeAnalyzer
get_thermocline_rLake <- function(df) {
  df <- df[order(df$DEPTH), ]  # ensure increasing depth
  tryCatch({
    thermo.depth(wtr = df$TEMPERATURE, depths = df$DEPTH)
  }, error = function(e) NA)
}

# Apply per SITE_ID
#thermocline_rLake <- profile_clean_2017 %>%
#  group_by(SITE_ID) %>%
#  summarise(thermocline_depth = get_thermocline_rLake(cur_data_all()),
#            zmax = max(DEPTH, na.rm = TRUE)) %>%
#  ungroup()

#REMOVE ZMAX and remane thermocline_depth as thermocline_depth_rLake
thermocline_rLake2017 <- profile_clean_2017 %>%
  group_by(SITE_ID) %>%
  summarise(thermocline_depth_rLake = get_thermocline_rLake(cur_data_all())) %>%
  ungroup()

# View result
head(thermocline_rLake2017)

###############################################################COMBINE THE TWO THERMO CALCULATION

# Merge datasets 
thermocline_depth_data_2017 <- thermocline_per_site_2017 %>%
  left_join(thermocline_rLake2017 , by = "SITE_ID")


#######################################################JOIN combined_data_pca_2017_NEW WITH Thermocline Depth and ZMAX data
########################### Merge datasets 
combined_data_pca_2017_NEW <- combined_data_pca_2017_NEW %>%
  left_join(thermocline_depth_data_2017, by = "SITE_ID")

names(combined_data_pca_2017_NEW)

##############################
#cHECK RELATIONSHIPS BETWEN INDEX SITE DEPTH AND THE NEW ZMAX CALCULATED

Zmax_compare2017 <- combined_data_pca_2017_NEW %>%
  ggplot(aes(x = INDEX_SITE_DEPTH, y = zmax)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman",
           label.x = 1, label.y = 2,
           size = 9,   # adjust stats text size
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw(base_size = 16) +  # sets overall scaling baseline
  labs(title = "(B) NLA data 2017") +
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  )
Zmax_compare2017 


#RHO=0.99 WITH P LESS THAN 0.0001

###COMBINED 2022 & 2017 PLOT FOR ZMAX VS INDEX SITE DEPTH

# Combine all plots
combined_ZMAX <- Zmax_compare2022 + Zmax_compare2017 + plot_layout(nrow = 1)

# Display
print(combined_ZMAX)


###########################################################################CHECK 2017 final selected data BELOW
names(combined_data_pca_2017_NEW)

#cmpare with 2022
names(combined_data_pca_NEW)




#####################################################################################2012####repeat processes done in 2022 and 2017 for 2012
#2012 DATA 
#Load in rawdata from Github ##remember to use that raw link (this appears to create a one time token, that have to be repeated everytime)######

##NLA12_waterchem data
WaterChem2012 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2012_waterchem_wide.csv")

##NLA12_CHLA data
CHLA2012 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2012_chla_wide.csv")

##NLA12_Toxin data (MICROCYSTINS ONLY)
toxin2012 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2012_algaltoxins_08192016.csv")

##NLA12_Secchi data
secchi2012 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2012_secchi_08232016.csv")

##NLA12_profile data
profile2012 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2012_profile_wide.csv")

##NLA12_siteinfo data
siteinfo2012 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2012_wide_siteinfo_08232016.csv")

#NLA12_Phytoplankton data
phytoplanktoncount2012_data <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla2012_wide_phytoplankton_count_02122014.csv")  


#############################################################

#pIVOT WHERE NECESSARY##################

#waterchem data in 2012 is already in a wide format but..check
#waterchem data in 2012 has no DATE_COL hence no need to format dates
view(WaterChem2012)
#CHL-A
view(CHLA2012)


#Toxin2012 already wide
names(toxin2012)
#CHECK DATES
str(toxin2012$DATE_COL)
# Convert "5/30/2012" format to Date
toxin2012$DATE_COL <- mdy(toxin2012$DATE_COL)
# Check the conversion
head(toxin2012$DATE_COL)
str(toxin2012$DATE_COL)
###################################################


#SECCHI 2012 ALREADY CALCULAATED
names(secchi2012)
#CHECK DATES
str(secchi2012$DATE_COL)
# Convert "5/30/2012" format to Date
secchi2012$DATE_COL <- mdy(secchi2012$DATE_COL)
# Check the conversion
head(secchi2012$DATE_COL)
str(secchi2012$DATE_COL)


##################################################


#Select specific columns i.e. relevant columns for my work####
#WaterChem
#2012

names(WaterChem2012)
WaterChem2012_subset <- WaterChem2012 %>%         #Note the waterchem data here has no Site_ID and Date_COL
  dplyr::select(UID, AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, #NO CHL-A here
                CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NITRATE_N_RESULT, 
                NITRATE_NITRITE_N_RESULT, NITRITE_N_RESULT, NTL_RESULT, PH_RESULT, POTASSIUM_RESULT,
                PTL_RESULT, SODIUM_RESULT, SULFATE_RESULT, TURB_RESULT)

#Select CHL-A
#2012

names(CHLA2012)
CHLA2012_subset <- CHLA2012 %>% 
  dplyr::select(UID, CHLX_RESULT)


#Select Toxin
#2012
names(toxin2012)
toxin2012_subset <- toxin2012 %>% 
  dplyr::select(UID, SITE_ID, VISIT_NO, DATE_COL, MICX_RESULT)


# Select SECCHI
#2012
names(secchi2012)  
secchi2012_sebset <- secchi2012 %>% 
  dplyr::select(UID, SITE_ID, DATE_COL, VISIT_NO, SECCHI)


#DATE FOR PROFILE DATA
view(profile2012)
##################################################
#CHECK DATES
str(profile2012$DATE_COL)
# Convert "5/30/2012" format to Date
profile2012$DATE_COL <- mdy(profile2012$DATE_COL)
# Check the conversion
head(profile2012$DATE_COL)
str(profile2012$DATE_COL)
###################################################

# select profile
names(profile2012)
#1-depth-averaged values across all depths
mean_profiles2012_alldepths <- profile2012 %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_mean = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_mean = mean(OXYGEN, na.rm = TRUE),
    pH_mean = mean(PH, na.rm = TRUE),
    Conductivity_mean = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#2-calculate mean of temp, DO, pH, conductivity at the top 1m
mean_profiles2012_top1m <- profile2012 %>%
  filter(DEPTH <= 1) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_top1m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_top1m = mean(OXYGEN, na.rm = TRUE),
    pH_top1m = mean(PH, na.rm = TRUE),
    Conductivity_top1m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#3-compute the mean between 1 and 2 meters
mean_profiles2012_1to2m <- profile2012 %>%
  filter(DEPTH >= 1, DEPTH <= 2) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_1to2m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_1to2m = mean(OXYGEN, na.rm = TRUE),
    pH_1to2m = mean(PH, na.rm = TRUE),
    Conductivity_1to2m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#4- compute the mean between 2 and 4 meters
mean_profiles2012_2to4m <- profile2012 %>%
  filter(DEPTH >= 2, DEPTH <= 4) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_1to2m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_1to2m = mean(OXYGEN, na.rm = TRUE),
    pH_1to2m = mean(PH, na.rm = TRUE),
    Conductivity_1to2m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#5- compute the mean below 4 meters
mean_profiles2012_below4m <- profile2012 %>%
  filter(DEPTH >= 4) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_below4m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_below4m = mean(OXYGEN, na.rm = TRUE),
    pH_below4m = mean(PH, na.rm = TRUE),
    Conductivity_below4m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )

#6- compute the mean below 5 meters
mean_profiles2012_below5m <- profile2012 %>%
  filter(DEPTH >= 5) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO) %>%
  summarize(
    Temp_below5m = mean(TEMPERATURE, na.rm = TRUE),
    Oxygen_below5m = mean(OXYGEN, na.rm = TRUE),
    pH_below5m = mean(PH, na.rm = TRUE),
    Conductivity_below5m = mean(CONDUCTIVITY, na.rm = TRUE),
    .groups = "drop"
  )



# check date for siteinfo data
######################
#CHECK DATES
str(siteinfo2012$DATE_COL)
# Convert "5/30/2012" format to Date
siteinfo2012$DATE_COL <- mdy(siteinfo2012$DATE_COL)
# Check the conversion
head(siteinfo2012$DATE_COL)
str(siteinfo2012$DATE_COL)
###################################################

# select siteinfo
#2012 -(INDEX_SITE_DEPTH not found in site information data)
names(siteinfo2012) 
siteinfo2012_subset <- siteinfo2012 %>% 
  dplyr::select(UID, SITE_ID, AREA_HA, ELEVATION,LAKE_ORIGIN, LAT_DD83, LON_DD83, AGGR_ECO9_2015) 





# Select and calculation on phytoplankton data (note: CUSPIDOTHRIX-ANA &SAX)
##################################################
#First -CHECK DATES
str(phytoplanktoncount2012_data$DATE_COL)
# Convert "5/30/2012" format to Date
phytoplanktoncount2012_data$DATE_COL <- mdy(phytoplanktoncount2012_data$DATE_COL)
# Check the conversion
head(phytoplanktoncount2012_data$DATE_COL)
str(phytoplanktoncount2012_data$DATE_COL)
###################################################

# We define PTOX taxa based on Chapman & Foss (2020); Chorus & Welker (2021).
ptox_taxa <- c("ANABAENOPSIS", "ANABAENA", "APHANIZOMENON", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM", "CUSPIDOTHRIX",
               "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DESMONOSTOC",  "DOLICHOSPERMUM", "FISCHERELLA", "GEITLERINEMA", 
               "GLOEOTRICHIA", "HAPALOSIPHON", "LEPTOLYNGBYA", "PLECTONEMA", "LIMNOTHRIX", "MERISMOPEDIA", "MICROCOLEUS",
               "PHORMIDIUM", "MICROCYSTIS", "MICROSEIRA", "LYNGBYA", "NOSTOC", "OSCILLATORIA", "PLANKTOTHRIX", "PSEUDANABAENA",
               "RADIOCYSTIS", "RIVULARIA", "ROMERIA", "SCYTONEMA", "SNOWELLA", "SPHAEROSPERMOPSIS", "STENOMITOS", "SYNECHOCOCCUS",
               "SYNECHOCYSTIS", "TOLYPOTHRIX", "TRICHODESMIUM", "TRICHORMUS", "UMEZAKIA", "WORONICHINIA")

ptox_mic_taxa <- c("ANABAENOPSIS", "ANABAENA", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM","DESMONOSTOC",  "DOLICHOSPERMUM", "FISCHERELLA", "GEITLERINEMA", 
                   "GLOEOTRICHIA", "HAPALOSIPHON", "LEPTOLYNGBYA", "LIMNOTHRIX", "MERISMOPEDIA", "MICROCOLEUS","PHORMIDIUM", "MICROCYSTIS", "NOSTOC", "OSCILLATORIA", 
                   "PLANKTOTHRIX", "PSEUDANABAENA", "RADIOCYSTIS", "RIVULARIA", "ROMERIA", "SCYTONEMA", "SNOWELLA", "SPHAEROSPERMOPSIS", "STENOMITOS", "SYNECHOCOCCUS",
                   "SYNECHOCYSTIS", "TOLYPOTHRIX", "TRICHODESMIUM", "TRICHORMUS", "WORONICHINIA")

ptox_CYL_taxa <- c("ANABAENA", "APHANIZOMENON","CHRYSOSPORUM",
                   "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DOLICHOSPERMUM",
                   "MICROSEIRA", "OSCILLATORIA","SPHAEROSPERMOPSIS","UMEZAKIA")


#


names(phytoplanktoncount2012_data)
phyto2012_summary <- phytoplanktoncount2012_data %>%
  group_by(SITE_ID, DATE_COL) %>%
  summarise(
    # Total biovolume calculations
    total_phytoplankton_biovolume = sum(BIOVOLUME, na.rm = TRUE),
    total_cyanobacteria_biovolume = sum(BIOVOLUME[ALGAL_GROUP == "BLUE-GREEN ALGAE"], na.rm = TRUE), #2012 DATA USED BLUE-GREEN ALGAE instead of CYANOBACTERIA
    
    # Total density calculations
    total_phytoplankton_density = sum(DENSITY, na.rm = TRUE),
    total_cyanobacteria_density = sum(DENSITY[ALGAL_GROUP == "BLUE-GREEN ALGAE"], na.rm = TRUE),
    
    # Total abundance calculations
    total_phytoplankton_abundance = sum(ABUNDANCE, na.rm = TRUE),
    total_cyanobacteria_abundance = sum(ABUNDANCE[ALGAL_GROUP == "BLUE-GREEN ALGAE"], na.rm = TRUE),
    
    # PTOX Biovolume Calculation: Which is the Sum of biovolume where "TARGET_TAXON" matches "PTOX taxa"
    ##To ensure that any species containing the name "Anabaena" for example (including "Anabaena oscillarioides" etc) is captured, we modify the code to use pattern matching with grepl()
    ##grepl(pattern, TARGET_TAXON, ignore.case = TRUE)
    ##Checks if each TARGET_TAXON contains any word from ptox_taxa.
    ##Example: "Anabaena oscillarioides" matches "Anabaena".
    ##BIOVOLUME[grepl(...)]
    ##Filters BIOVOLUME only where the taxon contains a PTOX keyword.
    PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE),  #selects only the BIOVOLUME values where TARGET_TAXON matches a taxon in ptox_taxa.
    MIC_PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_mic_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE), 
    CYL_PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_CYL_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE) 
  ) %>%
  mutate(
    percent_cyanobacteria_biovolume = (total_cyanobacteria_biovolume / total_phytoplankton_biovolume) * 100,
    percent_cyanobacteria_density = (total_cyanobacteria_density / total_phytoplankton_density) * 100,
    percent_cyanobacteria_abundance = (total_cyanobacteria_abundance / total_phytoplankton_abundance) * 100,
    percent_PTOX_biovolume = (PTOX_biovolume / total_cyanobacteria_biovolume) * 100,  # % PTOX biovolume relative to total_cyanobacteria_biovolume
    percent_MIC_PTOX_biovolume = (MIC_PTOX_biovolume / total_cyanobacteria_biovolume) * 100,
    percent_CYL_PTOX_biovolume = (CYL_PTOX_biovolume / total_cyanobacteria_biovolume) * 100
  )

# View
print(phyto2012_summary)


###TO DEAL WITH BDL (non detect-ND),we use half detection limit#######
##Toxin MDL MICX = 0.1 ug/L; CYLSPER = 0.05 ug/L
#2012
names(toxin2012_subset)
# Adjust the code to handle both NA and values < 0.1
toxin2012_DL <- toxin2012_subset %>% 
  mutate(MICX_RESULT = ifelse(is.na(MICX_RESULT) | MICX_RESULT < 0.1, 0.1 / 2, MICX_RESULT))

head(toxin2012_DL) #CHECK HEAD
colSums(is.na(toxin2012_DL)) #cHECK NA count again




###JOIN SEBSET DATA#####
##To combine the datasets####
#2012
#Combine waterchem data with CHLA Data
combined_data1_NLA2012 <- left_join(WaterChem2012_subset, CHLA2012_subset, 
                                    by = c("UID"))


#JOIN TOXINS DATA
combined_data2_NLA2012 <- left_join(combined_data1_NLA2012, toxin2012_DL, 
                                    by = c("UID"))


#join secchi
combined_data3_NLA2012 <- left_join(combined_data2_NLA2012, secchi2012_sebset,
                                    by = c("UID", "SITE_ID", "DATE_COL"))



#Join mean_profiles_top1m
combined_data4_NLA2012 <- left_join(combined_data3_NLA2012, mean_profiles2012_top1m,       #Note: Consider joining the "mean_profiles_alldepths" later
                                    by = c("UID"))

#join site information

combined_data5_NLA2012 <- left_join(combined_data4_NLA2012, siteinfo2012_subset, 
                                    by = c("UID"),
                                    relationship = "many-to-many") #note

#view the newly combined dataset columns-change print to view
print(combined_data5_NLA2012)

#Join phyto data

combined_data6_NLA2012 <- left_join(combined_data5_NLA2012, phyto2012_summary, 
                                    by = c("SITE_ID"),
                                    relationship = "many-to-many") #note

view(combined_data6_NLA2012)
#Check names
names(combined_data6_NLA2012)
#view the newly combined dataset columns-change print to view
print(combined_data6_NLA2012)

###############################################################Add #TN-TP
#######################
#TN-TP
# TN:TP ratio by weight

# Convert NTL from mg/L to µg/L
combined_data6_NLA2012b <- combined_data6_NLA2012 %>%
  mutate(
    NTL_ugL = NTL_RESULT * 1000,  # Convert NTL to µg/L
    TN_TP_RATIO = NTL_ugL / PTL_RESULT  # Compute TN:TP ratio
  )

#check names again
names(combined_data6_NLA2012)
names(combined_data6_NLA2012b) ##NTL_ugL and TN-TP new coulmns

##############################################################################
################################################Diversity indices 
# Ensure the data structure is correct
str(phytoplanktoncount2012_data)

# Aggregate abundance by taxon and sample ("SITE_ID")
phyto_wide <- phytoplanktoncount2012_data %>%
  group_by(SITE_ID, TARGET_TAXON) %>%
  summarise(Abundance = sum(ABUNDANCE, na.rm = TRUE)) %>%
  pivot_wider(names_from = TARGET_TAXON, values_from = Abundance, values_fill = 0) %>%
  ungroup()

# Convert to matrix format for diversity calculations
phyto_matrix <- as.matrix(phyto_wide[,-1])  # Remove Sample_ID column

# Shannon Diversity Index
shannon_index <- diversity(phyto_matrix, index = "shannon")

# Simpson Diversity Index
simpson_index <- diversity(phyto_matrix, index = "simpson")

# Evenness (Shannon Index / log(S), where S = number of species)
evenness <- shannon_index / log(specnumber(phyto_matrix))

# Create results dataframe
diversity_results <- data.frame(
  SITE_ID = phyto_wide$SITE_ID,
  Shannon_Index = shannon_index,
  Simpson_Index = simpson_index,
  Evenness = evenness
)

# Print results
print(diversity_results)

########################################################Join phyto data with diversity_results
#Join phyto data with diversity_results

combined_data7A_2012 <- left_join(combined_data6_NLA2012b, diversity_results, 
                                  by = c("SITE_ID"),
                                  relationship = "many-to-many") #note
names(combined_data7A_2012)
#####################################################################################Nitrate/nitrite Mg/l combined
#combine/sum the three columns NITRATE_N, NITRATE_NITRITE_N, and NITRITE_N into a new column called NITRATE_NITRITE

combined_data7A_2012$NITRATE_NITRITE <- rowSums(
  combined_data7A_2012[, c("NITRATE_N_RESULT", "NITRATE_NITRITE_N_RESULT", "NITRITE_N_RESULT")],
  na.rm = TRUE
)
names(combined_data7A_2012)

#####################################################################################
################################################################################
#Group by Taxonomy (Genus)

names(phytoplanktoncount2012_data)
# Group by genus (first part of the species name) and sum the biovolume
phytoplankton_grouped <- phytoplanktoncount2012_data %>%
  mutate(
    Genus = word(TARGET_TAXON, 1)  # Extract the first word as genus
  ) %>%
  group_by(UID, SITE_ID, DATE_COL, VISIT_NO, Genus) %>%
  summarize(Total_Biovolume = sum(BIOVOLUME, na.rm = TRUE), .groups = "drop")

# Check the structure
str(phytoplankton_grouped)

#MAKE WIDER
phytoplankton_wide_GROUP <- phytoplankton_grouped %>%
  pivot_wider(
    id_cols = c(UID, SITE_ID, DATE_COL, VISIT_NO),
    names_from = Genus,
    values_from = Total_Biovolume,
    values_fill = 0
  )

names(phytoplankton_wide_GROUP)

#select UID instead of SITE ID (To use for 2012 PCA joining)
##selected_phytoplankton_wide_GROUP
selected_phytoplankton_wide_GROUPU <- phytoplankton_wide_GROUP %>%                        #2012: No CHRYSOSPORUM, CUSPIDOTHRIX,NOSTOC,SPHAEROSPERMOPSIS,
  dplyr::select(UID, ANABAENOPSIS, ANABAENA, APHANIZOMENON, APHANOCAPSA, ARTHROSPIRA,  #rEMOVE SITE ID TO RUN CORRELATION TEST
                RAPHIDIOPSIS, CYLINDROSPERMOPSIS, DOLICHOSPERMUM, GEITLERINEMA,       #NO  Genus DESMONOSTOC,FISCHERELLA,HAPALOSIPHON,PLECTONEMA,MICROCOLEUS, 
                GLOEOTRICHIA, LEPTOLYNGBYA, LIMNOTHRIX, MERISMOPEDIA, 
                PHORMIDIUM, MICROCYSTIS, LYNGBYA,  OSCILLATORIA, PLANKTOTHRIX, PSEUDANABAENA, #No  MICROSEIRA,RIVULARIA, SCYTONEMA, STENOMITOS, TOLYPOTHRIX, TRICHODESMIUM,
                RADIOCYSTIS, ROMERIA, SNOWELLA,  SYNECHOCOCCUS,           #No TRICHORMUS, UMEZAKIA,
                SYNECHOCYSTIS,  WORONICHINIA)


#################################
#create new combined data to select lat and long

names(combined_data7A_2012)

selected_data_combine2012_NEW <- dplyr::select(
  combined_data7A_2012, UID, LAT_DD83, LON_DD83, MICX_RESULT, CHLX_RESULT, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, PTOX_biovolume, percent_cyanobacteria_biovolume,
  percent_PTOX_biovolume, MIC_PTOX_biovolume, CYL_PTOX_biovolume, percent_MIC_PTOX_biovolume, percent_CYL_PTOX_biovolume, Shannon_Index, Simpson_Index, Evenness, Temp_top1m, SECCHI, pH_top1m, PH_RESULT, ELEVATION,
  AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NTL_RESULT, PTL_RESULT,
  SODIUM_RESULT, TURB_RESULT, SULFATE_RESULT, POTASSIUM_RESULT, NITRATE_NITRITE, TN_TP_RATIO, AREA_HA, LAKE_ORIGIN, AGGR_ECO9_2015,
)

###############################################################################################################
# Merge datasets by UNIQUE_ID
combined_data_pca_2012_NEW <- selected_data_combine2012_NEW %>%
  left_join(selected_phytoplankton_wide_GROUPU, by = "UID", relationship = "many-to-many")
#################################################################################################################
# Create a new salinity column as the sum of selected ion concentrations
combined_data_pca_2012_NEW$salinity <- rowSums(combined_data_pca_2012_NEW[, c("CALCIUM_RESULT", 
                                                                              "MAGNESIUM_RESULT", 
                                                                              "SODIUM_RESULT", 
                                                                              "POTASSIUM_RESULT", 
                                                                              "CHLORIDE_RESULT",
                                                                              "SULFATE_RESULT")], 
                                               na.rm = TRUE)

names(combined_data_pca_2012_NEW)


##############################################################
########################################################################################################### CREATE TROPHIC CLASSIFICATION

combined_data_pca_2012_NEW <- combined_data_pca_2012_NEW %>%
  filter(!is.na(CHLX_RESULT)) %>%
  mutate(
    trophic_status = case_when(
      CHLX_RESULT <= 2 ~ "Oligotrophic",
      CHLX_RESULT > 2 & CHLX_RESULT <= 7 ~ "Mesotrophic",
      CHLX_RESULT > 7 & CHLX_RESULT <= 30 ~ "Eutrophic",
      CHLX_RESULT > 30 ~ "Hypereutrophic",
      TRUE ~ NA_character_
    )
  )

# Ensure the trophic_status is a factor with the correct order
combined_data_pca_2012_NEW <- combined_data_pca_2012_NEW %>%
  mutate(trophic_status = factor(trophic_status,
                                 levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic")))

names(combined_data_pca_2012_NEW)

#######################################################JOIN combined_data_pca_2012_NEW WITH Thermocline Depth and ZMAX data
###################################################Calculate Thermocline Depth per Site


# Ensure numeric types
profile2012 <- profile2012 %>%
  mutate(DEPTH = as.numeric(DEPTH),
         TEMPERATURE = as.numeric(TEMPERATURE)) %>%
  filter(!is.na(DEPTH) & !is.na(TEMPERATURE))  # Remove rows with missing values

# Function to calculate thermocline depth
get_thermocline <- function(depth, temp) {
  if (length(depth) < 2) return(NA)
  ord <- order(depth)
  d <- depth[ord]
  t <- temp[ord]
  d_diff <- diff(d)
  t_diff <- diff(t)
  gradients <- abs(t_diff / d_diff)
  # Thermocline is where gradient is maximum
  if (length(gradients) == 0) return(NA)
  return(d[which.max(gradients)])
}

# Calculate thermocline per site
thermocline_per_site <- profile2012 %>%
  group_by(UID) %>%
  summarise(thermocline_depth = get_thermocline(DEPTH, TEMPERATURE),
            zmax = max(DEPTH, na.rm = TRUE)) %>%
  ungroup()

# View results
head(thermocline_per_site)


############################################################## verify Thermocline Depth WITH rLakeAnalyzer
# Install rLakeAnalyzer if not already installed
#install.packages("rLakeAnalyzer")  # if not on CRAN, install via GitHub: devtools::install_github("UW-FHL/rLakeAnalyzer")
#Note that in very shallow lakes thermocline may return NA.


# Clean and filter data
profile_clean_2012 <- profile2012 %>%
  filter(!is.na(DEPTH), !is.na(TEMPERATURE)) %>%
  mutate(DEPTH = as.numeric(DEPTH),
         TEMPERATURE = as.numeric(TEMPERATURE))

# Function to calculate thermocline depth per site using rLakeAnalyzer
get_thermocline_rLake <- function(df) {
  df <- df[order(df$DEPTH), ]  # ensure increasing depth
  tryCatch({
    thermo.depth(wtr = df$TEMPERATURE, depths = df$DEPTH)
  }, error = function(e) NA)
}

# Apply per SITE_ID
#thermocline_rLake <- profile_clean_2012 %>%
#  group_by(SITE_ID) %>%
#  summarise(thermocline_depth = get_thermocline_rLake(cur_data_all()),
#            zmax = max(DEPTH, na.rm = TRUE)) %>%
#  ungroup()

#REMOVE ZMAX and remane thermocline_depth as thermocline_depth_rLake
thermocline_rLake2012 <- profile_clean_2012 %>%
  group_by(UID) %>%
  summarise(thermocline_depth_rLake = get_thermocline_rLake(cur_data_all())) %>%
  ungroup()

# View result
head(thermocline_rLake2012)


###############################################################COMBINE THE TWO THERMO CALCULATION

# Merge datasets 
thermocline_depth_2012_data <- thermocline_per_site %>%
  left_join(thermocline_rLake2012 , by = "UID")

#######################################################JOIN combined_data_pca_2012_NEW WITH Thermocline Depth and ZMAX data
########################### Merge datasets 
combined_data_pca_2012_NEW <- combined_data_pca_2012_NEW %>%
  left_join(thermocline_depth_2012_data, by = "UID")

names(combined_data_pca_2012_NEW)

##############################
###########################################################################CHECK 2012 selected data BELOW#####

#2012
names(combined_data_pca_2012_NEW)
#2017
names(combined_data_pca_2017_NEW)
#compare with 2022
names(combined_data_pca_NEW)

######################################################################################################################
######################################################################################################################COMBINE all YEARS 
#FROM 2022
names(combined_data_pca_NEW)
dim(combined_data_pca_NEW)
#FROM 2017
names(combined_data_pca_2017_NEW)
dim(combined_data_pca_2017_NEW)
#FROM 2012
names(combined_data_pca_2012_NEW)
dim(combined_data_pca_2012_NEW)


#standardizing
###################harmonizes the column names in each dataset
# Function to rename columns based on mapping
rename_columns <- function(df, mapping) {
  for (i in seq_along(mapping)) {
    old_name <- names(mapping)[i]
    new_name <- mapping[[i]]
    if (old_name %in% names(df)) {
      names(df)[names(df) == old_name] <- new_name
    }
  }
  return(df)
}

# Mapping dictionary: old -> new
name_mapping <- c(
  "AMMONIA_N" = "AMMONIA_N_RESULT",
  "ANC" = "ANC_RESULT",
  "CALCIUM" = "CALCIUM_RESULT",
  "CHLORIDE" = "CHLORIDE_RESULT",
  "COLOR" = "COLOR_RESULT",
  "COND" = "COND_RESULT",
  "DOC" = "DOC_RESULT",
  "MAGNESIUM" = "MAGNESIUM_RESULT",
  "NTL" = "NTL_RESULT",
  "PTL" = "PTL_RESULT",
  "SODIUM" = "SODIUM_RESULT",
  "TURB" = "TURB_RESULT",
  "SULFATE" = "SULFATE_RESULT",
  "POTASSIUM" = "POTASSIUM_RESULT",
  "PH" = "PH_RESULT",
  "CHLA" = "CHLA_RESULT",
  "CHLX_RESULT" = "CHLA_RESULT",  # Rename CHLX_RESULT to CHLA_RESULT
  "MICX_RESULT" = "MICX",
  "Secchi" = "SECCHI", 
  "AGGR_ECO9_2015" = "AG_ECO9",
  "LAKE_ORGN" = "LAKE_ORIGIN",
  "UID" = "UNIQUE_ID"
)

# Apply renaming to datasets
combined_data_pca_2012_NEW <- rename_columns(combined_data_pca_2012_NEW, name_mapping)
combined_data_pca_2017_NEW <- rename_columns(combined_data_pca_2017_NEW, name_mapping)
combined_data_pca_NEW      <- rename_columns(combined_data_pca_NEW, name_mapping)

#CHECK NAMES AGAIN
names(combined_data_pca_NEW)
names(combined_data_pca_2017_NEW)
names(combined_data_pca_2012_NEW)


#Add a Year Column to Each Dataset
#to track which year the data belongs to
combined_data_pca_2012_NEW$Year <- 2012
combined_data_pca_2017_NEW$Year <- 2017
combined_data_pca_NEW$Year      <- 2022  


#
# Get common column names
common_cols <- Reduce(intersect, list(
  names(combined_data_pca_2012_NEW),
  names(combined_data_pca_2017_NEW),
  names(combined_data_pca_NEW)
))


#to find Non-Common Columns Across Datasets
# Get the column names
cols_2022 <- names(combined_data_pca_NEW)
cols_2017 <- names(combined_data_pca_2017_NEW)
cols_2012 <- names(combined_data_pca_2012_NEW)

# All columns
all_columns <- unique(c(cols_2022, cols_2017, cols_2012))

# Create a presence-absence matrix
column_comparison <- data.frame(
  Column = all_columns,
  In_2022 = all_columns %in% cols_2022,
  In_2017 = all_columns %in% cols_2017,
  In_2012 = all_columns %in% cols_2012
)

# View the columns that are not shared by all datasets
non_common_columns <- column_comparison %>%
  filter(!(In_2022 & In_2017 & In_2012))

print(non_common_columns)


# Add CYLSPER, INDEX_SITE_DEPTH: non_common_columns manually 
common_cols_plus <- unique(c(common_cols, "CYLSPER", "INDEX_SITE_DEPTH", "Year"))

# Subset datasets
data_2012_sub <- combined_data_pca_2012_NEW[, intersect(names(combined_data_pca_2012_NEW), common_cols_plus)]
data_2017_sub <- combined_data_pca_2017_NEW[, intersect(names(combined_data_pca_2017_NEW), common_cols_plus)]
data_2022_sub <- combined_data_pca_NEW[, intersect(names(combined_data_pca_NEW), common_cols_plus)]

# Add missing columns in 2012 (since it doesn't have those columns)
if(!"CYLSPER" %in% names(data_2012_sub)) {
  data_2012_sub$CYLSPER <- NA
}
if(!"INDEX_SITE_DEPTH" %in% names(data_2012_sub)) {
  data_2012_sub$INDEX_SITE_DEPTH <- NA
}


# Ensure UNIQUE_ID is character in all datasets
data_2012_sub$UNIQUE_ID <- as.character(data_2012_sub$UNIQUE_ID)
data_2017_sub$UNIQUE_ID <- as.character(data_2017_sub$UNIQUE_ID)
data_2022_sub$UNIQUE_ID <- as.character(data_2022_sub$UNIQUE_ID)

# Combine datasets #Comb. the 3 using dplyr::bind_rows
combined_all_years <- bind_rows(data_2012_sub, data_2017_sub, data_2022_sub)


#Quick Check
# Check dimensions and preview
dim(combined_all_years)
head(combined_all_years)
table(combined_all_years$Year)
names(combined_all_years)
###########################################################################################################################

#######################################################################################CORR PLOT WITH ALL YEAR DATA

str(combined_all_years)
# Select only specific columns (excluding identifiers)
combined_all_years_numeric <- combined_all_years %>%
  dplyr::select(-UNIQUE_ID, -LAT_DD83, -LON_DD83, -Year, -trophic_status, -LAKE_ORIGIN, -AG_ECO9)
str(combined_all_years_numeric)

# Compute the Spearman correlation matrix

spearman_cor5 <- cor(combined_all_years_numeric, method = "spearman", use = "complete.obs")

# Visualize the correlation matrix
corrplot(spearman_cor5, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Spearman Rank Correlation Matrix (combined_all_years)", mar = c(0, 0, 1, 0)) 


names(combined_all_years)
#SELECT FROM THE COMBINAL ALL YEARS 
selected_data_combined_all_years <- combined_all_years %>%
  dplyr::select(MICX, CYLSPER, CHLA_RESULT, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, MIC_PTOX_biovolume, CYL_PTOX_biovolume, #REMOVED PTOX_biovolume, percent_PTOX_biovolume, 
                percent_cyanobacteria_biovolume, percent_MIC_PTOX_biovolume, percent_CYL_PTOX_biovolume, Shannon_Index, Simpson_Index, Evenness, 
                Temp_top1m, SECCHI, pH_top1m, ELEVATION, zmax, AREA_HA, thermocline_depth_rLake, AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLORIDE_RESULT, COLOR_RESULT, 
                COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NTL_RESULT, PTL_RESULT, SODIUM_RESULT, TURB_RESULT, SULFATE_RESULT, POTASSIUM_RESULT, NITRATE_NITRITE, TN_TP_RATIO, salinity)

#CORR
spearman_cor6 <- cor(selected_data_combined_all_years, method = "spearman", use = "complete.obs")

#PLOT ANOTHER WITH P-VALUE
# Function to compute correlation and p-value
cor_test <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  return(c(cor = test$estimate, p.value = test$p.value))
}

# Prepare matrices for correlation and p-values
n_vars <- ncol(selected_data_combined_all_years)
p_matrix <- matrix(1, n_vars, n_vars)
rownames(p_matrix) <- colnames(selected_data_combined_all_years)
colnames(p_matrix) <- colnames(selected_data_combined_all_years)

# Loop through the matrix to fill p-values
for (i in 1:(n_vars - 1)) {
  for (j in (i + 1):n_vars) {
    result <- cor_test(selected_data_combined_all_years[[i]], selected_data_combined_all_years[[j]])
    p_matrix[i, j] <- result["p.value"]
    p_matrix[j, i] <- result["p.value"]
  }
}

# Visualize with ggcorrplot-
# insignificant correlations (p > sig.level) are left blank in the plot below.
ggcorrplot(spearman_cor6, 
           method = "circle",     # Use "circle" or "square"
           type = "upper", 
           lab = FALSE, 
           p.mat = p_matrix, 
           sig.level = 0.05, 
           insig = "blank" ) #good plot
########################################################################cORRELATION PLOT FOR CYANOBACTERIA TAXA VS TOXINS



names(combined_all_years)
selected_phyto_combined_all_years <- combined_all_years %>%                       
  dplyr::select(MICX, CYLSPER, ANABAENOPSIS, ANABAENA, APHANIZOMENON, APHANOCAPSA,   
                RAPHIDIOPSIS, CYLINDROSPERMOPSIS, DOLICHOSPERMUM,       
                GLOEOTRICHIA, LEPTOLYNGBYA, LIMNOTHRIX, MERISMOPEDIA, 
                PHORMIDIUM, MICROCYSTIS, LYNGBYA,  OSCILLATORIA, PLANKTOTHRIX, PSEUDANABAENA, 
                RADIOCYSTIS, ROMERIA, SNOWELLA,  SYNECHOCOCCUS,          
                WORONICHINIA)

#CORR
spearman_cor7 <- cor(selected_phyto_combined_all_years, method = "spearman", use = "complete.obs")

# Visualize the correlation matrix
corrplot(spearman_cor7, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Spearman Rank Correlation Matrix (combined_all_years)", mar = c(0, 0, 1, 0)) 


#PLOT ANOTHER WITH P-VALUE
# Function to compute correlation and p-value
cor_test <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  return(c(cor = test$estimate, p.value = test$p.value))
}

# Prepare matrices for correlation and p-values
n_vars <- ncol(selected_phyto_combined_all_years)
p_matrix <- matrix(1, n_vars, n_vars)
rownames(p_matrix) <- colnames(selected_phyto_combined_all_years)
colnames(p_matrix) <- colnames(selected_phyto_combined_all_years)

# Loop through the matrix to fill p-values
for (i in 1:(n_vars - 1)) {
  for (j in (i + 1):n_vars) {
    result <- cor_test(selected_phyto_combined_all_years[[i]], selected_phyto_combined_all_years[[j]])
    p_matrix[i, j] <- result["p.value"]
    p_matrix[j, i] <- result["p.value"]
  }
}

# Visualize with ggcorrplot-
# insignificant correlations (p > sig.level) are left blank in the plot below.
ggcorrplot(spearman_cor7, 
           method = "square",     # Use "circle" or "square"
           type = "upper", 
           lab = TRUE, 
           p.mat = p_matrix, 
           sig.level = 0.01, 
           insig = "blank", 
           title = "Spearman Rank Correlation Matrix with P-values (selected_data_combined_all_years)") #good plot #Cyano groups and TOXINS plus p-value consideration.

#######################################################################################################
####################################################################To create two separate correlation matrix 

# Define toxin and taxa variable names
toxin_vars <- c("MICX", "CYLSPER")
taxa_vars <- c("ANABAENOPSIS", "ANABAENA", "APHANIZOMENON", "APHANOCAPSA",
               "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DOLICHOSPERMUM", "GLOEOTRICHIA",
               "LEPTOLYNGBYA", "LIMNOTHRIX", "MERISMOPEDIA", "PHORMIDIUM",
               "MICROCYSTIS", "LYNGBYA", "OSCILLATORIA", "PLANKTOTHRIX",
               "PSEUDANABAENA", "RADIOCYSTIS", "ROMERIA", "SNOWELLA",
               "SYNECHOCOCCUS", "WORONICHINIA")

# Subset the data
selected_data <- combined_all_years[, c(toxin_vars, taxa_vars)]

# Compute Spearman correlation matrix
cor_matrix <- cor(selected_data, method = "spearman", use = "complete.obs")

# Compute p-value matrix
p_matrix <- matrix(1, ncol = ncol(selected_data), nrow = ncol(selected_data))
rownames(p_matrix) <- colnames(p_matrix) <- colnames(selected_data)

for (i in 1:(ncol(selected_data) - 1)) {
  for (j in (i + 1):ncol(selected_data)) {
    test <- cor.test(selected_data[[i]], selected_data[[j]], method = "spearman")
    p_matrix[i, j] <- test$p.value
    p_matrix[j, i] <- test$p.value
  }
}

# Subset 1: MICX & CYLSPER vs Taxa
cor_MICX_CYL_vs_taxa <- cor_matrix[toxin_vars, taxa_vars]
p_MICX_CYL_vs_taxa <- p_matrix[toxin_vars, taxa_vars]

# Subset 2: Taxa vs Taxa
cor_taxa_vs_taxa <- cor_matrix[taxa_vars, taxa_vars]
p_taxa_vs_taxa <- p_matrix[taxa_vars, taxa_vars]

# Plot 1: MICX/CYLSPER vs Taxa
ggcorrplot(cor_MICX_CYL_vs_taxa,
           method = "square",
           type = "full",
           lab = TRUE,
           p.mat = p_MICX_CYL_vs_taxa,
           sig.level = 0.01,
           insig = "blank",
           title = "MICX and CYLSPER vs Cyanobacteria Taxa")

# Plot 2: Taxa vs Taxa
ggcorrplot(cor_taxa_vs_taxa,
           method = "square",
           type = "upper",
           lab = TRUE,
           p.mat = p_taxa_vs_taxa,
           sig.level = 0.01,
           insig = "blank",
           title = "Inter-correlation Among Cyanobacteria Taxa")

#FOR TIMES NEW ROMAN FONT#################################################################### Figure 1 plot
#install.packages("extrafont")
library(extrafont)
#font_import()       # Only run once; takes time
#loadfonts(device = "win")  # or use `device = "pdf"` for PDFs

library(patchwork)

#replot (make circle and lab FALSE)
plotA <- ggcorrplot(cor_MICX_CYL_vs_taxa,
                    method = "circle",
                    type = "full",
                    lab = TRUE,
                    p.mat = p_MICX_CYL_vs_taxa,
                    sig.level = 0.01,
                    insig = "blank") + #title = "MICX and CYLSPER vs Cyanobacteria Taxa"
  theme(
    text = element_text(family = "Times New Roman", face = "bold")
  )

#REPLOT (make circle and lab FALSE)
# Plot 2: Taxa vs Taxa
plotB <-ggcorrplot(cor_taxa_vs_taxa,
                   method = "circle",
                   type = "upper",
                   lab = TRUE,
                   p.mat = p_taxa_vs_taxa,
                   sig.level = 0.01,
                   insig = "blank")+  #title = "Inter-correlation Among Cyanobacteria Taxa"
  theme(
    text = element_text(family = "Times New Roman", face = "bold")
  )

final_plotCorr <- plotA + plotB +
  plot_annotation(tag_levels = "A")

final_plotCorr

#save 

# Save the combined plot
ggsave("final_plotCorr.png",
       plot = final_plotCorr,
       width = 18,
       height = 12,
       dpi = 600,
       bg = "white")
##############################################################################################create three depth classes based on zmax
names(combined_all_years)

library(dplyr)

combined_all_years <- combined_all_years %>%
  mutate(
    depth_class = case_when(
      zmax < 3 ~ "shallow",
      zmax >= 3 & zmax <= 15 ~ "mid-deep",
      zmax > 15 ~ "deep",
      TRUE ~ NA_character_  # fallback for missing or invalid zmax
    )
  )
#Check to confirm
table(combined_all_years$depth_class, useNA = "ifany")

#Boxplot of MICX vs. Depth Class with Stats

# Remove rows with NA in depth_class
combined_all_years_clean <- combined_all_years %>%
  filter(!is.na(depth_class))

# Make sure MICX is numeric
combined_all_years_clean$MICX <- as.numeric(combined_all_years_clean$MICX)
#Log-transform
combined_all_years_clean$log_MICX <- log1p(combined_all_years_clean$MICX)

# Boxplot with Kruskal-Wallis test and pairwise comparisons
plot2 <- ggplot(combined_all_years_clean, aes(x = depth_class, y = log_MICX)) +
  geom_boxplot(aes(fill = depth_class), outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_MICX, na.rm = TRUE) * 1.05) +  # Global test
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(
      c("shallow", "mid-deep"),
      c("shallow", "deep"),
      c("mid-deep", "deep")
    ),
    label = "p.signif",
    tip.length = 0.01
  ) +
  labs(title = "(B) log_MICX Concentration Across Lake Depth Classes",
       x = "Lake Depth Class",
       y = "log_MICX Concentration (µg/L)") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw()

##########################################################################
###########################################################for cylsper
#Boxplot of cylsper vs. Depth Class
#Log-transform

#Log-transform
combined_all_years_clean$log_CYLSPER <- log1p(combined_all_years_clean$CYLSPER)

# Boxplot with Kruskal-Wallis test and pairwise comparisons
plot3 <- ggplot(combined_all_years_clean, aes(x = depth_class, y = log_CYLSPER)) +
  geom_boxplot(aes(fill = depth_class), outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_CYLSPER, na.rm = TRUE) * 1.05) +  # Global test
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(
      c("shallow", "mid-deep"),
      c("shallow", "deep"),
      c("mid-deep", "deep")
    ),
    label = "p.signif",
    tip.length = 0.01
  ) +
  labs(title = "(C) log_CYLSPER Concentration Across Lake Depth Classes",
       x = "Lake Depth Class",
       y = "log_CYLSPER Concentration (µg/L)") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw()
##########################################################################CYNOBACTERIA BIOVOLUME: Boxplot of cyano vs. Depth Class
names(combined_all_years_clean)

#Log-transform
combined_all_years_clean$log_total_cyanobacteria_biovolume <- log1p(combined_all_years_clean$total_cyanobacteria_biovolume)


plot1 <- ggplot(combined_all_years_clean, aes(x = depth_class, y = log_total_cyanobacteria_biovolume)) +
  geom_boxplot(aes(fill = depth_class), outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black") +
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_total_cyanobacteria_biovolume, na.rm = TRUE) * 1.05) +  # Global test
  stat_compare_means(
    method = "wilcox.test",
    comparisons = list(
      c("shallow", "mid-deep"),
      c("shallow", "deep"),
      c("mid-deep", "deep")
    ),
    label = "p.signif",
    tip.length = 0.01
  ) +
  labs(title = "(A) log_total_cyanobacteria_biovolume Across Lake Depth Classes",
       x = "Lake Depth Class",
       y = "log_total_cyanobacteria_biovolume (µm3/mL)") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw()


library(patchwork)
# Combine the plots side-by-side
plot1 + plot2 + plot3 + plot_layout(ncol = 3)

#SAVE
# Combine plots
final_plot <- plot1 + plot2 + plot3 + plot_layout(ncol = 3)
# Save the combined plot
ggsave("combined_plot_highres.png", plot = final_plot,
       width = 18, height = 6, dpi = 500)


###########################################################boxplot of MICX across trophic categories
#####################################################################
names(combined_all_years_clean)


# Set factor levels if needed (optional)
combined_all_years_clean$trophic_status <- factor(combined_all_years_clean$trophic_status, 
                                                  levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic"))

# Plot 2: MICX
plot2 <- ggboxplot(combined_all_years_clean, x = "trophic_status", y = "log_MICX",
                   color = "trophic_status", palette = "Set2", add = "jitter") +
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_MICX, na.rm = TRUE) * 1.1) +  # Kruskal-Wallis
  stat_compare_means(method = "wilcox.test", label = "p.signif", comparisons = list(
    c("Oligotrophic", "Mesotrophic"), c("Mesotrophic", "Eutrophic"), c("Eutrophic", "Hypereutrophic")
  )) +
  theme_bw() +
  labs(title = "(B) log_MICX across Trophic Status", y = "log_MICX", x = NULL)

# Plot 3: CYLSPER
plot3 <- ggboxplot(combined_all_years_clean, x = "trophic_status", y = "log_CYLSPER",
                   color = "trophic_status", palette = "Set2", add = "jitter") +   #c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_CYLSPER, na.rm = TRUE) * 1.1) +
  stat_compare_means(method = "wilcox.test", label = "p.signif", comparisons = list(
    c("Oligotrophic", "Mesotrophic"), c("Mesotrophic", "Eutrophic"), c("Eutrophic", "Hypereutrophic")
  )) +
  theme_bw() +
  labs(title = "(C) log_CYLSPER across Trophic Status", y = "log_CYLSPER", x = NULL)

# Plot 1: Total Cyanobacteria Biovolume
plot1 <- ggboxplot(combined_all_years_clean, x = "trophic_status", y = "log_total_cyanobacteria_biovolume",
                   color = "trophic_status", palette = "Dark2", add = "jitter") +
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_total_cyanobacteria_biovolume, na.rm = TRUE) * 1.1) +
  stat_compare_means(method = "wilcox.test", label = "p.signif", comparisons = list(
    c("Oligotrophic", "Mesotrophic"), c("Mesotrophic", "Eutrophic"), c("Eutrophic", "Hypereutrophic")
  )) +
  theme_bw() +
  labs(title = "(A) log_total_cyanobacteria_biovolume across Trophic Status", 
       y = "log_total_cyanobacteria_biovolume", x = "Trophic Status")

# Combine plots
final_plot <- plot1 + plot2 + plot3 + plot_layout(ncol = 3)

# Show the plot
print(final_plot)

# save
ggsave("Trophic_Status_Boxplots_with_Stats.png", final_plot, width = 18, height = 6, dpi = 500)

############################################################################################################################LAKE ORIGIN
names(combined_all_years)
names(combined_all_years_clean)


# Ensure LAKE_ORIGIN is a factor
combined_all_years_clean$LAKE_ORIGIN <- factor(combined_all_years_clean$LAKE_ORIGIN, 
                                               levels = c("NATURAL", "MAN_MADE"))

# Plot 1: MICX
plot2 <- ggboxplot(combined_all_years_clean, x = "LAKE_ORIGIN", y = "log_MICX",
                   color = "LAKE_ORIGIN", palette = "Dark2", add = "jitter") +
  stat_compare_means(method = "wilcox.test", label.y = max(combined_all_years_clean$log_MICX, na.rm = TRUE) * 1.1) +
  theme_bw() +
  labs(title = "(B) log_MICX by Lake Origin", x = "Lake Origin", y = "log_MICX")

# Plot 2: CYLSPER
plot3 <- ggboxplot(combined_all_years_clean, x = "LAKE_ORIGIN", y = "log_CYLSPER",
                   color = "LAKE_ORIGIN", palette = "Set2", add = "jitter") +
  stat_compare_means(method = "wilcox.test", label.y = max(combined_all_years_clean$log_CYLSPER, na.rm = TRUE) * 1.1) +
  theme_bw() +
  labs(title = "(C) log_CYLSPER by Lake Origin", x = "Lake Origin", y = "log_CYLSPER")

# Plot 3: Total Cyanobacteria Biovolume
plot1 <- ggboxplot(combined_all_years_clean, x = "LAKE_ORIGIN", y = "log_total_cyanobacteria_biovolume",
                   color = "LAKE_ORIGIN", palette = c("#E41A1C", "#377EB8"), add = "jitter") +
  stat_compare_means(method = "wilcox.test", label.y = max(combined_all_years_clean$log_total_cyanobacteria_biovolume, na.rm = TRUE) * 1.1) +
  theme_bw() +
  labs(title = "(A) log_total_cyanobacteria_biovolume by Lake Origin", 
       x = "Lake Origin", y = "log_total_cyanobacteria_biovolume")

# Combine all plots
combined_plot2 <- plot1 + plot2 + plot3 + plot_layout(ncol = 3)

# Display
print(combined_plot2)

# Save the plot
ggsave("MICX_CYLSPER_Biovolume_by_Lake_Origin.png", combined_plot2, width = 18, height = 6, dpi = 500)

######################################################################################################################################ECOREGION
#check the levels
unique(combined_all_years_clean$AG_ECO9)

combined_all_years_clean$AG_ECO9 <- as.factor(combined_all_years_clean$AG_ECO9)
levels(combined_all_years_clean$AG_ECO9)



#check how many lakes fall into each ecoregion
ggplot(combined_all_years_clean, aes(x = AG_ECO9)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Lakes Across AG_ECO9 Ecoregions",
       x = "Ecoregion",
       y = "Count")


#fOR MICX
plot_micx <- ggplot(combined_all_years_clean, aes(x = AG_ECO9, y = log_MICX, fill = AG_ECO9)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_MICX, na.rm = TRUE) * 0.95) +
  theme_bw() +
  labs(title = "(B) log_MICX across Ecoregions", y = "log_MICX", x = "Ecoregion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

plot_cylsper <- ggplot(combined_all_years_clean, aes(x = AG_ECO9, y = log_CYLSPER, fill = AG_ECO9)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_CYLSPER, na.rm = TRUE) * 0.95) +
  theme_bw() +
  labs(title = "(C) log_CYLSPER across Ecoregions", y = "log_CYLSPER", x = "Ecoregion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

plot_cyano <- ggplot(combined_all_years_clean, aes(x = AG_ECO9, y = log_total_cyanobacteria_biovolume, fill = AG_ECO9)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  stat_compare_means(method = "kruskal.test", label.y = max(combined_all_years_clean$log_total_cyanobacteria_biovolume, na.rm = TRUE) * 0.95) +
  theme_bw() +
  labs(title = " (A) log_total_cyanobacteria_biovolume across Ecoregions", y = "log_total_cyanobacteria_biovolume", x = "Ecoregion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")



plot_cyano + plot_micx + plot_cylsper + plot_layout(ncol = 1)


# Save the plot
ecoregion_plot <- plot_cyano + plot_micx + plot_cylsper + plot_layout(ncol = 1)
ggsave("ecoregion_plot.png", ecoregion_plot, width = 20, height = 20, dpi = 500)
######################################################################################################MICX by Lake Origin, Colored by Depth Class

p2 <- ggplot(combined_all_years_clean, aes(x = LAKE_ORIGIN, y = log_MICX, fill = depth_class)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = depth_class), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), alpha = 0.4) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "(B) log_MICX across Lake Origin by Depth Class",
       x = "Lake Origin", y = "log_MICX Concentration") +
  theme_bw()

p3 <- ggplot(combined_all_years_clean, aes(x = LAKE_ORIGIN, y = log_CYLSPER, fill = depth_class)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = depth_class), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), alpha = 0.4) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "(C) CYLSPER across Lake Origin by Depth Class",
       x = "Lake Origin", y = "log_CYLSPER Concentration") +
  theme_bw()

p1 <- ggplot(combined_all_years_clean, aes(x = LAKE_ORIGIN, y = log_total_cyanobacteria_biovolume, fill = depth_class)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = depth_class), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), alpha = 0.4) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "(A) log_total_cyanobacteria_biovolume across Lake Origin by Depth Class",
       x = "Lake Origin", y = "log_total_cyanobacteria_biovolume") +
  theme_bw()

# Combine all plots
combined_p <- p1 + p2 + p3 + plot_layout(ncol = 3)

# Display
print(combined_p)

# Save the plot
ggsave("MICX_by_Lake Origin_Colored_by_Depth_Class.png", combined_p, width = 19, height = 6, dpi = 500)

##########################################################################################################################pca
###################################################################################################################4PCA

selected_data_combine_all <- combined_all_years %>%
  dplyr::select(MICX, CYLSPER, CHLA_RESULT, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, MIC_PTOX_biovolume, CYL_PTOX_biovolume, PTOX_biovolume, percent_cyanobacteria_biovolume,
                percent_PTOX_biovolume, percent_MIC_PTOX_biovolume, percent_CYL_PTOX_biovolume, Shannon_Index, Simpson_Index, Evenness, Temp_top1m, SECCHI, pH_top1m, ELEVATION, zmax, AREA_HA,
                thermocline_depth_rLake, AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NTL_RESULT, PTL_RESULT,
                SODIUM_RESULT, TURB_RESULT, SULFATE_RESULT, POTASSIUM_RESULT, salinity, NITRATE_NITRITE, TN_TP_RATIO)


# Check structure
str(selected_data_combine_all)

# Scale the dataset
selected_data_combine_all_scaled <- scale(selected_data_combine_all)

# Remove rows with missing values
selected_data_combine_all_clean <- selected_data_combine_all_scaled %>% 
  na.omit()

# Perform PCA
pca_model <- prcomp(selected_data_combine_all_clean, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_model)




# Extract the contributions for PC1 and PC2
contributions <- abs(pca_model$rotation[, 1:2])  # Absolute values of loadings for PC1 and PC2
contrib_sums <- rowSums(contributions)  # Sum of contributions for PC1 and PC2

# Identify top 10 contributing variables
top_vars <- names(sort(contrib_sums, decreasing = TRUE)[1:10]) # change 10 to 20, to see the top 20.

#see top 10
top_vars


# Plot with points and reduced variable labels
fviz_pca_biplot(pca_model, 
                repel = TRUE, 
                col.var = "steelblue", 
                col.ind = "gray", 
                pointshape = 16, 
                pointsize = 3, 
                alpha.ind = 0.6, 
                select.var = list(name = top_vars),
                title = "PCA Biplot: Selected Variables and Data Points")

#Replot
# Biplot
fviz_pca_biplot(pca_model, repel = TRUE, 
                col.var = 'red', col.ind = 'white', #USED TO SEE ONLY THE VARIBLES WHILE THE INDIVIDUAL OBERVATION ARE MADE WHITE 
                title = 'PCA Biplot')

###########################################################################################RDA

library(vegan)
#highly collinear variables (VIF > 10) to be removed
#vif.cca(rda_model)


names(combined_all_years)
# Select  few variables
selectp__all_years_few <- combined_all_years %>%
  dplyr::select(MICX, CYLSPER, CHLA_RESULT, total_phytoplankton_biovolume, PTOX_biovolume, percent_cyanobacteria_biovolume, total_cyanobacteria_biovolume, 
                percent_PTOX_biovolume, Shannon_Index, Evenness, Temp_top1m, SECCHI, pH_top1m, ELEVATION, zmax, 
                AMMONIA_N_RESULT, ANC_RESULT, CHLORIDE_RESULT, COLOR_RESULT, DOC_RESULT, NTL_RESULT, PTL_RESULT,
                TURB_RESULT, POTASSIUM_RESULT, CALCIUM_RESULT, salinity, NITRATE_NITRITE, TN_TP_RATIO
  ) %>%
  na.omit()  # Remove rows with missing data

#USE combined_all_years FIRST THEN REPLACE WITH
# Step 1: Response matrix (make sure both columns are numeric and have no NA)
response_vars <- selectp__all_years_few[, c("MICX", "CYLSPER")]

# Step 2: Explanatory variables (remove MICX and CYLSPER from predictors)
predictor_vars <- selectp__all_years_few %>%
  dplyr::select(-MICX, -CYLSPER)

# Optional: Remove rows with missing values in either block
complete_cases <- complete.cases(response_vars, predictor_vars)
response_vars <- response_vars[complete_cases, ]
predictor_vars <- predictor_vars[complete_cases, ]

# Log-transform toxin data
response_vars_log <- log10(response_vars)  

# Step 3: Run RDA
rda_model_all <- rda(response_vars_log ~ ., data = predictor_vars)


#PLOT
plot(rda_model_all, type = "n", scaling = 2, main = "RDA: log_toxins vs Environmental Variables")   # Set up empty plot

# Add elements
points(rda_model_all, display = "sites", col = "blue", pch = 20, scaling = 2)  # sites/samples
text(rda_model_all, display = "species", col = "red", cex = 1.2, scaling = 2)  # response vars (toxins)
text(rda_model_all, display = "bp", col = "darkgreen", cex = 1.1, scaling = 2) # environmental variables


summary(rda_model_all)
anova(rda_model_all)

anova(rda_model_all)                      # Global test
anova(rda_model_all, by = "terms")        # Test each environmental variable
anova(rda_model_all, by = "axis")         # Test each axis
#################################################################################group label based on dominant toxin
#################################################################################group label based on dominant toxin

# Log-transform toxin data safely
response_vars_log2 <- log10(response_vars)  


# Add a group label based on dominant toxin
dominant4_toxin <- apply(response_vars_log2, 1, function(x) { #response_vars #response_vars_log2
  if (x["MICX"] > x["CYLSPER"]) {
    "MICX-dominant"
  } else if (x["CYLSPER"] > x["MICX"]) {
    "CYLSPER-dominant"
  } else {
    "Equal"
  }
})

# Run RDA
rda_model4_log <- rda(response_vars_log2 ~ ., data = predictor_vars) ##response_vars_log2

# Extract site scores
site2_scores <- scores(rda_model4_log, display = "sites", scaling = 2)

# Plot and color points based on dominant toxin
plot(rda_model4_log, type = "n", scaling = 2, main = "RDA: Sites Colored by Dominant toxin_data_log")
cols <- c("MICX-dominant" = "red", "CYLSPER-dominant" = "blue", "Equal" = "purple")
points(site2_scores, col = cols[dominant4_toxin], pch = 19)                               #Change dominant4_toxin

# Add legend and labels
legend("topright", legend = names(cols), col = cols, pch = 19, title = "Dominant Toxin")
text(rda_model4_log, display = "species", col = "black", cex = 1.2, scaling = 2)
text(rda_model4_log, display = "bp", col = "darkgreen", cex = 1.1, scaling = 2)
####################################################################################SAVE HIGH RES IMAGE

# Open PNG device with high resolution
png("C:/Users/Yusuf_Olaleye1/Downloads/RDA_PLOT.png", width = 7000, height = 4000, res = 600)  # width/height in pixels, res in dpi

# Plot your RDA
plot(rda_model4_log, type = "n", scaling = 2, main = "RDA: Sites Colored by Dominant toxin_data (log)")
cols <- c("MICX-dominant" = "red", "CYLSPER-dominant" = "blue", "Equal" = "purple")
points(site2_scores, col = cols[dominant4_toxin], pch = 19)
legend("topright", legend = names(cols), col = cols, pch = 19, title = "Dominant Toxin")
text(rda_model4_log, display = "species", col = "black", cex = 1.2, scaling = 2)
text(rda_model4_log, display = "bp", col = "darkgreen", cex = 1.1, scaling = 2)

# Close the device
dev.off()
########################################################################## updated RDA bigger text and trim area (Figure 2 RDA PLOT)

################################### Set up plot with larger labels
library(showtext)
# Load Times New Roman (Windows path)
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

png("RDA_plot3.png", width = 10.5, height = 12, units = "in", res = 300) #????
par(family = "Times New Roman")

# Set up plot with larger labels
plot(rda_model4_log, type = "n", scaling = 2,
     cex.lab = 1.6,    # axis labels
     cex.axis = 1.2)   # tick labels

# Define colors
cols <- c("MICX-dominant" = "red",
          "CYLSPER-dominant" = "blue",
          "Equal" = "purple")

# Plot sites
points(site2_scores,
       col = cols[dominant4_toxin],
       pch = 19,
       cex = 1.4)

# Add legend (cleaner and slightly larger)
legend(x = 1.5, y = -1,  # adjust coordinates as needed
       legend = names(cols),
       col = cols,
       pch = 19,
       pt.cex = 1.3,
       cex = 1.2,
       title = "Dominant Toxin",
       bty = "n")

# Add response variables (toxins)
text(rda_model4_log, display = "species",
     col = "black",
     cex = 1.5,
     font = 2,
     scaling = 2)

# Add environmental variables (labels)
text(rda_model4_log, display = "bp",
     col = "darkgreen",
     cex = 1.3,
     font = 2,
     scaling = 2)

# Optional: Add arrows for environmental gradients
arrows(0, 0,
       scores(rda_model4_log, display = "bp", scaling = 2)[,1],
       scores(rda_model4_log, display = "bp", scaling = 2)[,2],
       col = "darkgreen",
       length = 0.1,
       lwd = 1.5)


# (rest of your plotting code...)
dev.off()

summary(rda_model4_log)
anova(rda_model4_log)
anova(rda_model4_log, by = "terms") 


###############################################################################################################total number of unique lakes IN COMBIANE ALL

n_lakes <- combined_all_years %>%
  dplyr::distinct(UNIQUE_ID) %>%
  nrow()

print(n_lakes) #tOTAL =3559

#OR
length(unique(combined_all_years$UNIQUE_ID))
##tOTAL =3559

names(combined_all_years)

combined_all_years$thermocline_depth
##############################################################################################################stratification_type

#New column for stratification type
combined_all_years_clean <- combined_all_years_clean %>%
  mutate(stratification_type = ifelse(is.na(thermocline_depth_rLake), "Isothermal", "Stratified"))
# View summary
table(combined_all_years_clean$stratification_type)


#check the levels
unique(combined_all_years_clean$stratification_type)
combined_all_years_clean$stratification_type <- as.factor(combined_all_years_clean$stratification_type)
levels(combined_all_years_clean$stratification_type)



# Plot 1: MICX
plotb <- ggboxplot(combined_all_years_clean, x = "stratification_type", y = "log_MICX",
                   color = "stratification_type", palette = "Dark1", add = "jitter") +
  stat_compare_means(method = "wilcox.test", label.y = max(combined_all_years_clean$log_MICX, na.rm = TRUE) * 1.1) +
  theme_bw() +
  labs(title = "(B) log_MICX by stratification_type", x = "stratification_type", y = "log_MICX")

# Plot 2: CYLSPER
plotc <- ggboxplot(combined_all_years_clean, x = "stratification_type", y = "log_CYLSPER",
                   color = "stratification_type", palette = "Set2", add = "jitter") +
  stat_compare_means(method = "wilcox.test", label.y = max(combined_all_years_clean$log_CYLSPER, na.rm = TRUE) * 1.1) +
  theme_bw() +
  labs(title = "(C) log_CYLSPER by stratification_type", x = "stratification_type", y = "log_CYLSPER")

# Plot 3: Total Cyanobacteria Biovolume
plota <- ggboxplot(combined_all_years_clean, x = "stratification_type", y = "log_total_cyanobacteria_biovolume",
                   color = "stratification_type", palette = c("#7570b3", "#d95f02"), add = "jitter") +
  stat_compare_means(method = "wilcox.test", label.y = max(combined_all_years_clean$log_total_cyanobacteria_biovolume, na.rm = TRUE) * 1.1) +
  theme_bw() +
  labs(title = "(A) log_total_cyanobacteria_biovolume by stratification_type", 
       x = "stratification_type", y = "log_total_cyanobacteria_biovolume")

# Combine all plots
combined_plota <- plota + plotb + plotc + plot_layout(ncol = 3)

# Display
print(combined_plota)

# Save the plot
ggsave("MICX_CYLSPER_Biovolume_by_stratification_type.png", combined_plota, width = 20, height = 6, dpi = 500)


################################################################################################# STATISTICS FOR LAKE FEATURES SUCH AS DEPTH CLASS, TROPHIC STATUS, LAKE ORIGIN, STATIFICATION TYPE
kruskal.test(MICX ~ depth_class, data = combined_all_years_clean)
kruskal.test(CYLSPER ~ depth_class, data = combined_all_years_clean)
kruskal.test(total_cyanobacteria_biovolume ~ depth_class, data = combined_all_years_clean)

#postHOC
########################depth class
#1- Cyanobacteria
pairwise.wilcox.test(
  combined_all_years_clean$total_cyanobacteria_biovolume,
  combined_all_years_clean$depth_class,
  p.adjust.method = "BH"   # Benjamini-Hochberg correction, you can also use "bonferroni"
)
#gET  group medians or means
combined_all_years_clean %>%
  group_by(depth_class) %>%
  summarise(
    median_total_cyanobacteria_biovolume = median(total_cyanobacteria_biovolume, na.rm = TRUE),
    mean_total_cyanobacteria_biovolume = mean(total_cyanobacteria_biovolume, na.rm = TRUE)
  )

#2- MICX
pairwise.wilcox.test(
  combined_all_years_clean$MICX,
  combined_all_years_clean$depth_class,
  p.adjust.method = "BH"   # Benjamini-Hochberg correction, you can also use "bonferroni"
)
#gET  group medians or means
combined_all_years_clean %>%
  group_by(depth_class) %>%
  summarise(
    median_MICX = median(MICX, na.rm = TRUE),
    mean_MICX = mean(MICX, na.rm = TRUE)
  )

#3- CYLSPER
pairwise.wilcox.test(
  combined_all_years_clean$CYLSPER,
  combined_all_years_clean$depth_class,
  p.adjust.method = "BH"   # Benjamini-Hochberg correction, you can also use "bonferroni"
)
#gET  group medians or means
combined_all_years_clean %>%
  group_by(depth_class) %>%
  summarise(
    median_CYLSPER = median(CYLSPER, na.rm = TRUE),
    mean_CYLSPER = mean(CYLSPER, na.rm = TRUE)
  )
######################trophic status
kruskal.test(total_cyanobacteria_biovolume ~ trophic_status, data = combined_all_years_clean)
kruskal.test(MICX ~ trophic_status, data = combined_all_years_clean)
kruskal.test(CYLSPER ~ trophic_status, data = combined_all_years_clean)

#postHOC
########################trophic_status
#1- Cyanobacteria
pairwise.wilcox.test(
  combined_all_years_clean$total_cyanobacteria_biovolume,
  combined_all_years_clean$trophic_status,
  p.adjust.method = "BH"   # Benjamini-Hochberg correction, you can also use "bonferroni"
)
#gET  group medians or means
combined_all_years_clean %>%
  group_by(trophic_status) %>%
  summarise(
    median_total_cyanobacteria_biovolume = median(total_cyanobacteria_biovolume, na.rm = TRUE),
    mean_total_cyanobacteria_biovolume = mean(total_cyanobacteria_biovolume, na.rm = TRUE)
  )

#2- MICX
pairwise.wilcox.test(
  combined_all_years_clean$MICX,
  combined_all_years_clean$trophic_status,
  p.adjust.method = "BH"   # Benjamini-Hochberg correction, you can also use "bonferroni"
)
#gET  group medians or means
combined_all_years_clean %>%
  group_by(trophic_status) %>%
  summarise(
    median_MICX = median(MICX, na.rm = TRUE),
    mean_MICX = mean(MICX, na.rm = TRUE)
  )

#3- CYLSPER
pairwise.wilcox.test(
  combined_all_years_clean$CYLSPER,
  combined_all_years_clean$trophic_status,
  p.adjust.method = "BH"   # Benjamini-Hochberg correction, you can also use "bonferroni"
)
#gET  group medians or means
combined_all_years_clean %>%
  group_by(trophic_status) %>%
  summarise(
    median_CYLSPER = median(CYLSPER, na.rm = TRUE),
    mean_CYLSPER = mean(CYLSPER, na.rm = TRUE)
  )
###########
#############################LAKE ORIGIN
kruskal.test(total_cyanobacteria_biovolume ~ LAKE_ORIGIN, data = combined_all_years_clean)
kruskal.test(MICX ~ LAKE_ORIGIN, data = combined_all_years_clean)
kruskal.test(CYLSPER ~ LAKE_ORIGIN, data = combined_all_years_clean)


#MICX
pairwise.wilcox.test(
  combined_all_years_clean$MICX,
  combined_all_years_clean$LAKE_ORIGIN,
  p.adjust.method = "BH"   
)

#gET  group medians or means
combined_all_years_clean %>%
  group_by(LAKE_ORIGIN) %>%
  summarise(
    median_MICX = median(MICX, na.rm = TRUE),
    mean_MICX = mean(MICX, na.rm = TRUE)
  )

#CYANO
combined_all_years_clean %>%
  group_by(LAKE_ORIGIN) %>%
  summarise(
    median_total_cyanobacteria_biovolume = median(total_cyanobacteria_biovolume, na.rm = TRUE),
    mean_total_cyanobacteria_biovolume = mean(total_cyanobacteria_biovolume, na.rm = TRUE)
  )
#CYLSPER
combined_all_years_clean %>%
  group_by(LAKE_ORIGIN) %>%
  summarise(
    median_CYLSPER = median(CYLSPER, na.rm = TRUE),
    mean_CYLSPER = mean(CYLSPER, na.rm = TRUE)
  )

#######################lake stratification
#MICX
pairwise.wilcox.test(
  combined_all_years_clean$MICX,
  combined_all_years_clean$stratification_type,
  p.adjust.method = "BH"   
)


#sig p<0.0001
names(combined_all_years_clean)
#gET  group medians or means
combined_all_years_clean %>%
  group_by(stratification_type) %>%
  summarise(
    median_MICX = median(MICX, na.rm = TRUE),
    mean_MICX = mean(MICX, na.rm = TRUE)
  )


#CYLSPER
pairwise.wilcox.test(
  combined_all_years_clean$CYLSPER,
  combined_all_years_clean$stratification_type,
  p.adjust.method = "BH"   
)
combined_all_years_clean %>%
  group_by(stratification_type) %>%
  summarise(
    median_CYLSPER = median(CYLSPER, na.rm = TRUE),
    mean_CYLSPER = mean(CYLSPER, na.rm = TRUE)
  )


#CYANO
pairwise.wilcox.test(
  combined_all_years_clean$total_cyanobacteria_biovolume,
  combined_all_years_clean$stratification_type,
  p.adjust.method = "BH"   
)

combined_all_years_clean %>%
  group_by(stratification_type) %>%
  summarise(
    median_total_cyanobacteria_biovolume = median(total_cyanobacteria_biovolume, na.rm = TRUE),
    mean_total_cyanobacteria_biovolume = mean(total_cyanobacteria_biovolume, na.rm = TRUE)
  )

#######################################ECOREGION

#CYANO
pairwise.wilcox.test(
  combined_all_years_clean$total_cyanobacteria_biovolume,
  combined_all_years_clean$AG_ECO9,
  p.adjust.method = "BH"   
)

#MICX
pw1 <- pairwise.wilcox.test(
  combined_all_years_clean$MICX,
  combined_all_years_clean$AG_ECO9,
  p.adjust.method = "BH"   
)


#MICX
pw1 <- pairwise.wilcox.test(
  combined_all_years_clean$MICX,
  combined_all_years_clean$AG_ECO9,
  p.adjust.method = "BH"   
)

######################Create compact letter display
# Convert p-value matrix to vector
#For MICX
pvals <- pw1$p.value
pvals_vec <- as.vector(pvals)
names(pvals_vec) <- paste(
  rownames(pvals)[row(pvals)],
  colnames(pvals)[col(pvals)],
  sep = "-"
)

# Remove NA comparisons
pvals_vec <- pvals_vec[!is.na(pvals_vec)]
# Generate compact letter display
letters <- multcompLetters(pvals_vec)$Letters
letters

#CYLSPER
pw2 <- pairwise.wilcox.test(
  combined_all_years_clean$CYLSPER,
  combined_all_years_clean$AG_ECO9,
  p.adjust.method = "BH"   
)
#############################Create compact letter display
# Convert p-value matrix to vector
#For CYLSPER
pvals2 <- pw2$p.value
pvals_vect <- as.vector(pvals2)
names(pvals_vect) <- paste(
  rownames(pvals2)[row(pvals2)],
  colnames(pvals2)[col(pvals2)],
  sep = "-"
)

# Remove NA comparisons
pvals_vect <- pvals_vect[!is.na(pvals_vect)]
# Generate compact letter display
letters2 <- multcompLetters(pvals_vect)$Letters
letters2

#####################MAXIMUM_toxins values################
names(combined_all_years_clean)
#1-TROPHIC STATUS
# Toget the max concentration in each class
max_MICX_by_trophic <- combined_all_years_clean %>%
  group_by(trophic_status) %>%
  slice_max(order_by = MICX, n = 1, with_ties = FALSE) %>%
  dplyr::select(trophic_status, UNIQUE_ID, Year, MICX)
# View the result
max_MICX_by_trophic

#cylsper
# Toget the max concentration in each class
max_CYLSPER_by_trophic <- combined_all_years_clean %>%
  group_by(trophic_status) %>%
  slice_max(order_by = CYLSPER, n = 1, with_ties = FALSE) %>%
  dplyr::select(trophic_status, UNIQUE_ID, Year, CYLSPER)
# View the result
max_CYLSPER_by_trophic

#2-DEPTH CLASS
# Toget the max concentration in each class
max_MICX_by_depth_class <- combined_all_years_clean %>%
  group_by(depth_class) %>%
  slice_max(order_by = MICX, n = 1, with_ties = FALSE) %>%
  dplyr::select(depth_class, UNIQUE_ID, Year, MICX)
# View the result
max_MICX_by_depth_class

#cylsper
# Toget the max concentration in each class
max_CYLSPER_by_depth_class <- combined_all_years_clean %>%
  group_by(depth_class) %>%
  slice_max(order_by = CYLSPER, n = 1, with_ties = FALSE) %>%
  dplyr::select(depth_class, UNIQUE_ID, Year, CYLSPER)
# View the result
max_CYLSPER_by_depth_class

#3-LAKE ORIGIN
max_MICX_by_LAKE_ORIGIN <- combined_all_years_clean %>%
  group_by(LAKE_ORIGIN) %>%
  slice_max(order_by = MICX, n = 1, with_ties = FALSE) %>%
  dplyr::select(LAKE_ORIGIN, UNIQUE_ID, Year, MICX)
# View the result
max_MICX_by_LAKE_ORIGIN

#cylsper
max_CYLSPER_by_LAKE_ORIGIN <- combined_all_years_clean %>%
  group_by(LAKE_ORIGIN) %>%
  slice_max(order_by = CYLSPER, n = 1, with_ties = FALSE) %>%
  dplyr::select(LAKE_ORIGIN, UNIQUE_ID, Year, CYLSPER)
# View the result
max_CYLSPER_by_LAKE_ORIGIN

#
#4-LAKE stratification type
max_MICX_by_stratification_type <- combined_all_years_clean %>%
  group_by(stratification_type) %>%
  slice_max(order_by = MICX, n = 1, with_ties = FALSE) %>%
  dplyr::select(stratification_type, UNIQUE_ID, Year, MICX)
# View the result
max_MICX_by_stratification_type

#cylsper
max_CYLSPER_by_stratification_type <- combined_all_years_clean %>%
  group_by(stratification_type) %>%
  slice_max(order_by = CYLSPER, n = 1, with_ties = FALSE) %>%
  dplyr::select(stratification_type, UNIQUE_ID, Year, CYLSPER)
# View the result
max_CYLSPER_by_stratification_type

#5-ECOREGIONS
max_MICX_by_AG_ECO9 <- combined_all_years_clean %>%
  group_by(AG_ECO9) %>%
  slice_max(order_by = MICX, n = 1, with_ties = FALSE) %>%
  dplyr::select(AG_ECO9, UNIQUE_ID, Year, MICX)
# View the result
max_MICX_by_AG_ECO9

#cylsper
max_CYLSPER_by_AG_ECO9 <- combined_all_years_clean %>%
  group_by(AG_ECO9) %>%
  slice_max(order_by = CYLSPER, n = 1, with_ties = FALSE) %>%
  dplyr::select(AG_ECO9, UNIQUE_ID, Year, CYLSPER)
# View the result
max_CYLSPER_by_AG_ECO9


#########################################################################################################MAPS FOR SPATIAL VARIABILITY

#salinity INDEX
# Get U.S. state map data
us_states <- map_data("state")

# Plot
ggplot() +
  # Add state map
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  
  # Add lake points colored by trophic status
  geom_point(data = combined_all_years_clean %>% filter(!is.na(salinity)),
             aes(x = LON_DD83, y = LAT_DD83, color = salinity),
             size = 3) +
  scale_color_viridis_c(limits = c(0, 6000), oob = scales::squish) +  # Adjust this range as needed
  theme_minimal() +
  coord_fixed(1.3)+
  labs(title = "Spatial Variation in Salinity index", color = "Salinity index (mg/L)") +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )

#####################CHLORIDE
# Plot
ggplot() +
  # Add state map
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  
  # Add lake points colored by trophic status
  geom_point(data = combined_all_years_clean %>% filter(!is.na(CHLORIDE_RESULT)),
             aes(x = LON_DD83, y = LAT_DD83, color = CHLORIDE_RESULT),
             size = 3) +
  scale_color_viridis_c(
    limits = c(0, 250),
    oob = scales::squish,
    breaks = c(0, 50, 100, 150, 200, 250),
    labels = c("0", "50", "100", "150", "200", "≥250")
  ) +
  theme_minimal() +
  coord_fixed(1.3)+
  labs(title = "Spatial Variation in Chloride concentrations", color = "Chloride (mg/L)")

######
#trophic status

# Ensure the trophic_status is a factor with the correct order
combined_all_years_clean <- combined_all_years_clean %>%
  mutate(trophic_status = factor(trophic_status,
                                 levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic")))

us_states <- map_data("state")
ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = combined_all_years_clean %>% filter(!is.na(trophic_status)),
             aes(x = LON_DD83, y = LAT_DD83, color = trophic_status),
             size = 3, ) +
  scale_color_manual(values = c(
    "Oligotrophic" = "#1f78b4",    
    "Mesotrophic" = "#33a02c",    
    "Eutrophic" = "#ffcc00",      
    "Hypereutrophic" = "#e31a1c"   
  )) +
  
  labs(title = "Spatial Variation in Trophic Status",
       color = "Trophic Status") +
  coord_fixed(1.3) +  # Keeps map aspect ratio
  theme_minimal()+
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )

#############################depth class

unique(combined_all_years_clean$depth_class)

# Ensure the its a factor with the correct order
combined_all_years_clean <- combined_all_years_clean %>%
  mutate(depth_class = factor(depth_class,
                              levels = c("shallow", "mid-deep", "deep")))

ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = combined_all_years_clean,
             aes(x = LON_DD83, y = LAT_DD83, color = depth_class),
             size = 3) +
  scale_color_manual(values = c(
    "shallow" = "#7570b3",    
    "mid-deep" = "#33a02c",    
    "deep" = "#e31a1c"   
  )) +
  
  labs(title = "Spatial Variation in depth class",
       color = "depth_class") +
  coord_fixed(1.3) +  # Keeps map aspect ratio
  theme_minimal() +
  theme(
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )
###################################MICX

MIC_MAP <- ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = combined_all_years_clean, aes(x = LON_DD83, y = LAT_DD83, color = MICX),
             size = 2 ) +
  scale_color_viridis_c(
    limits = c(0, 8),
    oob = scales::squish,
    breaks = c(0, 2, 4, 6, 8),
    #To modify the legend to reflect that values above 8 are squished into the upper limit.
    labels = c("0", "2", "4", "6", "≥8")
  ) +
  labs(
    title = "(A) Spatial Variation in MICX",
    color = "MICX (µg/L)"
  ) +
  coord_fixed(1.3) +
  theme_minimal()
MIC_MAP
#################CYLSPER
CYL_MAP <- ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = combined_all_years_clean %>% filter(!is.na(CYLSPER)), #REMOVE NA
             aes(x = LON_DD83, y = LAT_DD83, color = CYLSPER),
             size = 2,  ) +
  scale_color_viridis_c(
    limits = c(0, 1.25),
    oob = scales::squish,
    breaks = c(0, 0.25, 0.50, 0.75, 1.00, 1.25),
    labels = c("0", "0.25", "0.5", "0.75", "1.00", "≥1.25")
  ) +
  labs(
    title = "(B) Spatial Variation in CYLSPER",
    color = "CYLSPER (µg/L)"
  ) +
  coord_fixed(1.3) +
  theme_minimal()
CYL_MAP


#COMBINE
combine_MAP <- MIC_MAP + CYL_MAP + plot_layout(ncol = 2)

# Show the plot
print(combine_MAP)


###################

#Check nmanes
names(combined_all_years_clean)

#INDIVIDUAL RELATIONSHIPS CHECKS
#cHECK RELATIONSHIPS BETWEN calculated thermocline_depth AND thermocline_depth_rLake
combined_all_years_clean %>%
  ggplot(aes(x = CHLA_RESULT, y = MICX)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw()

#notes: total cyano vs micx rho=0.26; PTOX Biovolume= 0.24; mic_ptox vs micx= 0.24; MICROCYSTIS vs micx= 0.22
#total_phytoplankton_biovolume VS MICX=0.18

combined_all_years_clean %>%
  ggplot(aes(x = total_phytoplankton_biovolume, y = MICX)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw()
#TO DO: MAKE CORR MATRIX FOR BIOLOGICAL INDICATORS OF TOXINS VS MICX AND CYSPERS

##########################cORR MATRIX: Biological indicators
#Check nmanes
names(combined_all_years_clean)

# Select  biological indicators of toxins
Bio_indicators <- combined_all_years %>%
  dplyr::select(MICX, CYLSPER, CHLA_RESULT, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, percent_cyanobacteria_biovolume,
                MIC_PTOX_biovolume, CYL_PTOX_biovolume, percent_MIC_PTOX_biovolume, percent_CYL_PTOX_biovolume, 
                Shannon_Index, Simpson_Index, Evenness
  ) %>%
  na.omit()  # Remove rows with missing data

#CORR
spearman_cor_indicators <- cor(Bio_indicators, method = "spearman", use = "complete.obs")

# Visualize the correlation matrix
corrplot(spearman_cor_indicators, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Phytoplankton composition metrics vs. cyanotoxins", mar = c(0, 0, 1, 0)) 

#plot2
#PLOT ANOTHER WITH P-VALUE
# Function to compute correlation and p-value
cor_test <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  return(c(cor = test$estimate, p.value = test$p.value))
}

# Prepare matrices for correlation and p-values
n_vars <- ncol(Bio_indicators)
p_matrix <- matrix(1, n_vars, n_vars)
rownames(p_matrix) <- colnames(Bio_indicators)
colnames(p_matrix) <- colnames(Bio_indicators)

# Loop through the matrix to fill p-values
for (i in 1:(n_vars - 1)) {
  for (j in (i + 1):n_vars) {
    result <- cor_test(Bio_indicators[[i]], Bio_indicators[[j]])
    p_matrix[i, j] <- result["p.value"]
    p_matrix[j, i] <- result["p.value"]
  }
}

# Visualize with ggcorrplot-
# insignificant correlations (p > sig.level) are left blank in the plot below.
ggcorrplot(spearman_cor_indicators, 
           method = "square",     # Use "circle" or "square"
           type = "upper", 
           lab = TRUE, 
           p.mat = p_matrix, 
           sig.level = 0.01, #note p<0.01 instead of 0.05
           insig = "blank", 
           title = "Phytoplankton composition metrics vs. cyanotoxins") 

##############################################################################################
#####################################################################################CONDITIONAL INFERENCE TREE

#conditional inference tree
# Load the necessary libraries

# Ensure MICX is numeric
combined_all_years_clean$MICX <- as.numeric(combined_all_years_clean$MICX)

##IF CYLSPER has NAs or zeros,
combined_all_years_clean <- combined_all_years_clean %>%
  filter(!is.na(CYLSPER)) %>%
  mutate(CYLSPER = ifelse(CYLSPER < 0, NA, CYLSPER))  # remove negatives if any

# Ensure is numeric
combined_all_years_clean$CYLSPER <- as.numeric(combined_all_years_clean$CYLSPER)

#REMOVEW NAs
combined_all_years_clean <- combined_all_years_clean %>%
  filter(!is.na(total_cyanobacteria_biovolume)) %>%
  mutate(total_cyanobacteria_biovolume = ifelse(total_cyanobacteria_biovolume < 0, NA, total_cyanobacteria_biovolume))  # remove negatives if any
# Ensure is numeric
combined_all_years_clean$total_cyanobacteria_biovolume <- as.numeric(combined_all_years_clean$total_cyanobacteria_biovolume)

#Check names
names(combined_all_years_clean)

# Define the predictors
predictors_MICX <- c("CHLA_RESULT", "total_phytoplankton_biovolume", "total_cyanobacteria_biovolume", "MIC_PTOX_biovolume", 
                     "percent_cyanobacteria_biovolume", "percent_MIC_PTOX_biovolume", "Shannon_Index", "Simpson_Index", "Evenness", "Temp_top1m", 
                     "SECCHI", "pH_top1m", "ELEVATION", "zmax","thermocline_depth_rLake", "AREA_HA", "AMMONIA_N_RESULT", "ANC_RESULT", "CALCIUM_RESULT", 
                     "CHLORIDE_RESULT", "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", 
                     "NTL_RESULT", "PTL_RESULT", "SODIUM_RESULT", 
                     "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT", "TN_TP_RATIO", "NITRATE_NITRITE")

predictors_MICX2 <- c("CHLA_RESULT","total_cyanobacteria_biovolume",
                      "percent_cyanobacteria_biovolume", "Shannon_Index", "Simpson_Index", "Evenness", "Temp_top1m", 
                      "SECCHI", "PH_RESULT", "zmax", "ANC_RESULT", "CALCIUM_RESULT", 
                      "CHLORIDE_RESULT", "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", 
                      "NTL_RESULT", "PTL_RESULT", "SODIUM_RESULT", 
                      "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")

predictors_MICX_new <- c("total_cyanobacteria_biovolume", "Temp_top1m","SECCHI", "PH_RESULT", "zmax", "ELEVATION", "thermocline_depth_rLake", "AREA_HA",
                         "ANC_RESULT", "CALCIUM_RESULT", "CHLORIDE_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", "AMMONIA_N_RESULT",
                         "NTL_RESULT", "PTL_RESULT", "TN_TP_RATIO", "SODIUM_RESULT", "NITRATE_NITRITE",
                         "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")

predictors_MICX_new2 <- c("CHLA_RESULT", "Temp_top1m","zmax","DOC_RESULT", "AMMONIA_N_RESULT",
                          "NTL_RESULT", "PTL_RESULT", "TN_TP_RATIO", "NITRATE_NITRITE"
)

predictors_MICX_Bonilla_var <- c("total_cyanobacteria_biovolume", "Temp_top1m", "pH_top1m", "depth_class", "ELEVATION","AREA_HA",
                                 "NTL_RESULT", "PTL_RESULT")

#TOXIN
toxin_predictors <- c("total_cyanobacteria_biovolume", "Temp_top1m", "pH_top1m", "depth_class", "ELEVATION","AREA_HA",
                      "NTL_RESULT", "PTL_RESULT", "CALCIUM_RESULT", "Evenness", "CHLORIDE_RESULT", "SULFATE_RESULT",
                      "TURB_RESULT", "DOC_RESULT", "AMMONIA_N_RESULT","TN_TP_RATIO", "MAGNESIUM_RESULT")


#cyano predictors

predictors_Cyano <- c("Temp_top1m","SECCHI", "PH_RESULT", "zmax","ANC_RESULT", "CALCIUM_RESULT", 
                      "CHLORIDE_RESULT", "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", 
                      "NTL_RESULT", "PTL_RESULT", "SODIUM_RESULT", 
                      "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")


# Create the formula
formula <- as.formula(paste("MICX ~", paste(toxin_predictors, collapse = " + ")))

formula2 <- as.formula(paste("CYLSPER ~", paste(toxin_predictors, collapse = " + ")))

formula3 <- as.formula(paste("total_cyanobacteria_biovolume ~", paste(predictors_Cyano, collapse = " + ")))


# Fit the conditional inference tree
micx_ctree <- ctree(formula, data = combined_all_years_clean)

cylsper_ctree <- ctree(formula2, data = combined_all_years_clean)


#Cyano_ctree <- ctree(formula3, data = combined_all_years_clean)

# Plot the tree
plot(micx_ctree, main = "Conditional Inference Tree for MICX")

plot(cylsper_ctree, main = "Conditional Inference Tree for CYLSPER")

#plot(Cyano_ctree, main = "Conditional Inference Tree for cyanobacteria")

##SAFE PLOTS
png("C:/Users/Yusuf_Olaleye1/Downloads/MICX_ctree.png",
    width = 28, height = 14, units = "in", res = 600) #ADJUST width and height
plot(micx_ctree, main = "Conditional Inference Tree for MICX")
dev.off()

#cylsper
png("C:/Users/Yusuf_Olaleye1/Downloads/cylsper_ctree.png",
    width = 21, height = 10, units = "in", res = 600) #ADJUST width and height
plot(cylsper_ctree, main = "Conditional Inference Tree for CYLSPER")
dev.off()

#cyANO
#png("C:/Users/Yusuf_Olaleye1/Downloads/Cyano_ctree.png",
#    width = 28, height = 14, units = "in", res = 600) #ADJUST width and height
#plot(Cyano_ctree, main = "Conditional Inference Tree for cyanobacteria biovolume")
#dev.off()


# Print the summary of the tree
summary(micx_ctree)
print(micx_ctree) #TO SEE THE RESULTS IN EACH NODES

#####################################################################################
##################################################################################### GAMM MODELS


names(combined_all_years_clean)
#Make-UNIQUE_ID FACTOR
combined_all_years_clean$UNIQUE_ID <-as.factor(combined_all_years_clean$UNIQUE_ID)

# Remove NA values and convert MICX to a vector
x <- as.vector(na.omit(combined_all_years_clean$MICX))
fit <- fitdist(x, distr = "gamma", method = "mle") #"lnorm" #gamma
plot(fit)



#############################plot to see distribution of predictors
# Select predictors from your main dataset
predictors_data <- combined_all_years_clean %>%
  dplyr::select(all_of(predictors_MICX)) #toxin_predictors -DEPTHLASS doent work #predictors_MICX

# Convert to long format for plotting
predictors_long <- predictors_data %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms for all predictors
#ggplot(predictors_long, aes(x = Value)) +
#  geom_histogram(bins = 30, fill = "steelblue3", color = "black", alpha = 0.7) +
#  facet_wrap(~ Variable, scales = "free") +
#  theme_minimal(base_size = 12) +
#  labs(
#    title = "Distribution of Predictor Variables (MICX Model)",
#    x = "Value",
#    y = "Frequency"
#  )

#density plot
#ggplot(predictors_long, aes(x = Value, fill = Variable)) +
#  geom_density(alpha = 0.5) +
#  facet_wrap(~ Variable, scales = "free") +
#  theme_minimal(base_size = 12) +
#  theme(legend.position = "none") +
#  labs(title = "Density Plots of Predictors")

#############################TO CREATE LOG TRANSFORMED PREDICTORS
#check names
names(combined_all_years_clean)


combined_all_years_clean_log <- combined_all_years_clean %>%
  mutate(
    log_CHLA = log10(CHLA_RESULT + 1),
    log_total_phytoplankton_biovolume = log10(total_phytoplankton_biovolume + 1),
    log_total_cyanobacteria_biovolume = log10(total_cyanobacteria_biovolume + 1),
    log_PTOX_biovolume = log10(PTOX_biovolume + 1),
    log_MIC_PTOX_biovolume = log10(PTOX_biovolume + 1),
    log_CYL_PTOX_biovolume = log10(CYL_PTOX_biovolume + 1),
    log_MICROCYSTIS = log10(MICROCYSTIS + 1),
    log_CYLINDROSPERMOPSIS = log10(CYLINDROSPERMOPSIS + 1),
    log_RAPHIDIOPSIS = log10(RAPHIDIOPSIS + 1),
    log_SECCHI = log10(SECCHI + 1),
    log_ELEVATION = log10(ELEVATION + 1),
    log_AMMONIA_N = log10(AMMONIA_N_RESULT + 1),
    log_ANC = log10(ANC_RESULT + 1),
    log_CALCIUM = log10(CALCIUM_RESULT + 1),
    log_CHLORIDE = log10(CHLORIDE_RESULT + 1),
    log_COLOR = log10(COLOR_RESULT + 1),
    log_CONDUCTIVITY = log10(COND_RESULT + 1),
    log_DOC = log10(DOC_RESULT + 1),
    log_MAGNESIUM = log10(MAGNESIUM_RESULT + 1),
    log_NTL = log10(NTL_RESULT + 1),
    log_PTL = log10(PTL_RESULT + 1),
    log_SODIUM = log10(SODIUM_RESULT + 1),
    log_TURBIDITY = log10(TURB_RESULT + 1),
    log_SULFATE = log10(SULFATE_RESULT + 1),
    log_POTASSIUM = log10(POTASSIUM_RESULT + 1),
    log_NITRATE_NITRITE = log10(NITRATE_NITRITE + 1),
    log_TN_TP_RATIO = log10(TN_TP_RATIO + 1),
    log_AREA_HA = log10(AREA_HA + 1),
    log_salinity = log10(salinity + 1),
    log_zmax = log10(zmax + 1),
    log_thermocline_depth_rLake = log10(thermocline_depth_rLake + 1)
  )

#check names
names(combined_all_years_clean_log)

############################MICX_m1_log

MICX_m1_log <- gamm(
  formula = log_MICX ~ s(log_total_cyanobacteria_biovolume) + s(Temp_top1m) + s(pH_top1m) + s(log_NTL) + s(log_PTL) +
    s(log_zmax) + s(log_ELEVATION) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + s(log_CHLORIDE) +
    s(log_SULFATE) + s(log_TURBIDITY) + s(log_DOC) + s(log_AMMONIA_N) + s(log_TN_TP_RATIO) + s(log_MAGNESIUM),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m1_log$gam)
#view LME summary
summary(MICX_m1_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m1_log, shade_color = "steelblue2") {
  plot(MICX_m1_log, shade = TRUE, shade.col = shade_color, pages = 2, main = "MICX_m1_log")}
plot_gam_custom(MICX_m1_log$gam)

#Plot LME 
plot(MICX_m1_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m1_log") #Changed pch = 19 to 10


############################MICX_m2_log

MICX_m2_log <- gamm(
  formula = log_MICX ~ s(log_CHLA) + s(log_NTL) + s(log_PTL) + s(log_TN_TP_RATIO) + s(Temp_top1m) + s(pH_top1m) 
  + s(log_DOC),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m2_log$gam)
#view LME summary
summary(MICX_m2_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m2_log, shade_color = "steelblue2") {
  plot(MICX_m2_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m2_log")}
plot_gam_custom(MICX_m2_log$gam)

#Plot LME 
plot(MICX_m2_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m2_log") #Changed pch = 19 to 10


############################MICX_m3_log

MICX_m3_log <- gamm(
  formula = log_MICX ~ s(log_total_cyanobacteria_biovolume) + s(log_NTL) + s(log_PTL) + s(log_TN_TP_RATIO) + s(pH_top1m) 
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_thermocline_depth_rLake) + s(log_salinity),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m3_log$gam)
#view LME summary
summary(MICX_m3_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m3_log, shade_color = "steelblue2") {
  plot(MICX_m3_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m3_log")}
plot_gam_custom(MICX_m3_log$gam)

#Plot LME 
plot(MICX_m3_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m3_log")


############################MICX_m4_log

MICX_m4_log <- gamm(
  formula = log_MICX ~ s(log_total_cyanobacteria_biovolume) + s(log_CHLA) + s(log_MIC_PTOX_biovolume) +  s(log_NTL) + s(log_PTL) + s(pH_top1m) 
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m4_log$gam)
#view LME summary
summary(MICX_m4_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m4_log, shade_color = "steelblue2") {
  plot(MICX_m4_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m4_log")}
plot_gam_custom(MICX_m4_log$gam)

#Plot LME 
plot(MICX_m4_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m4_log")


############################MICX_m5_log

MICX_m5_log <- gamm(
  formula = log_MICX ~ s(log_CHLA) +  s(log_NTL) + s(log_PTL) + s(pH_top1m) 
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_thermocline_depth_rLake) + s(log_salinity),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m5_log$gam)
#view LME summary
summary(MICX_m5_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m5_log, shade_color = "steelblue2") {
  plot(MICX_m5_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m5_log")}
plot_gam_custom(MICX_m5_log$gam)

#Plot LME 
plot(MICX_m5_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m5_log")


############################MICX_m5B_log
MICX_m5B_log <- gamm(
  formula = log_MICX ~ s(log_CHLA) +  s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m)   
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_thermocline_depth_rLake) + s(log_salinity),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m5B_log$gam)
#view LME summary
summary(MICX_m5B_log$lme)


############################MICX_m6_log

MICX_m6_log <- gamm(
  formula = log_MICX ~ s(log_MICROCYSTIS) +  s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m)  
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_thermocline_depth_rLake) + s(log_salinity),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m6_log$gam)
#view LME summary
summary(MICX_m6_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m6_log, shade_color = "steelblue2") {
  plot(MICX_m6_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m6_log")}   #MICX_Response_m6_model
plot_gam_custom(MICX_m6_log$gam)

#Plot LME 
plot(MICX_m6_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m6_log")



############################MICX_m7_log

MICX_m7_log <- gamm(
  formula = log_MICX ~  s(log_CHLA) + s(log_total_cyanobacteria_biovolume) + 
    s(log_MICROCYSTIS) + +s(Shannon_Index) + s(Simpson_Index) + 
    s(Evenness) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m) + 
    s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + 
    s(log_thermocline_depth_rLake) + s(log_salinity), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m7_log$gam)
#view LME summary
summary(MICX_m7_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m7_log, shade_color = "steelblue2") {
  plot(MICX_m7_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m7_log")}  
plot_gam_custom(MICX_m7_log$gam)

#Plot LME 
plot(MICX_m7_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m7_log")




############7B
MICX_m7B_log <- gamm(
  formula = log_MICX ~  s(log_CHLA) + s(log_total_cyanobacteria_biovolume) + 
    s(log_MICROCYSTIS) + +s(Shannon_Index) + s(Simpson_Index) + 
    s(Evenness) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m) + 
    s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC), #REMOVED s(log_thermocline_depth_rLake) + s(log_salinity)
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m7B_log$gam)
#view LME summary
summary(MICX_m7B_log$lme)


############7C
MICX_m7C_log <- gamm(
  formula = log_MICX ~  s(log_CHLA) + s(log_total_cyanobacteria_biovolume) + 
    s(log_MICROCYSTIS) +s(Shannon_Index) + s(Simpson_Index) + 
    s(Evenness) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m) + 
    s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_salinity), #REMOVED s(log_thermocline_depth_rLake) 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m7C_log$gam)
#view LME summary
summary(MICX_m7C_log$lme)

######7D
MICX_m7D_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) +s(Shannon_Index) + s(Simpson_Index) + #s(log_CHLA) + s(log_total_cyanobacteria_biovolume) +
    s(Evenness) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m) + 
    s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_salinity), #REMOVED s(log_thermocline_depth_rLake) 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m7D_log$gam)
#view LME summary
summary(MICX_m7D_log$lme)


######7E
MICX_m7E_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) +s(Shannon_Index) + #s(log_CHLA) + s(log_total_cyanobacteria_biovolume) + s(Simpson_Index) + 
    s(Evenness) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m) + 
    s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_salinity), #REMOVED s(log_thermocline_depth_rLake) 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m7E_log$gam)
#view LME summary
summary(MICX_m7E_log$lme)



###############################################MICX_m8_log 
#-ALL SIGNIFICANT PREDICTORS FROM M7E 
MICX_m8_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(log_TURBIDITY) + s(log_DOC),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m8_log$gam)
#view LME summary
summary(MICX_m8_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m8_log, shade_color = "steelblue2") {
  plot(MICX_m8_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m8_log")}  
plot_gam_custom(MICX_m8_log$gam)

#Plot LME 
plot(MICX_m8_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m8_log")



###############################################MICX_m9_log 
#-ALL SIGNIFICANT PREDICTORS FROM M7E AND OTHER MODEL
MICX_m9_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(log_TURBIDITY) + s(log_DOC) 
  + s(log_CALCIUM) + s(log_AMMONIA_N) + s(log_TN_TP_RATIO) + s(log_MAGNESIUM),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m9_log$gam)
#view LME summary
summary(MICX_m9_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m9_log, shade_color = "steelblue2") {
  plot(MICX_m9_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m9_log")}  
plot_gam_custom(MICX_m9_log$gam)

#Plot LME 
plot(MICX_m9_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m9_log")


####9B
MICX_m9B_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(log_TURBIDITY) + s(log_DOC)
  + s(log_CALCIUM) + s(log_AMMONIA_N) + s(log_TN_TP_RATIO) + s(log_MAGNESIUM) + s(log_SECCHI) + s(log_NITRATE_NITRITE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m9B_log$gam)
#view LME summary
summary(MICX_m9B_log$lme)

###############################################MICX_m10_log 

MICX_m10_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC)
  + s(log_AMMONIA_N)  + s(log_NITRATE_NITRITE) + s(log_MAGNESIUM) + s(log_zmax) + s(log_AREA_HA) + s(pH_top1m) + s(Temp_top1m),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m10_log$gam)
#view LME summary
summary(MICX_m10_log$lme)
#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m10_log, shade_color = "steelblue2") {
  plot(MICX_m10_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m10_log")}  
plot_gam_custom(MICX_m10_log$gam)

#Plot LME 
plot(MICX_m10_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m10_log")




###MICX_m10B_log 
MICX_m10B_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC)
  + s(log_AMMONIA_N)  + s(log_NITRATE_NITRITE) + s(log_MAGNESIUM), #+ s(log_zmax) + s(log_AREA_HA) + s(pH_top1m) + s(Temp_top1m)
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m10B_log$gam)
#view LME summary
summary(MICX_m10B_log$lme)


#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m10B_log, shade_color = "steelblue2") {
  plot(MICX_m10B_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m10B_log")}  
plot_gam_custom(MICX_m10B_log$gam)

#Plot LME 
plot(MICX_m10_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m10_log")



###############################################MICX_m11_log 

MICX_m11_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC)
  + s(log_AMMONIA_N)  + s(log_NITRATE_NITRITE) + s(log_thermocline_depth_rLake) + s(log_salinity), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m11_log$gam)
#view LME summary
summary(MICX_m11_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m11_log, shade_color = "steelblue2") {
  plot(MICX_m11_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m11_log")}  
plot_gam_custom(MICX_m11_log$gam)

#Plot LME 
plot(MICX_m11_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m11_log")



##MICX_m11B_log
MICX_m11B_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) #+ s(Shannon_Index) + s(log_NTL)
  + s(log_AMMONIA_N)  + s(log_NITRATE_NITRITE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m11B_log$gam)
#view LME summary
summary(MICX_m11B_log$lme)



##MICX_m11C_log
MICX_m11C_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) 
  + s(log_AMMONIA_N),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(MICX_m11C_log$gam)
#view LME summary
summary(MICX_m11C_log$lme)



##MICX_m11D_log
MICX_m11D_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) 
  + s(log_NITRATE_NITRITE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m11D_log$gam)
#view LME summary
summary(MICX_m11D_log$lme)



##MICX_m11E_log
MICX_m11E_log <- gamm(
  formula = log_MICX ~  s(log_CHLA ) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) + s(log_AMMONIA_N),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m11E_log$gam)
#view LME summary
summary(MICX_m11E_log$lme)




###############################################MICX_m12_log 

MICX_m12_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index)  + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) + s(log_AMMONIA_N) #+ s(log_NTL)
  + s(log_MAGNESIUM) + s(pH_top1m),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m12_log$gam)
#view LME summary
summary(MICX_m12_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m12_log, shade_color = "steelblue2") {
  plot(MICX_m12_log, shade = TRUE, shade.col = shade_color, pages = 1, main = "MICX_m12_log")}  
plot_gam_custom(MICX_m12_log$gam)

#Plot LME 
plot(MICX_m12_log$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MICX_m12_log")


#####MICX_m12B_log 
MICX_m12B_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) #+ s(log_AMMONIA_N) 
  + s(log_MAGNESIUM) + s(pH_top1m),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
summary.gam(MICX_m12B_log$gam)
#view LME summary
summary(MICX_m12B_log$lme)



###############################################MICX_m13_log (THIS IS THE TOP PERFORMING MODEL FOR MICX)
MICX_m13_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) + s(log_AMMONIA_N) 
  + s(log_MAGNESIUM) + s(pH_top1m) + s(log_CALCIUM) + s(log_NITRATE_NITRITE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m13_log$gam)
#view LME summary
summary(MICX_m13_log$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MICX_m13_log, shade_color = "steelblue2") {
  plot(MICX_m13_log, shade = TRUE, shade.col = shade_color, main = "MICX_model")} # MICX_m13_log renamed as MICX_model
plot_gam_custom(MICX_m13_log$gam)

#Plot LME 
# pch = 20 (dots smaller than 19 dots)
plot(MICX_m13_log$lme, cex = 2, pch = 20, main = "Fitted Values Vs. Standardized Residuals in MICX_model") # MICX_m13_log renamed as MICX_model 

#############################

#Function to compute a breakpoint based on data density:
get_density_break <- function(x) {
  d <- density(x, na.rm = TRUE)
  # Find where density drops to 10% of max
  threshold <- max(d$y) * 0.10
  idx <- which(d$y < threshold)[1]
  if (is.na(idx)) return(NA)
  return(d$x[idx])
}


#Extract the GAM Component
m <- MICX_m13_log$gam

#Add breakpoints to GAM smooth plots
bp_MICRO <- 8.0   # visually identified breakpoint

plot(m, select = 1, shade = TRUE, shade.col = "steelblue2", main = "s(log_MICROCYSTIS)")
abline(v = bp_MICRO, col = "red", lwd = 2, lty = 2)
text(bp_MICRO, 0, labels = paste0("Breakpoint = ", bp_MICRO),
     pos = 4, col = "red")
####plot2
bp_MICRO <- 8.0           # breakpoint on log scale
orig_MICRO_bp <- 2980.958 # original (unlogged) value

plot(MICX_m13_log$gam, select = 1, shade = TRUE, shade.col = "steelblue2",
     main = "Effect of log_MICROCYSTIS on log_MICX")

abline(v = bp_MICRO, col = "red", lwd = 2, lty = 2)

text(bp_MICRO, 0,
     labels = paste0("Data density breakpoint = ", bp_MICRO,
                     "\nNon-log biovolume value (µm³/mL)= ", format(round(orig_MICRO_bp, 2), big.mark=",")),
     pos = 4, col = "red")
#####################

# breakpoint (red)
bp_MICRO <- 8.4
orig_bp_MICRO <- (10^(bp_MICRO)) - 1
#orig_MICRO_bp <- 2980.958    #original-scale breakpoint

# New manually chosen breakpoint where data are sparse
bp_sparse <- 2.7
orig_bp1_sparse <- (10^(bp_sparse)) - 1


plot(m, select = 1, shade = TRUE, shade.col = "steelblue2",
     main = "Effect of Microcystis biovolume (µm³/mL) on Microcystins (MICX) concentrations")

# Existing RED breakpoint
abline(v = bp_MICRO, col = "red", lwd = 2, lty = 2)
text(bp_MICRO + 0.05, -0.2,
     labels = paste0("Breakpoint (log scale) = ", bp_MICRO, "\nNon-log value (µm³/mL) = ", format(round(orig_bp_MICRO, 2), big.mark=",")),
     pos = 4, col = "red")

#NEW BLUE breakpoint
abline(v = bp_sparse, col = "red", lwd = 2, lty = 2) #lty = 3
text(bp_sparse + 0.05, 0.3,
     labels = paste0("Sparse point (log scale) ~", bp_sparse,
                     "\n(orig (µm³/mL) = ", round(orig_bp1_sparse, 2), ")"),
     pos = 2, col = "red")
##############################################################################
####ntl2
bp_NTL <- 1.08               # example log-scale breakpoint
orig_bp_NTL <- (10^(bp_NTL)) - 1


plot(MICX_m13_log$gam, select = 3, shade = TRUE, shade.col = "steelblue2",
     main = "Effect of total nitrogen (NTL) on Microcystins (MICX) concentrations")

abline(v = bp_NTL, col = "red", lwd = 2, lty = 2)

text(bp_NTL, 0,
     labels = paste0("Breakpoint = ", bp_NTL,
                     "\nOriginal (non-log) value (mg/L) = ", round(orig_bp_NTL, 3)),
     pos = 4, col = "red")


################################################################################Two GAM Plots Side by Side
# Set up 1 row, 2 columns for plotting
par(mfrow = c(1, 2))

# ---- PLOT 1: MICRO

plot(m, select = 1, shade = TRUE, shade.col = "steelblue2",
     main = "Effect of Microcystis biovolume\non Microcystins (MICX)")

# Red vertical line (breakpoint 1)
abline(v = bp_MICRO, col = "red", lwd = 2, lty = 2)

text(bp_MICRO + 0.05, -0.2,
     labels = paste0("Breakpoint (log) = ", bp_MICRO,
                     "\nOriginal (µm³/mL) = ", format(round(orig_bp_MICRO, 2), big.mark=",")),
     pos = 4, col = "red")

# Second vertical line for sparse region
abline(v = bp_sparse, col = "red", lwd = 2, lty = 2)

text(bp_sparse + 0.05, 0.3,
     labels = paste0("Sparse point (log) ~", bp_sparse,
                     "\nOriginal (µm³/mL) = ", round(orig_bp1_sparse, 2)),
     pos = 4, col = "red")

# ---- PLOT 2: NTL 

plot(MICX_m13_log$gam, select = 3, shade = TRUE, shade.col = "steelblue2",
     main = "Effect of Total Nitrogen (NTL)\non Microcystins (MICX)")

# Add vertical breakpoint line
abline(v = bp_NTL, col = "red", lwd = 2, lty = 2)

text(bp_NTL, 0,
     labels = paste0("Breakpoint (log) ~ ", bp_NTL,
                     "\nOriginal (mg/L) = ", round(orig_bp_NTL, 3)),
     pos = 4, col = "red")


# Reset plotting layout to default
par(mfrow = c(1, 1))


###########################
###################################################################
##################################################################################selected, Significat MICX PREDICTOR GAM PLOT (4VAR)___FIGURE 4
#bREAK POINTS

# Your existing breakpoint (red)
bp_MICRO <- 8.4
orig_bp_MICRO <- (10^(bp_MICRO)) - 1
# breakpoint where data are sparse
bp_sparse <- 2.7
orig_bp1_sparse <- (10^(bp_sparse)) - 1
#NTL
bp_NTL <- 1.09               
orig_bp_NTL <- (10^(bp_NTL)) - 1
#MG2+
bp_MG2 <- 3
orig_bp_MG2 <- (10^(bp_MG2)) - 1
#PH
bp_PH <- 4.5
bp_PH2 <- 10

# Set up 2 row, 2 columns for plotting
par(mfrow = c(2, 2))

#PLOT 1: MICRO
plot(MICX_m13_log$gam, select = 1, shade = TRUE, shade.col = "steelblue2",
     main = "(A) Microcystis biovolume",
     ylab = "Partial effect on MICX Conc.")
# Red vertical line (breakpoint 1)
abline(v = bp_MICRO, col = "red", lwd = 2, lty = 2)
text(bp_MICRO + 0.05, -0.2,
     labels = paste0("Sparse point (log) = ", bp_MICRO,
                     "\nOriginal (µm³/mL) = ", format(round(orig_bp_MICRO, 2), big.mark=",")),
     pos = 4, col = "red")
# Second vertical line for sparse region
abline(v = bp_sparse, col = "red", lwd = 2, lty = 2)
text(bp_sparse + 0.05, 0.3,
     labels = paste0("Sparse point (log) ~", bp_sparse,
                     "\nOriginal (µm³/mL) = ", round(orig_bp1_sparse, 2)),
     pos = 4, col = "red")

#PLOT 2: NTL 
plot(MICX_m13_log$gam, select = 3, shade = TRUE, shade.col = "steelblue2",
     main = "(B) Total Nitrogen (NTL)",
     ylab = "Partial effect on MICX Conc.")
abline(v = bp_NTL, col = "red", lwd = 2, lty = 2)
text(bp_NTL, 0,
     labels = paste0("Sparse point (log) ~ ", bp_NTL,
                     "\nOriginal (mg/L) = ", round(orig_bp_NTL, 3)),
     pos = 4, col = "red")


#PLOT 3: MAGNESIUM
plot(MICX_m13_log$gam, select = 8, shade = TRUE, shade.col = "steelblue2",
     main = "(C) Magnesium (Mg2+)",
     ylab = "Partial effect on MICX Conc.")
abline(v = bp_MG2, col = "red", lwd = 2, lty = 2)
text(bp_MG2 + 1.05, -0.1,
     labels = paste0("Sparse point (log) ~ ", bp_MG2,
                     "\nOriginal (mg/L) = ", round(orig_bp_MG2, 3)),
     pos = 4, col = "red")

#PLOT 4: pH
plot(MICX_m13_log$gam, select = 9, shade = TRUE, shade.col = "steelblue2",
     main = "(D) Surface water pH",
     ylab = "Partial effect on MICX Conc.")
abline(v = bp_PH, col = "red", lwd = 2, lty = 2)
text(bp_PH + 0.05, -0.1,
     labels = paste0("Sparse point ~ ", bp_PH),
     pos = 4, col = "red")
abline(v = bp_PH2, col = "red", lwd = 2, lty = 2)
text(bp_PH2, 0.1,
     labels = paste0("Sparse point ~ ", bp_PH2),
     pos = 2, col = "red")



# Reset plotting layout to default
par(mfrow = c(1, 1))


############################################################################


###m13B
MICX_m13B_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(log_NTL) + s(log_PTL),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m13B_log$gam)
#view LME summary
summary(MICX_m13B_log$lme)

###m13C
MICX_m13C_log <- gamm(
  formula = log_MICX ~  s(log_MICROCYSTIS) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) + s(pH_top1m), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(MICX_m13C_log$gam)
#view LME summary
summary(MICX_m13C_log$lme)

#####################################################################################################################GAMM FOR CYLSPER

############################Model_CYL1

Model_CYL1 <- gamm(
  formula = log_CYLSPER ~ s(log_total_cyanobacteria_biovolume) + s(Temp_top1m) + s(pH_top1m) + s(log_NTL) + s(log_PTL) +
    s(log_zmax) + s(log_ELEVATION) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + s(log_CHLORIDE) +
    s(log_SULFATE) + s(log_TURBIDITY) + s(log_DOC) + s(log_AMMONIA_N) + s(log_TN_TP_RATIO) + s(log_MAGNESIUM),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL1$gam)
#view LME summary
summary(Model_CYL1$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(Model_CYL1, shade_color = "pink") {
  plot(Model_CYL1, shade = TRUE, shade.col = shade_color, pages = 1, main = "Model_CYL1")}
plot_gam_custom(Model_CYL1$gam)

#Plot LME 
plot(Model_CYL1$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in Model_CYL1")




#####Model_CYL1B
Model_CYL1B <- gamm(
  formula = log_CYLSPER ~ s(log_total_cyanobacteria_biovolume) + s(Temp_top1m) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + 
    s(log_DOC) + s(log_SECCHI) + s(log_NITRATE_NITRITE) + s(log_AMMONIA_N) + s(log_SULFATE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL1B$gam)
#view LME summary
summary(Model_CYL1B$lme)




#####Model_CYL1C
Model_CYL1C <- gamm(
  formula = log_CYLSPER ~ s(log_total_cyanobacteria_biovolume) + s(Temp_top1m) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + 
    s(log_DOC) + s(log_SULFATE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL1C$gam)
#view LME summary
summary(Model_CYL1C$lme)


#####Model_CYL1_D
Model_CYL1_D <- gamm(
  formula = log_CYLSPER ~ s(log_CHLA) + s(Temp_top1m) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + 
    s(log_DOC) + s(log_SULFATE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL1_D$gam)
#view LME summary
summary(Model_CYL1_D$lme)


#####Model_CYL1_E
Model_CYL1_E <- gamm(
  formula = log_CYLSPER ~ s(log_CYL_PTOX_biovolume) + s(Temp_top1m) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + 
    s(log_DOC) + s(log_SULFATE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL1_E$gam)
#view LME summary
summary(Model_CYL1_E$lme)


#####Model_CYL1_F
Model_CYL1_F <- gamm(
  formula = log_CYLSPER ~ s(log_CYLINDROSPERMOPSIS) + s(Temp_top1m) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + 
    s(log_DOC) + s(log_SULFATE),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL1_F$gam)
#view LME summary
summary(Model_CYL1_F$lme)


#####Model_CYL1_G
Model_CYL1_G <- gamm(
  formula = log_CYLSPER ~ s(log_CYL_PTOX_biovolume) + s(Temp_top1m) + s(log_CALCIUM) + s(Evenness) + 
    s(log_SULFATE) + s(log_TN_TP_RATIO),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL1_G$gam)
#view LME summary
summary(Model_CYL1_G$lme)


#####Model_CYL1_H
Model_CYL1_H <- gamm(
  formula = log_CYLSPER ~ s(log_CYL_PTOX_biovolume) + s(Temp_top1m) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + 
    s(log_SULFATE) + s(log_TN_TP_RATIO), #s(log_DOC) + 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL1_H$gam)
#view LME summary
summary(Model_CYL1_H$lme)


############################Model_CYL2

Model_CYL2 <- gamm(
  formula = log_CYLSPER ~ s(log_CHLA) + s(log_NTL) + s(log_PTL) + s(log_TN_TP_RATIO) + s(Temp_top1m) + s(pH_top1m) 
  + s(log_DOC),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL2$gam)
#view LME summary
summary(Model_CYL2$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(Model_CYL2, shade_color = "pink") {
  plot(Model_CYL2, shade = TRUE, shade.col = shade_color, pages = 1, main = "Model_CYL2")}
plot_gam_custom(Model_CYL2$gam)

#Plot LME 
plot(Model_CYL2$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in Model_CYL2")

############################Model_CYL3

Model_CYL3 <- gamm(
  formula = log_CYLSPER ~ s(log_total_cyanobacteria_biovolume) + s(log_NTL) + s(log_PTL) + s(log_TN_TP_RATIO) + s(pH_top1m) 
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_thermocline_depth_rLake) + s(log_salinity),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(Model_CYL3$gam)
#view LME summary
summary(Model_CYL3$lme)


############################Model_CYL4


Model_CYL4 <- gamm(
  formula = log_CYLSPER ~ s(log_total_cyanobacteria_biovolume) + s(log_CHLA) + s(log_MIC_PTOX_biovolume) +  s(log_NTL) + s(log_PTL) + s(pH_top1m) 
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL4$gam)
#view LME summary
summary(Model_CYL4$lme)


############################Model_CYL5

Model_CYL5 <- gamm(
  formula = log_CYLSPER ~ s(log_CHLA) +  s(log_NTL) + s(log_PTL) + s(pH_top1m) 
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_thermocline_depth_rLake) + s(log_salinity),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL5$gam)
#view LME summary
summary(Model_CYL5$lme)


############################Model_CYL6

Model_CYL6 <- gamm(
  formula = log_CYLSPER ~ s(log_CYLINDROSPERMOPSIS) +  s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m)  
  + s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + s(log_thermocline_depth_rLake) + s(log_salinity),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL6$gam)
#view LME summary
summary(Model_CYL6$lme)


############################Model_CYL7

Model_CYL7 <- gamm(
  formula = log_CYLSPER ~ s(log_CYLINDROSPERMOPSIS) + s(Shannon_Index) + 
    s(Evenness) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m) + 
    s(log_TURBIDITY) + s(log_zmax) + s(log_ELEVATION) + s(log_DOC) + 
    s(log_salinity), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL7$gam)
#view LME summary
summary(Model_CYL7$lme)

############################Model_CYL8
#-ALL SIGNIFICANT PREDICTORS FROM cyl7
Model_CYL8 <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(Evenness) + s(log_PTL) + s(Temp_top1m) + s(log_DOC) + s(log_salinity), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL8$gam)
#view LME summary
summary(Model_CYL8$lme)



############################Model_CYL9
#-ALL SIGNIFICANT PREDICTORS FROM cyl7 AND OTHER MODEL (9A as used for MICX and 9B specfic for CYLSPER)
Model_CYL9 <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(log_TURBIDITY) + s(log_DOC) 
  + s(log_CALCIUM) + s(log_AMMONIA_N) + s(log_TN_TP_RATIO) + s(log_MAGNESIUM),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL9$gam)
#view LME summary
summary(Model_CYL9$lme)


###Model_CYL9B
Model_CYL9B <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(Shannon_Index) + s(Evenness) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m) + s(log_DOC) 
  + s(log_CALCIUM) + s(log_zmax) + s(log_ELEVATION), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL9B$gam)
#view LME summary
summary(Model_CYL9B$lme)


###Model_CYL9C
Model_CYL9C <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(Evenness) + s(log_PTL) + s(Temp_top1m) + s(log_DOC) 
  + s(log_CALCIUM), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL9C$gam)
#view LME summary
summary(Model_CYL9C$lme)



######Model_CYL9D
Model_CYL9D <- gamm(
  formula = log_CYLSPER ~  s(log_CHLA) + s(log_CYLINDROSPERMOPSIS) + s(Evenness) + s(log_NTL) + s(log_PTL) + s(pH_top1m) + s(Temp_top1m) + s(log_DOC) 
  + s(log_CALCIUM) + s(log_zmax) + s(log_ELEVATION), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL9D$gam)
#view LME summary
summary(Model_CYL9D$lme)


############################Model_CYL_10

Model_CYL_10 <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC)
  + s(log_AMMONIA_N)  + s(log_NITRATE_NITRITE) + s(log_MAGNESIUM) + s(log_zmax) + s(log_AREA_HA) + s(pH_top1m) + s(Temp_top1m),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_10$gam)
#view LME summary
summary(Model_CYL_10$lme)

#####Model_CYL_10B
Model_CYL_10B <- gamm(
  formula = log_CYLSPER ~ s(log_CYLINDROSPERMOPSIS) + s(Shannon_Index) + s(Evenness) + s(log_PTL) + s(Temp_top1m) + s(log_DOC) 
  + s(log_CALCIUM)+ s(log_MAGNESIUM) + s(log_AREA_HA),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_10B$gam)
#view LME summary
summary(Model_CYL_10B$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(Model_CYL_10B, shade_color = "pink") {
  plot(Model_CYL_10B, shade = TRUE, shade.col = shade_color, pages = 1, main = "Model_CYL_10B")}
plot_gam_custom(Model_CYL_10B$gam)
############################Model_CYL_11

Model_CYL_11 <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC)
  + s(log_AMMONIA_N)  + s(log_NITRATE_NITRITE) + s(log_thermocline_depth_rLake) + s(log_salinity), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_11$gam)
#view LME summary
summary(Model_CYL_11$lme)


####Model_CYL_11B
Model_CYL_11B <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(Shannon_Index) + s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC)
  + s(log_AMMONIA_N)  + s(log_NITRATE_NITRITE), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_11B$gam)
#view LME summary
summary(Model_CYL_11B$lme)


####Model_CYL_11C
Model_CYL_11C <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(Shannon_Index) + s(log_PTL) + s(log_TURBIDITY), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_11C$gam)
#view LME summary
summary(Model_CYL_11C$lme)




############################Model_CYL_12


Model_CYL_12 <- gamm(
  formula = log_CYLSPER ~  s(log_CYLINDROSPERMOPSIS) + s(log_PTL) + s(log_TURBIDITY) + s(log_DOC) + s(log_AMMONIA_N) 
  + s(log_MAGNESIUM) + s(pH_top1m),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_12$gam)
#view LME summary
summary(Model_CYL_12$lme)




############################Model_CYL_13
###Model_CYL_13
Model_CYL_13 <- gamm(
  formula = log_CYLSPER ~ s(log_CYLINDROSPERMOPSIS) + s(Temp_top1m) + s(log_PTL) + 
    s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) +
    s(log_SULFATE)  + s(log_DOC),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_13$gam)
#view LME summary
summary(Model_CYL_13$lme)


#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(Model_CYL_13, shade_color = "pink") {
  plot(Model_CYL_13, shade = TRUE, shade.col = shade_color, pages = 1, main = "Model_CYL_13")}
plot_gam_custom(Model_CYL_13$gam)

#Plot LME 
plot(Model_CYL_13$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in Model_CYL_13")



####Model_CYL_13B
Model_CYL_13B <- gamm(
  formula = log_CYLSPER ~ s(log_CYLINDROSPERMOPSIS) + s(Temp_top1m) + s(pH_top1m) + s(log_NTL) + s(log_PTL) +
    s(log_zmax) + s(log_ELEVATION) + s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + s(log_CHLORIDE) +
    s(log_SULFATE) + s(log_TURBIDITY) + s(log_DOC) + s(log_AMMONIA_N) + s(log_MAGNESIUM),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_13B$gam)
#view LME summary
summary(Model_CYL_13B$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(Model_CYL_13B, shade_color = "pink") {
  plot(Model_CYL_13B, shade = TRUE, shade.col = shade_color, main = "CYLSPER_model")} # Model_CYL_13B renamed CYLSPER_model
plot_gam_custom(Model_CYL_13B$gam)

#Plot LME 
plot(Model_CYL_13B$lme, cex = 2, pch = 20, main = "Fitted Values Vs. Standardized Residuals in CYLSPER_model") # Model_CYL_13B renamed CYLSPER_model

#Plot LME with pink 
plot(
  Model_CYL_13B$lme,
  cex = 2,
  pch = 20,
  col = "pink",
  main = "Fitted Values vs. Standardized Residuals in CYLSPER_model"
)

############################
#Extract the GAM Component
cyl_m <-  Model_CYL_13B$gam


#Add breakpoints to GAM smooth plots
bp_CYLINDRO <- 7.7   # visually identified breakpoint

plot(cyl_m, select = 1, shade = TRUE, shade.col = "pink", main = "cylindrospermopsis")
abline(v = bp_CYLINDRO, col = "red", lwd = 2, lty = 2)
text(bp_CYLINDRO, 0, labels = paste0("Breakpoint = ", bp_CYLINDRO),
     pos = 4, col = "red")

#####################

# Your existing breakpoint (red)
bp_CYLINDRO <- 7.7
orig_CYLINDRO_bp <- (10^(bp_CYLINDRO)) - 1

#orig_CYLINDRO <- exp(bp_CYLINDRO)    # your original-scale breakpoint

# New manually chosen breakpoint where data are sparse
bpc_sparse <- 3.1
orig_bpc_sparse <- (10^(bpc_sparse)) - 1 #for log10 back transform 

#orig_sparse_bpc <- exp(bpc_sparse)   # for natural log

plot(cyl_m, select = 1, shade = TRUE, shade.col = "pink",
     main = "Effect of Cylindrospermopsis biovolume (µm³/mL) on cylindrospermopsins (CYLSPER)")

# ---- Existing RED breakpoint ----
abline(v = bp_CYLINDRO, col = "red", lwd = 2, lty = 2)
text(bp_CYLINDRO, -0.01,
     labels = paste0("Breakpoint (log scale) = ", bp_CYLINDRO, "\nNon-log value (µm³/mL) = ", format(round(orig_CYLINDRO_bp, 2), big.mark=",")),
     pos = 4, col = "red")

# ---- NEW BLUE breakpoint ----
abline(v = bpc_sparse, col = "red", lwd = 2, lty = 2) #lty = 3
text(bpc_sparse, 0.01,
     labels = paste0("Sparse point (log scale) ~", bpc_sparse,
                     "\n(orig (µm³/mL) = ", round(orig_bpc_sparse, 2), ")"),
     pos = 2, col = "red")
##############################################################################
####CALCIUM 

bp_CALCIUM <- 2.4               # example log-scale breakpoint
orig_bp_CALCIUM <- (10^(bp_CALCIUM)) - 1
#orig_CALCIUM_bp <- exp(bp_CALCIUM)

plot(Model_CYL_13B$gam, select = 9, shade = TRUE, shade.col = "pink",
     main = "Effect of CALCIUM on cylindrospermopsis (CYLSPER) concentrations")

abline(v = bp_CALCIUM, col = "blue", lwd = 2, lty = 2)

text(bp_CALCIUM, -0.02,
     labels = paste0("Breakpoint = ", bp_CALCIUM,
                     "\nOriginal (non-log) value (mg/L) = ", round(orig_bp_CALCIUM, 3)),
     pos = 4, col = "red")

########################################AXIS LABEL ADDED
# Set font and text size
par(family = "Times New Roman", 
    cex.lab = 1.6,       # axis label size
    font.lab = 2,        # axis label bold
    cex.axis = 1.3,      # tick label size
    font.axis = 2)       # tick label regular #good label bolder changed from 1 to 2

bp_CALCIUM <- 2.4
orig_bp_CALCIUM <- (10^(bp_CALCIUM)) - 1

plot(Model_CYL_13B$gam, select = 9, shade = TRUE, shade.col = "pink",
     xlab = "log(CALCIUM)", 
     ylab = "Partial effect on log_CYLSPER",
     main = "Effect of CALCIUM on cylindrospermopsins (CYLSPER) concentrations")

abline(v = bp_CALCIUM, col = "blue", lwd = 2, lty = 2)

text(bp_CALCIUM, -0.02,
     labels = paste0("Breakpoint = ", bp_CALCIUM,
                     "\nOriginal (mg/L) = ", round(orig_bp_CALCIUM, 3)),
     pos = 4, col = "red")


#Restore Defaults
par(family = "", cex.lab = 1, font.lab = 1, cex.axis = 1, font.axis = 1)

################################################################################Two GAM Plots Side by Side
# Set up 1 row, 2 columns for plotting
par(mfrow = c(1, 2))

# ---- PLOT 1: CYLSPER

plot(cyl_m, select = 1, shade = TRUE, shade.col = "pink",
     main = "Effect of Cylindrospermopsis biovolume (µm³/mL) on cylindrospermopsins (CYLSPER)")
# first line
abline(v = bp_CYLINDRO, col = "red", lwd = 2, lty = 2)
text(bp_CYLINDRO, -0.01,
     labels = paste0("Breakpoint (log scale) = ", bp_CYLINDRO, "\nNon-log value (µm³/mL) = ", format(round(orig_CYLINDRO_bp, 2), big.mark=",")),
     pos = 4, col = "red")

# second line
abline(v = bpc_sparse, col = "red", lwd = 2, lty = 2) #lty = 3
text(bpc_sparse, 0.01,
     labels = paste0("Sparse point (log scale) ~", bpc_sparse,
                     "\n(orig (µm³/mL) = ", round(orig_bpc_sparse, 2), ")"),
     pos = 2, col = "red")

# ---- PLOT 2: Calcium

plot(Model_CYL_13B$gam, select = 9, shade = TRUE, shade.col = "pink",
     main = "Effect of CALCIUM on cylindrospermopsins (CYLSPER)")

abline(v = bp_CALCIUM, col = "blue", lwd = 2, lty = 2)

text(bp_CALCIUM, -0.04,
     labels = paste0("Breakpoint = ", bp_CALCIUM,
                     "\nNon-log value (mg/L) = ", round(orig_bp_CALCIUM, 3)),
     pos = 2, col = "red")

# Reset plotting layout to default
par(mfrow = c(1, 1))

##################################################################################selected, Significat CYL PREDICTOR GAM PLOT___FIGURE5
#bREAK POINTS

# CYLINDRO existing breakpoint (red)
bp_CYLINDRO <- 7.7
orig_CYLINDRO_bp <- (10^(bp_CYLINDRO)) - 1

# CYLINDRO breakpoint where data are sparse
bpc_sparse <- 3.1
orig_bpc_sparse <- (10^(bpc_sparse)) - 1 #for log10 back transform 

#CALCIUM
bp_CALCIUM <- 2.4
orig_bp_CALCIUM <- (10^(bp_CALCIUM)) - 1

#NTL
bp_NTLa <- 1
orig_bp_NTLa <- (10^(bp_NTLa)) - 1


#TEMP
bp_TEMP <- 8.5
orig_bp_TEMP <- (10^(bp_TEMP)) - 1



# Set up 2 row, 2 columns for plotting
#par(mfrow = c(2, 2), oma = c(0, 0, 2, 2))  # outer margin
par(mfrow = c(2, 2))

#PLOT 1: CYLSPER

plot(cyl_m, select = 1, shade = TRUE, shade.col = "pink",
     main = "(A) Cylindrospermopsis Biovolume",
     ylab = "Partial effect on CYLSPER Conc.")
# first line
abline(v = bp_CYLINDRO, col = "red", lwd = 2, lty = 2)
text(bp_CYLINDRO, -0.01,
     labels = paste0("Sparse point (log scale) = ", bp_CYLINDRO, "\nNon-log value (µm³/mL) = ", format(round(orig_CYLINDRO_bp, 2), big.mark=",")),
     pos = 4, col = "red")

# second line
abline(v = bpc_sparse, col = "red", lwd = 2, lty = 2) #lty = 3
text(bpc_sparse, -0.01,
     labels = paste0("Sparse point (log scale) ~", bpc_sparse,
                     "\n(orig (µm³/mL) = ", round(orig_bpc_sparse, 2), ")"),
     pos = 2, col = "red")

########PLOT 2: Total Nitrogen

plot(Model_CYL_13B$gam, select = 4, shade = TRUE, shade.col = "pink",
     main = "(B) Total Nitrogen (NTL)",
     ylab = "Partial effect on CYLSPER Conc.")

abline(v = bp_NTLa, col = "red", lwd = 2, lty = 2)

text(bp_NTLa, -0.04,
     labels = paste0("Sparse point = ", bp_NTLa,
                     "\nNon-log value (mg/L) = ", round(orig_bp_NTLa, 3)),
     pos = 2, col = "red")

######PLOT 3: Calcium

plot(Model_CYL_13B$gam, select = 9, shade = TRUE, shade.col = "pink",
     main = "(C) Calcium (Ca2+)",
     ylab = "Partial effect on CYLSPER Conc.")

abline(v = bp_CALCIUM, col = "blue", lwd = 2, lty = 2)

text(bp_CALCIUM, -0.04,
     labels = paste0("Breakpoint = ", bp_CALCIUM,
                     "\nNon-log value (mg/L) = ", round(orig_bp_CALCIUM, 3)),
     pos = 2, col = "red")


######PLOT 4: Temperature

plot(Model_CYL_13B$gam, select = 2, shade = TRUE, shade.col = "pink",
     main = "(D) Surface water temperature", ####Effect of Temperature on cylindrospermopsins (CYLSPER)
     ylab = "Partial effect on CYLSPER Conc.")

abline(v = bp_TEMP, col = "red", lwd = 2, lty = 2)

text(bp_TEMP, -0.04,
     labels = paste0("Sparse point = ", bp_TEMP),
     pos = 2, col = "red")



# Reset plotting layout to default
par(mfrow = c(1, 1))


########################
#####################################################

####Model_CYL_13C
Model_CYL_13C <- gamm(
  formula = log_CYLSPER ~ s(log_CYLINDROSPERMOPSIS) + s(Temp_top1m) + s(log_NTL) + s(log_PTL) +
    s(log_AREA_HA) + s(log_CALCIUM) + s(Evenness) + s(log_DOC),
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 
#View GAM summary
summary.gam(Model_CYL_13C$gam)
#view LME summary
summary(Model_CYL_13C$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(Model_CYL_13C, shade_color = "pink") {
  plot(Model_CYL_13C, shade = TRUE, shade.col = shade_color, pages = 1, main = "Model_CYL_13C")}
plot_gam_custom(Model_CYL_13C$gam)





#########
#################################
names(combined_all_years_clean_log)
#log_CHLA
#log_total_phytoplankton_biovolume
#log_total_cyanobacteria_biovolume
#log_CYL_PTOX_biovolume
#log_CYLINDROSPERMOPSIS
#log_RAPHIDIOPSIS
#log_PTOX_biovolume
#log_MICROCYSTIS
#log_MIC_PTOX_biovolume
################################

#  log_CHLA
#  log_total_cyanobacteria_biovolume
#  log_MICROCYSTIS
#  log_MIC_PTOX_biovolume
#  log_CYLINDROSPERMOPSIS
#  log_CYL_PTOX_biovolume

#  log_PTOX_biovolume
############################################################GAMM FOR CYANO BIOVOLUME
############################CYANO_BIOV_9

#CYANO_BIOV_9
# CYANO_BIOV_9_cyano
# Micro_BIOV_9 
#Cylindro_BIOV_9 


CYANO_BIOV_9a <- gamm(
  formula = log_CHLA ~ s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + 
    s(log_zmax) + s(Temp_top1m) + s(log_AMMONIA_N) + s(log_NITRATE_NITRITE) + s(log_ELEVATION) + s(log_AREA_HA), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

CYANO_BIOV_9b <- gamm(
  formula = log_total_cyanobacteria_biovolume ~ s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + 
    s(log_zmax) + s(Temp_top1m) + s(log_AMMONIA_N) + s(log_NITRATE_NITRITE) + s(log_ELEVATION) + s(log_AREA_HA), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

CYANO_BIOV_9c <- gamm(
  formula = log_MICROCYSTIS ~ s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + 
    s(log_zmax) + s(Temp_top1m) + s(log_AMMONIA_N) + s(log_NITRATE_NITRITE) + s(log_ELEVATION) + s(log_AREA_HA), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

CYANO_BIOV_9d <- gamm(
  formula = log_CYLINDROSPERMOPSIS ~ s(log_NTL) + s(log_PTL) + s(log_TURBIDITY) + 
    s(log_zmax) + s(Temp_top1m) + s(log_AMMONIA_N) + s(log_NITRATE_NITRITE) + s(log_ELEVATION) + s(log_AREA_HA), 
  random = list(UNIQUE_ID = ~1),
  family = gaussian(),  
  data = combined_all_years_clean_log,
  niterPQL = 100) 

#View GAM summary
summary.gam(CYANO_BIOV_9a$gam)
#view LME summary
summary(CYANO_BIOV_9a$lme)

#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(CYANO_BIOV_9a, shade_color = "skyblue") {
  plot(CYANO_BIOV_9a, shade = TRUE, shade.col = shade_color, pages = 1, main = "CYANO_BIOV_9a")}   
plot_gam_custom(CYANO_BIOV_9a$gam)

#Plot LME 
plot(CYANO_BIOV_9b$lme, cex = 2, pch = 20, main = "Fitted Values Vs. Standardized Residuals in CYANO_BIOV_9b")




####################################################################################
#####################CHLA
# 1. Predicted log-values
predicted_chla <- predict(CYANO_BIOV_9a$gam, type = "response")
# 2. Extract observed log-values from the model's internal data
observed_chla <- CYANO_BIOV_9a$gam$model$log_CHLA

##GGPLOT
df2 <- data.frame(
  observed_chla = observed_chla,
  predicted_chla = predicted_chla)

#PLOT
# Calculate R²
R2 <- cor(df2$observed_chla, df2$predicted_chla)^2

plot_chla <- ggplot(df2, aes(x = observed_chla, y = predicted_chla)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "red",lwd = 1.2) +
  geom_smooth(method = "lm", 
              color = "blue", se = FALSE, linewidth = 1.2) +
  annotate("text", 
           x = min(df2$observed_chla), 
           y = max(df2$predicted_chla), 
           label = paste0("R² = ", round(R2, 3)),
           hjust = 0, vjust = 2,
           size = 5) +
  labs(
    x = "observed_chla",
    y = "predicted_chla",
    title = "(A). predicted_chla vs observed_chla"
  ) +
  theme_bw()
#check r2 again: Calculate R² between predicted and observed
cor(df2$observed_chla, df2$predicted_chla)^2

#####################Cyanobacteria_biovolume
# 1. Predicted log-values
predicted_Cyanobacteria_biovolume <- predict(CYANO_BIOV_9b$gam, type = "response")
# 2. Extract observed log-values from the model's internal data
observed_Cyanobacteria_biovolume <- CYANO_BIOV_9b$gam$model$log_total_cyanobacteria_biovolume

##GGPLOT
df3 <- data.frame(
  observed_Cyanobacteria_biovolume = observed_Cyanobacteria_biovolume,
  predicted_Cyanobacteria_biovolume = predicted_Cyanobacteria_biovolume)

#PLOT
# Calculate R²
C_R2 <- cor(df3$observed_Cyanobacteria_biovolume, df3$predicted_Cyanobacteria_biovolume)^2

ggplot(df3, aes(x = observed_Cyanobacteria_biovolume, y = predicted_Cyanobacteria_biovolume)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.2) +
  annotate("text", 
           x = min(df3$observed_Cyanobacteria_biovolume), 
           y = max(df3$predicted_Cyanobacteria_biovolume), 
           label = paste0("R² = ", round(C_R2, 3)),
           hjust = 0, vjust = 1,
           size = 5) +
  labs(
    x = "observed_Cyanobacteria_biovolume",
    y = "predicted_Cyanobacteria_biovolume",
    title = "predicted_Cyanobacteria_biovolume vs observed_Cyanobacteria_biovolume"
  ) +
  theme_bw()
#check r2 again: Calculate R² between predicted and observed
cor(df3$observed_Cyanobacteria_biovolume, df3$predicted_Cyanobacteria_biovolume)^2

#####

# Plot with R² annotation
plot_cyano <-ggplot(df3, aes(x = observed_Cyanobacteria_biovolume, y = predicted_Cyanobacteria_biovolume)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.2) +
  geom_smooth(method = "lm", 
              color = "blue", se = FALSE, linewidth = 1.2) +
  annotate("text", 
           x = min(df3$observed_Cyanobacteria_biovolume), 
           y = max(df3$predicted_Cyanobacteria_biovolume), 
           label = paste0("R² = ", round(C_R2, 3)),
           hjust = 0, vjust = 1,
           size = 5) +
  labs(
    x = "Observed log Cyanobacteria Biovolume",
    y = "Predicted log Cyanobacteria Biovolume",
    title = "(B). Predicted vs Observed: Cyanobacteria Biovolume"
  ) +
  theme_bw()

#####################Microcystis_biovolume
# 1. Predicted log-values
predicted_Microcystis_biovolume <- predict(CYANO_BIOV_9c$gam, type = "response")
# 2. Extract observed log-values from the model's internal data
observed_Microcystis_biovolume <- CYANO_BIOV_9c$gam$model$log_MICROCYSTIS

##GGPLOT
df4 <- data.frame(
  observed_Microcystis_biovolume = observed_Microcystis_biovolume,
  predicted_Microcystis_biovolume = predicted_Microcystis_biovolume)

#PLOT
ggplot(df4, aes(x = observed_Microcystis_biovolume, y = predicted_Microcystis_biovolume)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.2) +
  labs(
    x = "observed_Microcystis_biovolume",
    y = "predicted_Microcystis_biovolume",
    title = "predicted_Microcystis_biovolume vs observed_Microcystis_biovolume"
  ) +
  theme_bw()
#check r2 again: Calculate R² between predicted and observed
Micro_R2 <- cor(df4$observed_Microcystis_biovolume, df4$predicted_Microcystis_biovolume)^2


#PLOT--iMPROVED
plot_micro <- ggplot(df4, aes(x = observed_Microcystis_biovolume, y = predicted_Microcystis_biovolume)) +
  geom_point(alpha = 0.6) +
  # 1:1 line (perfect prediction)
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.2) + #removed linetype = "dashed",
  # Smooth GAMM line
  geom_smooth(method = "lm", color = "blue", se = FALSE, size = 1.2) +
  annotate("text", 
           x = min(df4$observed_Microcystis_biovolume), 
           y = max(df4$predicted_Microcystis_biovolume), 
           label = paste0("R² = ", round(Micro_R2, 3)),
           hjust = -1, vjust = 2,
           size = 5) +
  labs(x = "observed_Microcystis_biovolume (log scale)", 
       y = "predicted_Microcystis_biovolume (log scale)",
       title = "(C). predicted_Microcystis_biovolume vs observed_Microcystis_biovolume") +
  theme_bw()



#####################Cylindrospermopsis_biovolume
# 1. Predicted log-values
predicted_Cylindrospermopsis_biovolume <- predict(CYANO_BIOV_9d$gam, type = "response")
# 2. Extract observed log-values from the model's internal data
observed_Cylindrospermopsis_biovolume <- CYANO_BIOV_9d$gam$model$log_CYLINDROSPERMOPSIS

##GGPLOT
df5 <- data.frame(
  observed_Cylindrospermopsis_biovolume = observed_Cylindrospermopsis_biovolume,
  predicted_Cylindrospermopsis_biovolume = predicted_Cylindrospermopsis_biovolume)

#check r2 again: Calculate R² between predicted and observed
Cylindro_R2 <- cor(df5$observed_Cylindrospermopsis_biovolume, df5$predicted_Cylindrospermopsis_biovolume)^2

#PLOT
plot_cylindro <- ggplot(df5, aes(x = observed_Cylindrospermopsis_biovolume, y = predicted_Cylindrospermopsis_biovolume)) +
  geom_point(alpha = 0.6) +
  # 1:1 line (perfect prediction)
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.2) + #removed linetype = "dashed",
  # Smooth GAMM line
  geom_smooth(method = "lm", color = "blue", se = FALSE, size = 1.2) +
  annotate("text", 
           x = min(df5$observed_Cylindrospermopsis_biovolume), 
           y = max(df5$predicted_Cylindrospermopsis_biovolume), 
           label = paste0("R² = ", round(Cylindro_R2, 3)),
           hjust = -0.5, vjust = 2,
           size = 5) +
  labs(x = "observed_Cylindrospermopsis_biovolume (log scale)", 
       y = "predicted_Cylindrospermopsis_biovolume (log scale)",
       title = "(D). Predicted_Cylindrospermopsis_biovolume vs observed_Cylindrospermopsis_biovolume") +
  theme_bw()



####Combine plot
# Combine plots
combine_nplot <- plot_chla + plot_cyano + plot_micro + plot_cylindro + plot_layout(ncol = 2)

# Show the plot
print(combine_nplot)

# save
ggsave("cOMBINED_CHLA_CYANO_PREDICT.png", combine_nplot, width = 18, height = 12, dpi = 500)

################TOXINS PREDICT VS OBSERVED
##########################MICX
######predic vs OBSERVE
#NAMES
names(MICX_m13_log$gam$model)

# 1. Predicted log-values
predicted_micx <- predict(MICX_m13_log$gam, type = "response")

# 2. Extract observed log-values from the model's internal data
observed_micx <- MICX_m13_log$gam$model$log_MICX

#ggplot2 
df <- data.frame(
  observed_micx = observed_micx,
  predicted_micx = predicted_micx)

#check r2 again: Calculate R² between predicted and observed
MICX_R2 <- cor(df$observed_micx, df$predicted_micx)^2

#plot
plot_micx <- ggplot(df, aes(x = observed_micx, y = predicted_micx)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.2) +
  geom_smooth(method = "gam", color = "blue", se = FALSE, size = 1.2) +
  annotate("text", 
           x = min(df$observed_micx), 
           y = max(df$predicted_micx), 
           label = paste0("R² = ", round(MICX_R2, 3)),
           hjust = 0, vjust = 2,
           size = 5) +
  labs(
    x = "Observed log(MICX)",
    y = "Predicted log(MICX)",
    title = "(A). Predicted MICX vs Observed MICX with 1:1 Line and GAM Smooth"
  ) +
  theme_bw()
#view
plot_micx

##########################CYLSPER
######predic vs OBSERVE
#NAMES
names(Model_CYL_13B$gam$model)

# 1. Predicted log-values
predicted_cylsper <- predict(Model_CYL_13B$gam, type = "response")

# 2. Extract observed log-values from the model's internal data
observed_cylsper <- Model_CYL_13B$gam$model$log_CYLSPER

#ggplot2 
df6 <- data.frame(
  observed_cylsper = observed_cylsper,
  predicted_cylsper = predicted_cylsper)

#check r2 again: Calculate R² between predicted and observed
CYLSPER_R2 <- cor(df6$observed_cylsper, df6$predicted_cylsper)^2

#plot
plot_cyl <- ggplot(df6, aes(x = observed_cylsper, y = predicted_cylsper)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.2) +
  geom_smooth(method = "gam", color = "blue", se = FALSE, size = 1.2) +
  annotate("text", 
           x = min(df6$observed_cylsper), 
           y = max(df6$predicted_cylsper), 
           label = paste0("R² = ", round(CYLSPER_R2, 3)),
           hjust = -1.5, vjust = 2,
           size = 5) +
  labs(
    x = "Observed log(CYLSPER)",
    y = "Predicted log(CYLSPER)",
    title = "(B). Predicted vs Observed CYLSPER with 1:1 line and GAM smooth line"
  ) +
  theme_bw()

#view
plot_cyl
#######################COMBINE MICX AND CYLSPER PRED/OBERSERVE

####Combine plot
# Combine plots
combine_toxin_p <- plot_micx + plot_cyl + plot_layout(ncol = 2)

# Show the plot
print(combine_toxin_p)
###############################################################

##########################################################################varible importance plot___FIGURE 3

library(mgcv)
library(tidyverse)
##########################
library(showtext)
# Load Times New Roman
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")  # Windows path
showtext_auto()
#ADD to plot
#theme_bw(base_family = "Times New Roman")
####################################


#2-CYLSPER
# Extract summary
sm <- summary(Model_CYL_13B$gam)$s.table

# Convert to data frame
imp_df <- as.data.frame(sm)
imp_df$term <- rownames(sm)

# Add significance flag
imp_df <- imp_df %>%
  mutate(significant = ifelse(`p-value` < 0.05, "Significant", "Not significant"))

# Rank by F-statistic
imp_df <- imp_df %>% 
  arrange(desc(F)) %>% 
  mutate(term = factor(term, levels = term))

# Bar plot (NOT USED)
ggplot(imp_df, aes(x = term, y = F)) +
  geom_col() +
  coord_flip() +
  labs(title = "CYLSPER GAMM variable importance (Ranked by F-statistic)",
       x = "Predictor (Smooth terms)",
       y = "F value") +
  theme_bw(base_size = 14)


# dot plot
AA <- ggplot(imp_df, aes(x = F, y = term, color = significant)) +
  geom_point(size = 6) +
  geom_segment(aes(x = 0, xend = F, y = term, yend = term), color = "black") + #grey60
  scale_color_manual(values = c("Significant" = "red", 
                                "Not significant" = "blue")) +
  labs(
    title = "(B). CYLSPER GAMM variable importance",
    x = "F-statistics",
    y = "Predictors (Smooth terms)",
    color = "Significance (p < 0.05)"
  ) +
  theme_bw(base_size = 14, base_family = "Times New Roman")+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
AA

##############1-MICX
# Extract summary
sm22 <- summary(MICX_m13_log$gam)$s.table

# Convert to data frame
imp_df2 <- as.data.frame(sm22)
imp_df2$term <- rownames(sm22)

# Add significance flag
imp_df2 <- imp_df2 %>%
  mutate(significant = ifelse(`p-value` < 0.05, "Significant", "Not significant"))

# Rank by F-statistic
imp_df2 <- imp_df2 %>% 
  arrange(desc(F)) %>% 
  mutate(term = factor(term, levels = term))

# bar Plot
ggplot(imp_df2, aes(x = term, y = F)) +
  geom_col() +
  coord_flip() +
  labs(title = "MICX GAMM variable importance (Ranked by F-statistic)",
       x = "Predictor (Smooth Terms)",
       y = "F value") +
  theme_bw(base_size = 14)

# dot plot
#BB <- ggplot(imp_df2, aes(x = F, y = term)) +
#  geom_point(size = 4) +
#  geom_segment(aes(x = 0, xend = F, y = term, yend = term)) +
#  labs(title = "(A). MICX GAMM variable importance",
#       x = "F-statistic",
#       y = "Predictor (Smooth terms)") +
#  theme_bw(base_size = 14)

BB <- ggplot(imp_df2, aes(x = F, y = term, color = significant)) +
  geom_point(size = 6) +
  geom_segment(aes(x = 0, xend = F, y = term, yend = term), color = "black") +
  scale_color_manual(values = c("Significant" = "red", 
                                "Not significant" = "blue")) +
  labs(
    title = "(A). MICX GAMM variable importance",
    x = "F-statistics",
    y = "Predictors (Smooth terms)",
    color = "Significance (p < 0.05)"
  ) +
  theme_bw(base_size = 14, base_family = "Times New Roman") + 
  theme(legend.position = "none")+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


BB
#COMBINE
combine_AA <- BB + AA + plot_layout(ncol = 2)

# Show the plot
print(combine_AA)


#SAVE
#Increase theme text globally
combine_AA <- BB + AA + plot_layout(ncol = 2) &
  theme(
    plot.title = element_text(size = 50, face = "bold"),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 38),
    legend.title = element_text(size = 38),
    legend.text = element_text(size = 30)
  )
#THEN SAVE
ggsave("combine_AA.png",
       plot = combine_AA,
       width = 14.76,
       height = 10.08,
       units = "in",
       dpi = 300)

#################################################chla-cyano__FIGURE6
##############3-MICX

CYANO_BIOV_9a
CYANO_BIOV_9b
CYANO_BIOV_9c
CYANO_BIOV_9d


# Extract summary
sma <- summary(CYANO_BIOV_9a$gam)$s.table
smb <- summary(CYANO_BIOV_9b$gam)$s.table
smc <- summary(CYANO_BIOV_9c$gam)$s.table
smd <- summary(CYANO_BIOV_9d$gam)$s.table


# Convert to data frame
#a
imp_df2a <- as.data.frame(sma)
imp_df2a$term <- rownames(sma)
#b
imp_df2b <- as.data.frame(smb)
imp_df2b$term <- rownames(smb)
#c
imp_df2c <- as.data.frame(smc)
imp_df2c$term <- rownames(smc)
#d
imp_df2d <- as.data.frame(smd)
imp_df2d$term <- rownames(smd)

# Add significance flag
#a
imp_df2a <- imp_df2a %>%
  mutate(significant = ifelse(`p-value` < 0.05, "Significant", "Not significant"))
#b
imp_df2b <- imp_df2b %>%
  mutate(significant = ifelse(`p-value` < 0.05, "Significant", "Not significant"))
#c
imp_df2c <- imp_df2c %>%
  mutate(significant = ifelse(`p-value` < 0.05, "Significant", "Not significant"))
#d
imp_df2d <- imp_df2d %>%
  mutate(significant = ifelse(`p-value` < 0.05, "Significant", "Not significant"))


# Rank by F-statistic
#a
imp_df2a <- imp_df2a %>% 
  arrange(desc(F)) %>% 
  mutate(term = factor(term, levels = term))
#b
imp_df2b <- imp_df2b %>% 
  arrange(desc(F)) %>% 
  mutate(term = factor(term, levels = term))
#c
imp_df2c <- imp_df2c %>% 
  arrange(desc(F)) %>% 
  mutate(term = factor(term, levels = term))
#d
imp_df2d <- imp_df2d %>% 
  arrange(desc(F)) %>% 
  mutate(term = factor(term, levels = term))

####PLOTS
#dot plot
#A
cyanoA <- ggplot(imp_df2a, aes(x = F, y = term, color = significant)) +
  geom_point(size = 6) +
  geom_segment(aes(x = 0, xend = F, y = term, yend = term), color = "black") +
  scale_color_manual(values = c("Significant" = "red", 
                                "Not significant" = "blue")) +
  labs(
    title = "(A). Chlorophyll-a GAMM variable importance",
    x = "F-statistic",
    y = "Predictor (Smooth terms)",
    color = "Significance (p < 0.05)"
  ) +
  theme_bw(base_size = 14, base_family = "Times New Roman")+ 
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

#B
cyanoB <- ggplot(imp_df2b, aes(x = F, y = term, color = significant)) +
  geom_point(size = 6) +
  geom_segment(aes(x = 0, xend = F, y = term, yend = term), color = "black") +
  scale_color_manual(values = c("Significant" = "red", 
                                "Not significant" = "blue")) +
  labs(
    title = "(B). Cyanobacteria biovolume GAMM variable importance",
    x = "F-statistic",
    y = "Predictor (Smooth terms)",
    color = "Significance (p < 0.05)"
  ) +
  theme_bw(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

#C
cyanoC <- ggplot(imp_df2c, aes(x = F, y = term, color = significant)) +
  geom_point(size = 6) +
  geom_segment(aes(x = 0, xend = F, y = term, yend = term), color = "black") +
  scale_color_manual(values = c("Significant" = "red", 
                                "Not significant" = "blue")) +
  labs(
    title = "(C). Microcystis biovolume GAMM variable importance",
    x = "F-statistic",
    y = "Predictor (Smooth terms)",
    color = "Significance (p < 0.05)"
  ) +
  theme_bw(base_size = 14, base_family = "Times New Roman")+ 
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

#D
cyanoD <- ggplot(imp_df2d, aes(x = F, y = term, color = significant)) +
  geom_point(size = 6) +
  geom_segment(aes(x = 0, xend = F, y = term, yend = term), color = "black") +
  scale_color_manual(values = c("Significant" = "red", 
                                "Not significant" = "blue")) +
  labs(
    title = "(D). Cylindrospermopsis biovolume GAMM variable importance",
    x = "F-statistic",
    y = "Predictor (Smooth terms)",
    color = "Significance (p < 0.05)"
  ) +
  theme_bw(base_size = 14, base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

#COMBINE
combine_cyano <- cyanoA + cyanoB + cyanoC + cyanoD + plot_layout(ncol = 2)

# Show the plot
print(combine_cyano)


#SAVE
#Increase theme text globally
combine_cyano <- cyanoA + cyanoB + cyanoC + cyanoD + plot_layout(ncol = 2) &
  theme(
    plot.title = element_text(size = 50, face = "bold"),
    axis.title = element_text(size = 45),
    axis.text = element_text(size = 38),
    legend.title = element_text(size = 38),
    legend.text = element_text(size = 30)
  )
#THEN SAVE
ggsave("combine_cyano.png",
       plot = combine_cyano,
       width = 16,
       height = 12,
       units = "in",
       dpi = 300)

