##Ongoing work: update regularly
#Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr) #for correlation tests
library(corrplot)
library(ggcorrplot)
#FOR CLEAN NAMES (USE LATER) e.g. clean_names(poorly_named_df)
library(janitor)



#Load in rawdata from Github ##remember to use that raw link (this appears to create a one time token, that have to be repeaat everytime######
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
########################################################################################################
#cHECK RELATIONSHIPS BETWEN calculated thermocline_depth AND thermocline_depth_rLake
thermocline_depth_data %>%
  ggplot(aes(x = thermocline_depth_rLake, y = thermocline_depth)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw()

#######################################################JOIN combined_data_pca_NEW WITH Thermocline Depth and ZMAX data
########################### Merge datasets 
combined_data_pca_NEW <- combined_data_pca_NEW %>%
  left_join(thermocline_depth_data, by = "SITE_ID")

names(combined_data_pca_NEW)

##############################
#cHECK RELATIONSHIPS BETWEN INDEX SITE DEPTH AND THE NEW ZMAX CALCULATED

combined_data_pca_NEW %>%
  ggplot(aes(x = INDEX_SITE_DEPTH, y = zmax)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw()

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

###########################################################################CHECK 2022 selected data BELOW######################################################
names(combined_data_pca_NEW)

################################################################################################################2017####repeat processes done in 2022 for 2017
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

library(dplyr)
library(stringr)

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
library(dplyr)

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

# Load the package
library(rLakeAnalyzer)
library(dplyr)

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
########################################################################################################
#cHECK RELATIONSHIPS BETWEN calculated thermocline_depth AND thermocline_depth_rLake
thermocline_depth_data_2017 %>%
  ggplot(aes(x = thermocline_depth_rLake, y = thermocline_depth)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw()

#######################################################JOIN combined_data_pca_2017_NEW WITH Thermocline Depth and ZMAX data
########################### Merge datasets 
combined_data_pca_2017_NEW <- combined_data_pca_2017_NEW %>%
  left_join(thermocline_depth_data_2017, by = "SITE_ID")

names(combined_data_pca_2017_NEW)

##############################
#cHECK RELATIONSHIPS BETWEN INDEX SITE DEPTH AND THE NEW ZMAX CALCULATED

combined_data_pca_2017_NEW %>%
  ggplot(aes(x = INDEX_SITE_DEPTH, y = zmax)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw()

#RHO=0.99 WITH P LESS THAN 0.0001

###########################################################################CHECK 2017 final selected data BELOW############
names(combined_data_pca_2017_NEW)

#cmpare with 2022
names(combined_data_pca_NEW)




#################################################################################################################################2012####repeat processes done in 2022 and 2017 for 2012
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

library(dplyr)
library(stringr)

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
library(dplyr)

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

# Load the package
library(rLakeAnalyzer)
library(dplyr)

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
########################################################################################################
#cHECK RELATIONSHIPS BETWEN calculated thermocline_depth AND thermocline_depth_rLake
thermocline_depth_2012_data %>%
  ggplot(aes(x = thermocline_depth_rLake, y = thermocline_depth)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), 
           parse = TRUE) +
  theme_bw()

#######################################################JOIN combined_data_pca_2012_NEW WITH Thermocline Depth and ZMAX data
########################### Merge datasets 
combined_data_pca_2012_NEW <- combined_data_pca_2012_NEW %>%
  left_join(thermocline_depth_2012_data, by = "UID")

names(combined_data_pca_2012_NEW)

##############################
###########################################################################CHECK 2012 selected data BELOW######################################################

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
           lab = TRUE, 
           p.mat = p_matrix, 
           sig.level = 0.05, 
           insig = "blank", 
           title = "Spearman Rank Correlation Matrix with P-values (selected_data_combined_all_years)") #good plot #Cyano groups and TOXINS plus p-value consideration.
######################################################################################################################################



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

# Load necessary libraries
library(ggcorrplot)

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

#FOR TIMES NEW ROMAN FONT####################################################################
#install.packages("extrafont")
library(extrafont)
#font_import()       # Only run once; takes time
#loadfonts(device = "win")  # or use `device = "pdf"` for PDFs


#replot (make circle and lab FALSE)
ggcorrplot(cor_MICX_CYL_vs_taxa,
           method = "circle",
           type = "full",
           lab = TRUE,
           p.mat = p_MICX_CYL_vs_taxa,
           sig.level = 0.01,
           insig = "blank",
           title = "MICX and CYLSPER vs Cyanobacteria Taxa") +
  theme(
    text = element_text(family = "Times New Roman", face = "bold")
  )

#REPLOT (make circle and lab FALSE)
# Plot 2: Taxa vs Taxa
ggcorrplot(cor_taxa_vs_taxa,
           method = "circle",
           type = "upper",
           lab = TRUE,
           p.mat = p_taxa_vs_taxa,
           sig.level = 0.01,
           insig = "blank",
           title = "Inter-correlation Among Cyanobacteria Taxa")+
  theme(
    text = element_text(family = "Times New Roman", face = "bold")
  )
##############################################################################################

