##Ongoing work: update regularly
#Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr) #for correlation tests

#FOR CLEAN NAMES (USE LATER) e.g. clean_names(poorly_named_df)
library(janitor)



#Load in rawdata from Github ##remember to use that raw link (this appears to create a one time token, that have to be repeaat everytime######
#2012 DATA
##NLA22_waterchem data
WaterChem2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2022_dataset/nla22_waterchem_wide.csv?token=GHSAT0AAAAAAC65NYVYA26MFHC24NK5FWSEZ6UGV7A')

##NLA22_Toxin data
toxin2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2022_dataset/nla22_algaltoxins.csv?token=GHSAT0AAAAAAC65NYVZHP4ZM6SOZBPQY37SZ6ULG2A')

##NLA22_Secchi data
secchi2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2022_dataset/nla22_secchi.csv?token=GHSAT0AAAAAAC65NYVY6HWCNMW5ZHVDNSYUZ6VRFZQ')

##NLA22_landscape data
landscape2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2022_dataset/nla2022_landscape_wide_0.csv?token=GHSAT0AAAAAAC65NYVZQUAGHVRPC4YIFHO4Z6YQJRQ')

##NLA22_profile data
profile2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2022_dataset/nla2022_profile_wide.csv?token=GHSAT0AAAAAAC65NYVZLJPLSWOAGAXSRUY6Z6ZNA6A')


##NLA22_siteinfo data
siteinfo2022 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2022_dataset/nla22_siteinfo.csv?token=GHSAT0AAAAAAC65NYVY4L5TLAFL6DXY2XIUZ62XU2A')

#NLA22_Phytoplankton data
phytoplanktoncount2022_data <- read.csv("https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2022_dataset/nla2022_phytoplanktoncount_wide.csv?token=GHSAT0AAAAAAC65NYVY5ZGHCOTHKXJKZRSGZ7CXQLA")  

#############################################################
#2017 DATA














#Check more detail on Warning message
#2022
problems(WaterChem2022)


#Check2
# check the column names
##2022
names(WaterChem2022)
names(toxin2022)
names(secchi2022)
names(landscap2022)
names(profile2022)
names(siteinfo2022)


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
  select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLA_RESULT,
         CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NITRATE_N_RESULT, 
         NITRATE_NITRITE_N_RESULT, NITRITE_N_RESULT, NTL_DISS_RESULT, NTL_RESULT, PH_RESULT, POTASSIUM_RESULT,
         PTL_DISS_RESULT, PTL_RESULT, SODIUM_RESULT, SULFATE_RESULT, TURB_RESULT
  )

#Select Toxin
#2022
names(toxin2022_wide)
toxin2022_subset <- toxin2022_wide %>% 
  select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, MICX, CYLSPER)


# Select SECCHI
#2022
names(secchi2022_cal)

secchi2022_sebset <- secchi2022_cal %>% 
  select(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO, Secchi)


# Select landscape (elevation)
#2022 select for Elevation

landscape2022_sebset <- landscape2022 %>% 
  select(UNIQUE_ID, SITE_ID, ELEV, ELEV_MAX, ELEV_MIN) #Note : no DATE_COL and VISIT_NO in landscape Data

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

siteinfo2022_subset <- siteinfo2022 %>% 
  select(UNIQUE_ID, SITE_ID, AREA_HA, ELEVATION,LAKE_ORGN, LAT_DD83, LON_DD83, INDEX_SITE_DEPTH) #note: we have elevation in landscape data too



# Select and calculation on phytoplankton data

# We define PTOX taxa based on Chapman & Foss (2020); Chorus & Welker (2021).
ptox_taxa <- c("ANABAENOPSIS", "ANABAENA", "APHANIZOMENON", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM", "CUSPIDOTHRIX",
               "RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DESMONOSTOC",  "DOLICHOSPERMUM", "FISCHERELLA", "GEITLERINEMA", 
               "GLOEOTRICHIA", "HAPALOSIPHON", "LEPTOLYNGBYA", "PLECTONEMA", "LIMNOTHRIX", "MERISMOPEDIA", "MICROCOLEUS",
               "PHORMIDIUM", "MICROCYSTIS", "MICROSEIRA", "LYNGBYA", "NOSTOC", "OSCILLATORIA", "PLANKTOTHRIX", "PSEUDANABAENA",
               "RADIOCYSTIS", "RIVULARIA", "ROMERIA", "SCYTONEMA", "SNOWELLA", "SPHAEROSPERMOPSIS", "STENOMITOS", "SYNECHOCOCCUS",
               "SYNECHOCYSTIS", "TOLYPOTHRIX", "TRICHODESMIUM", "TRICHORMUS", "UMEZAKIA", "WORONICHINIA")


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
    PTOX_biovolume = sum(BIOVOLUME[grepl(paste(ptox_taxa, collapse = "|"), TARGET_TAXON, ignore.case = TRUE)], na.rm = TRUE)   #selects only the BIOVOLUME values where TARGET_TAXON matches a taxon in ptox_taxa.
  ) %>%
  mutate(
    percent_cyanobacteria_biovolume = (total_cyanobacteria_biovolume / total_phytoplankton_biovolume) * 100,
    percent_cyanobacteria_density = (total_cyanobacteria_density / total_phytoplankton_density) * 100,
    percent_cyanobacteria_abundance = (total_cyanobacteria_abundance / total_phytoplankton_abundance) * 100,
    percent_PTOX_biovolume = (PTOX_biovolume / total_cyanobacteria_biovolume) * 100  # % PTOX biovolume relative to total_cyanobacteria_biovolume
  )


# View
print(phyto2022_summary)





#View the first few rows
#2022
head(WaterChem2022_subset)
head(toxin2022_subset)
head(secchi2022_sebset)
head(landscape2022_sebset)
#head profiles
head(mean_profiles_alldepths)
head(mean_profiles_top1m) #Note: I will likely be using this top1m in the combined dataset
head(mean_profiles_1to2m)
head(mean_profiles_2to4m)
head(mean_profiles_below4m)
head(mean_profiles_below5m)
##siteinfo2022
head(siteinfo2022_subset)
#Phyto
head(phyto2022_summary)





#count missing values (NAs)
colSums(is.na(WaterChem2022_subset))
colSums(is.na(toxin2022_subset))
colSums(is.na(secchi2022_sebset))
colSums(is.na(landscape2022_sebset))
#profiles
colSums(is.na(mean_profiles_alldepths))
colSums(is.na(mean_profiles_top1m))
colSums(is.na(mean_profiles_1to2m))
colSums(is.na(mean_profiles_2to4m))
colSums(is.na(mean_profiles_below4m))
colSums(is.na(mean_profiles_below5m))
#siteinfo
colSums(is.na(siteinfo2022_subset)) #note NAs in LAKE_ORGN and INDEX_SITE_DEPTH 
#phyto
colSums(is.na(phyto2022_summary)) #note: NAs retured for percent ptox is due to 0s





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

#Join phyto data

combined_data6 <- left_join(combined_data5b, phyto2022_summary, 
                            by = c("SITE_ID", "DATE_COL"),
                            relationship = "many-to-many") #note
names(combined_data6)

#############################
#############################
####cHECK QUICK RELATIONSHIP PLOTS
#Load library
library(ggpubr)

combined_data5b %>%
  ggplot(aes(x = CHLA_RESULT, y = MICX)) + #Change variables as many times as possible
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+ #Note: R VALUES IN THE PLOT REPRESENT RHO VALUE.
  theme_bw()
#######################
#TN-TP
# TN:TP ratio by weight

# Convert NTL from mg/L to µg/L
combined_data5b <- combined_data5b %>%
  mutate(
    NTL_ugL = NTL_RESULT * 1000,  # Convert NTL to µg/L
    TN_TP_RATIO = NTL_ugL / PTL_RESULT  # Compute TN:TP ratio
  )

# TN-TP scatter plot with reference line
combined_data5b %>%
  ggplot(aes(x = NTL_RESULT, y = PTL_RESULT, color = TN_TP_RATIO > 14)) + 
  geom_point() +  # Scatter plot
  scale_x_log10() +  # Log scale for X-axis (NTL)
  scale_y_log10() +  # Log scale for Y-axis (PTL)
  stat_cor(method = "spearman", label.x = 1, label.y = 1.5, 
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) + 
  geom_abline(slope = 1, intercept = log10(1/14), 
              linetype = "dashed", color = "red", linewidth = 1.2) +  # 14:1 reference line
  scale_color_manual(values = c("blue", "red"), labels = c("Below 14:1", "Above 14:1")) +  
  theme_bw() +
  theme(legend.title = element_blank())  # Removes legend title



####################################################################################

######################################################################################################
########################################################################################################
#confirm values with
cor.test(combined_data5b$CHLA_RESULT, combined_data5b$MICX, method = "spearman")

#combined_data6
cor.test(combined_data6$CHLA_RESULT, combined_data6$CYLSPER, method = "spearman")


###############
#shows rho= , actual pvalues
combined_data6 %>%
  ggplot(aes(x = Temp_top1m, y = CYLSPER)) + #Change variables as many times as possible
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2, aes(label = paste("rho == ", ..r.., ..p.label.., sep = "~`,`~")))+ 
  theme_bw()

#make the plot label say ρ instead of R
combined_data6 %>%
  ggplot(aes(x = total_phytoplankton_density, y = MICX)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), #shows approx pvalue e.g p=0
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "total_phytoplankton_density",
    y = "Microcystins(MIC; ug/L)", #Cylindrspermopsins(CYL; ug/L)
  )
############################$$$$$$$
library(ggplot2)

# Define chloride regulatory limits
chloride_limits <- data.frame(
  Limit_Type = c("EPA Chronic Limit (230 mg/L)", 
                 "WHO Recommended Limit (250 mg/L)", 
                 "EPA Additional (314 mg/L)", 
                 "EPA Additional (441 mg/L)", 
                 "EPA Acute Limit (860 mg/L)"),
  Chloride_Concentration = c(230, 250, 314, 441, 860)
)

# Generate the plot for Chloride only
ggplot(combined_data5b, aes(x = CHLORIDE_RESULT)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +  # Histogram for Chloride
  geom_density(aes(y = ..count.. * 10), color = "red", size = 1.2) +  # Density plot scaled to match histogram
  geom_vline(data = chloride_limits, aes(xintercept = Chloride_Concentration, linetype = Limit_Type),
             color = "blue", size = 1) +  # Vertical lines for regulatory limits
  scale_x_log10() +  # Log scale for better visualization
  theme_bw() +
  labs(
    x = "Chloride Concentration (mg/L)",
    y = "Count",
    title = "Distribution of Chloride Concentrations with Regulatory Limits",
    linetype = "Regulatory Limits"
  )

#################$$$$$$$$$$$$$$$
#Create Quantile Groups
combined_data5b <- combined_data5b %>%
  mutate(CHLA_quantile = ntile(CHLA_RESULT, 4))  # Change 4 to number of quantile as necessary

#Note: 1 = 0–25%; 2 = 25–50%; 3 = 50–75%; 4 = 75–100%
#Lowest 25% of CHLORIDE values; Lower-middle 25%; Upper-middle 25%; Highest 25% of CHLORIDE values

#chloride QUANTILES
combined_data5b <- combined_data5b %>%
  mutate(CHLORIDE_quantile = ntile(CHLORIDE_RESULT, 4))  

#MICX QUANTILES
combined_data5b <- combined_data5b %>%
  mutate(MICX_quantile = ntile(MICX, 4))  

#nutrients(N&P) VS TOXINS QUANTILES
#IVE CHANGES THIS VARIABLES (4TESTING)
combined_data5b %>%
  filter(!is.na(CHLORIDE_quantile), CYLSPER > 0, MICX > 0) %>% 
  ggplot(aes(x = CYLSPER, y = MICX)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman",
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  facet_wrap(~ CHLORIDE_quantile, labeller = label_both) +
  theme_bw() +
  labs(
    x = "Cylindrospermopsin (CYL)",
    y = "Microcystins (MIC)",
    subtitle = "Log-transformed values with Spearman correlation",
  )


#NA GROUP REMOVED FROM PLOT
combined_data5b %>%
  filter(!is.na(CHLA_quantile), NTL_RESULT > 0, PTL_RESULT > 0) %>%
  ggplot(aes(x = NTL_RESULT, y = PTL_RESULT)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman",
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  facet_wrap(~ CHLA_quantile, labeller = label_both) +
  theme_bw()
#######################################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Load necessary package
library(vegan)
library(dplyr)

# Ensure the data structure is correct
str(phytoplanktoncount2022_data)

# Aggregate abundance by taxon and sample (assuming "Sample_ID" exists)
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

###
#Join phyto data with diversity_results

combined_data7A <- left_join(combined_data6, diversity_results, 
                             by = c("SITE_ID"),
                             relationship = "many-to-many") #note
names(combined_data7A)
#####################################################################################
#plots

combined_data7A %>%
  ggplot(aes(x = CHLA_RESULT, y = total_cyanobacteria_biovolume)) + #Simpson_Index Evenness Shannon_Index total_phytoplankton_biovolume total_phytoplankton_density total_phytoplankton_abundance
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)), #shows approx pvalue e.g p=0
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "CHLA_RESULT",
    y = "total_cyanobacteria_biovolume",
  )

########################
#
library(hexbin)

ggplot(combined_data7A, aes(x = Temp_top1m, y = Simpson_Index)) +
  geom_hex(bins = 30) +  
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Hexbin Plot of Simpson_Index Diversity Index vs. Temperature",
       x = "Temperature (°C)", y = "Simpson_Index")
##########################################################
ggplot(combined_data7A, aes(x = Temp_top1m, y = Shannon_Index)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "red") +
  theme_minimal() +
  labs(title = "GAM Fit: Temperature vs Shannon Diversity Index",
       x = "Temperature (°C)", y = "Shannon Diversity Index")
###############################################################&&&&&&&&&&&&&&&&&&&&&&&&&&
#Scatterplot with Trend Line (Updated)
library(ggplot2)
library(ggpubr)
#Pairwise Relationships: Scatterplots with Trend Lines
# Scatterplot for Temperature vs Microcystins
p1 <- combined_data6 %>%
  ggplot(aes(x = Temp_top1m, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_continuous() +  # Log scale removed for clarity
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 2, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Surface Temperature (°C, top 1m)",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between Temperature and Microcystins"
  )



#CHLORIDE_RESULT   MICX

p2 <- combined_data6 %>%
  ggplot(aes(x = CHLORIDE_RESULT, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_log10() +  # Log scale removed for clarity
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 2, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Chloride (mg/L)",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between Chloride and Microcystins"
  )


#NTL_RESULT
p3 <- combined_data6 %>%
  ggplot(aes(x = NTL_RESULT, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 0.5, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Total nitrogen (µg/L)",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between Total nitrogen and Microcystins"
  )


#PTL_RESULT
p4 <- combined_data6 %>%
  ggplot(aes(x = PTL_RESULT, y = MICX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Total Phosphrus (µg/L)",
    y = "Microcystins (MIC; µg/L)",
    title = "Relationship Between Total Phosphrus and Microcystins"
  )

# Arrange plots together
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
################################################################   CYLSPER Cylindrospermopsins (CYL; µg/L)
p1A <- combined_data6 %>%
  ggplot(aes(x = Temp_top1m, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_continuous() +  # Log scale removed for clarity
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 2, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Surface Temperature (°C, top 1m)",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between Temperature and Cylindrospermopsins"
  )



#CHLORIDE_RESULT   MICX

p2B <- combined_data6 %>%
  ggplot(aes(x = CHLORIDE_RESULT, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_log10() +  # Log scale removed for clarity
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 2, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Chloride (mg/L)",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between Chloride and Cylindrospermopsins"
  )


#NTL_RESULT
p3C <- combined_data6 %>%
  ggplot(aes(x = NTL_RESULT, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 0.5, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Total nitrogen (µg/L)",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between Total nitrogen and Cylindrospermopsins"
  )


#PTL_RESULT
p4D <- combined_data6 %>%
  ggplot(aes(x = PTL_RESULT, y = CYLSPER)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "purple") +
  scale_x_log10() + 
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2.5,
           aes(label = paste("rho == ", ..r.., "*','~p == ", ..p..)),
           parse = TRUE) +
  theme_bw() +
  labs(
    x = "Total Phosphrus (µg/L)",
    y = "Cylindrospermopsins (CYL; µg/L)",
    title = "Relationship Between Total Phosphrus and Cylindrospermopsins"
  )

# Arrange plots together
ggarrange(p1A, p2B, p3C, p4D, ncol = 2, nrow = 2)

#####################################################
##################################################################################CORRELATION MATRIX
# Install required packages if not already installed
#install.packages(c("GGally", "ggcorrplot"))

library(dplyr)
library(corrplot)
library(GGally)
library(ggcorrplot)


#cHECK NAMES
names(combined_data7A)
# Select specific numeric variables (adjust based on your dataset)
selected_data <- combined_data7A %>%
  select(MICX, CYLSPER, CHLA_RESULT, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, PTOX_biovolume, percent_cyanobacteria_biovolume,
         percent_PTOX_biovolume, Shannon_Index, Simpson_Index, Evenness)

selected_data2 <- combined_data7A %>%
  select(Temp_top1m, Secchi, pH_top1m, PH_RESULT, ELEVATION, INDEX_SITE_DEPTH, #conductivity top1 also removed
         AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NTL_RESULT, PTL_RESULT,
         NTL_DISS_RESULT, PTL_DISS_RESULT, SODIUM_RESULT, TURB_RESULT, SULFATE_RESULT, POTASSIUM_RESULT) #All the nitrate nitrite are removed because of NA problem check to see which can be used later

selected_data_combine <- combined_data7A %>%
  select(MICX, CYLSPER, CHLA_RESULT, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, PTOX_biovolume, percent_cyanobacteria_biovolume,
         percent_PTOX_biovolume, Shannon_Index, Simpson_Index, Evenness, Temp_top1m, Secchi, pH_top1m, PH_RESULT, ELEVATION, INDEX_SITE_DEPTH, 
         AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NTL_RESULT, PTL_RESULT,
         NTL_DISS_RESULT, PTL_DISS_RESULT, SODIUM_RESULT, TURB_RESULT, SULFATE_RESULT, POTASSIUM_RESULT)

# Check the structure to ensure you have the desired variables
str(selected_data)
str(selected_data2)
str(selected_data_combine)
# Compute the Spearman correlation matrix
spearman_cor <- cor(selected_data, method = "spearman", use = "complete.obs")
spearman_cor2 <- cor(selected_data2, method = "spearman", use = "complete.obs")
spearman_cor3 <- cor(selected_data_combine, method = "spearman", use = "complete.obs")

# Visualize the correlation matrix
corrplot(spearman_cor, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Spearman Rank Correlation Matrix", mar = c(0, 0, 1, 0))

corrplot(spearman_cor2, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Spearman Rank Correlation Matrix", mar = c(0, 0, 1, 0))


corrplot(spearman_cor3, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Spearman Rank Correlation Matrix", mar = c(0, 0, 1, 0))
###################################################################################################################################################################
# Load necessary libraries
library(dplyr)
library(ggcorrplot)
#test run with few selected variables
# Select specific numeric variables 
selected_data <- combined_data7A %>%
  select(AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLA_RESULT, 
         CHLORIDE_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NTL_RESULT, 
         PH_RESULT, SODIUM_RESULT, TURB_RESULT, MICX, CYLSPER)

# Compute the Spearman correlation matrix
spearman_cor <- cor(selected_data, method = "spearman", use = "complete.obs")

# Function to compute correlation and p-value
cor_test <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  return(c(cor = test$estimate, p.value = test$p.value))
}

# Prepare matrices for correlation and p-values
n_vars <- ncol(selected_data)
p_matrix <- matrix(1, n_vars, n_vars)
rownames(p_matrix) <- colnames(selected_data)
colnames(p_matrix) <- colnames(selected_data)

# Loop through the matrix to fill p-values
for (i in 1:(n_vars - 1)) {
  for (j in (i + 1):n_vars) {
    result <- cor_test(selected_data[[i]], selected_data[[j]])
    p_matrix[i, j] <- result["p.value"]
    p_matrix[j, i] <- result["p.value"]
  }
}

# Visualize with ggcorrplot
# insignificant correlations (p > sig.level) are left blank in the plot below.
ggcorrplot(spearman_cor, 
           method = "circle",     # Use "circle" or "square"
           type = "upper", 
           lab = TRUE, 
           p.mat = p_matrix, 
           sig.level = 0.05, 
           insig = "blank", 
           title = "Spearman Rank Correlation Matrix with P-values")
############################################################################################################################################################################4combined
# Select specific numeric variables
names(selected_data_combine)

# Compute the Spearman correlation matrix
spearman_cor_p <- cor(selected_data_combine, method = "spearman", use = "complete.obs")

# Function to compute correlation and p-value
cor_test <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  return(c(cor = test$estimate, p.value = test$p.value))
}

# Prepare matrices for correlation and p-values
n_vars <- ncol(selected_data_combine)
p_matrix <- matrix(1, n_vars, n_vars)
rownames(p_matrix) <- colnames(selected_data_combine)
colnames(p_matrix) <- colnames(selected_data_combine)

# Loop through the matrix to fill p-values
for (i in 1:(n_vars - 1)) {
  for (j in (i + 1):n_vars) {
    result <- cor_test(selected_data_combine[[i]], selected_data_combine[[j]])
    p_matrix[i, j] <- result["p.value"]
    p_matrix[j, i] <- result["p.value"]
  }
}

# Visualize with ggcorrplot-
# insignificant correlations (p > sig.level) are left blank in the plot below.
ggcorrplot(spearman_cor_p, 
           method = "circle",     # Use "circle" or "square"
           type = "upper", 
           lab = TRUE, 
           p.mat = p_matrix, 
           sig.level = 0.05, 
           insig = "blank", 
           title = "Spearman Rank Correlation Matrix with P-values")  ####A VERY GOOD INFORMATIVE PLOT
######################################################################################################################FOR SPECIES LEVEL DATA
library(tidyr)
library(dplyr)
names(phytoplanktoncount2022_data)
# Transform the data to a wider format with a summarizing function
phytoplankton_SPECIES <- phytoplanktoncount2022_data %>%
  select(UNIQUE_ID, TARGET_TAXON, SITE_ID,  DATE_COL, VISIT_NO, BIOVOLUME) %>%
  pivot_wider(
    names_from = TARGET_TAXON,
    values_from = BIOVOLUME,
    values_fill = list(BIOVOLUME = 0),  # Fill missing values with 0
    values_fn = list(BIOVOLUME = sum)   # Sum BIOVOLUME values for each combination
  )

# Check the structure and preview the data
str(phytoplankton_SPECIES)
head(phytoplankton_SPECIES)
names(phytoplankton_SPECIES)


#to make species longer
species_long <- phytoplankton_SPECIES %>%
  pivot_longer(
    cols = -c(UNIQUE_ID, SITE_ID, DATE_COL, VISIT_NO),  # Exclude the identifying columns
    names_to = "Species", 
    values_to = "Biovolume"
  )

# Check the structure to confirm the transformation
str(species_long)
colnames(phytoplankton_SPECIES)

# Select only species columns (excluding identifiers)
species_numeric <- phytoplankton_SPECIES %>%
  select(-UNIQUE_ID, -SITE_ID, -DATE_COL, -VISIT_NO)

# Ensure data is numeric
species_numeric <- as.data.frame(lapply(species_numeric, as.numeric))

# Check for columns with all zeros or missing values
species_numeric <- species_numeric[, colSums(!is.na(species_numeric)) > 0]
species_numeric <- species_numeric[, colSums(species_numeric != 0) > 0]

# Calculate Spearman correlation matrix
spearman_cor <- cor(species_numeric, method = "spearman", use = "complete.obs")

# Visualize the correlation matrix
corrplot(spearman_cor, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Spearman Rank Correlation Matrix", mar = c(0, 0, 1, 0)) #SPECIES LEVEL PLOTS ARE MESSY
################################################################################
#lets consider Group by Taxonomy (Genus)

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
  select(SITE_ID, ANABAENOPSIS, ANABAENA, APHANIZOMENON, APHANOCAPSA, ARTHROSPIRA, CHRYSOSPORUM, CUSPIDOTHRIX, #rEMOVE SITE ID TO RUN CORRELATION TEST
         RAPHIDIOPSIS, CYLINDROSPERMOPSIS, DOLICHOSPERMUM, GEITLERINEMA,    #NO DESMONOSTOC, FISCHERELLA, HAPALOSIPHON, PLECTONEMA, MICROCOLEUS Genus
         GLOEOTRICHIA, LEPTOLYNGBYA, LIMNOTHRIX, MERISMOPEDIA, 
         PHORMIDIUM, MICROCYSTIS, LYNGBYA, NOSTOC, OSCILLATORIA, PLANKTOTHRIX, PSEUDANABAENA, #No MICROSEIRA, RIVULARIA, SCYTONEMA, STENOMITOS, TOLYPOTHRIX, TRICHODESMIUM,
         RADIOCYSTIS, ROMERIA, SNOWELLA, SPHAEROSPERMOPSIS, SYNECHOCOCCUS,           #No TRICHORMUS, UMEZAKIA,
         SYNECHOCYSTIS,  WORONICHINIA)

# Check the structure to ensure you have the desired variables
str(selected_phytoplankton_wide_GROUP)
# Compute the Spearman correlation matrix

spearman_cor3 <- cor(selected_phytoplankton_wide_GROUP, method = "spearman", use = "complete.obs")

# Visualize the correlation matrix
corrplot(spearman_cor3, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Spearman Rank Correlation Matrix", mar = c(0, 0, 1, 0)) #Cyano groups only
########################################################################################

#JOIN THE NEW GENUS WITH  selected_data_combine
#selected_phytoplankton_wide_GROUP
#toxin2022_DL

#Join phyto GENUS data with toxin2022_DL

combined_data8 <- left_join(toxin2022_DL, selected_phytoplankton_wide_GROUP, 
                             by = c("SITE_ID"),
                             relationship = "many-to-many") #note
names(combined_data8)
str(combined_data8)
# Select only species columns (excluding identifiers)
combined_data8_numeric <- combined_data8 %>%
  select(-UNIQUE_ID, -SITE_ID, -DATE_COL, -VISIT_NO)
str(combined_data8_numeric)

# Compute the Spearman correlation matrix

spearman_cor4 <- cor(combined_data8_numeric, method = "spearman", use = "complete.obs")

# Visualize the correlation matrix
corrplot(spearman_cor4, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Spearman Rank Correlation Matrix", mar = c(0, 0, 1, 0)) #Cyano groups and TOXINS

#PLOT ANOTHER WITH P-VALUE
# Function to compute correlation and p-value
cor_test <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  return(c(cor = test$estimate, p.value = test$p.value))
}

# Prepare matrices for correlation and p-values
n_vars <- ncol(combined_data8_numeric)
p_matrix <- matrix(1, n_vars, n_vars)
rownames(p_matrix) <- colnames(combined_data8_numeric)
colnames(p_matrix) <- colnames(combined_data8_numeric)

# Loop through the matrix to fill p-values
for (i in 1:(n_vars - 1)) {
  for (j in (i + 1):n_vars) {
    result <- cor_test(combined_data8_numeric[[i]], combined_data8_numeric[[j]])
    p_matrix[i, j] <- result["p.value"]
    p_matrix[j, i] <- result["p.value"]
  }
}

# Visualize with ggcorrplot-
# insignificant correlations (p > sig.level) are left blank in the plot below.
ggcorrplot(spearman_cor4, 
           method = "circle",     # Use "circle" or "square"
           type = "upper", 
           lab = TRUE, 
           p.mat = p_matrix, 
           sig.level = 0.05, 
           insig = "blank", 
           title = "Spearman Rank Correlation Matrix with P-values") #good plot #Cyano groups and TOXINS plus p-value consideration.
###################################################################################################################4PCA

# Load required libraries
library(dplyr)
library(tidyr)

selected_data_combine2 <- combined_data7A %>%
  select(SITE_ID, MICX, CYLSPER, CHLA_RESULT, total_phytoplankton_biovolume, total_cyanobacteria_biovolume, PTOX_biovolume, percent_cyanobacteria_biovolume,
         percent_PTOX_biovolume, Shannon_Index, Simpson_Index, Evenness, Temp_top1m, Secchi, pH_top1m, PH_RESULT, ELEVATION, INDEX_SITE_DEPTH, 
         AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NTL_RESULT, PTL_RESULT,
         NTL_DISS_RESULT, PTL_DISS_RESULT, SODIUM_RESULT, TURB_RESULT, SULFATE_RESULT, POTASSIUM_RESULT)

# Select numeric columns from selected_data_combine
selected_physics <- selected_data_combine2 %>% 
  select_if(is.numeric)

# Check structure
str(selected_physics)

# Check structure of selected_phytoplankton_wide_GROUP
str(selected_phytoplankton_wide_GROUP)

# Ensure both datasets have the same number of rows
nrow(selected_physics) == nrow(selected_phytoplankton_wide_GROUP) #SHOWS FALSE

# Ensure that UNIQUE_ID is present in both datasets
# First, inspect the structure of each dataset to confirm the presence of UNIQUE_ID
str(selected_data_combine2)
str(selected_phytoplankton_wide_GROUP)

# Merge datasets by UNIQUE_ID

combined_data_pca <- selected_data_combine2 %>%
  left_join(selected_phytoplankton_wide_GROUP, by = "SITE_ID", relationship = "many-to-many")

# Check for missing values after merging
summary(combined_data_pca)

# Remove rows with missing values
combined_data_pca_clean <- combined_data_pca %>% 
  select(-SITE_ID) %>%  # Remove the identifier for PCA
  na.omit()
names(combined_data_pca)
# Check the structure to confirm alignment
str(combined_data_pca_clean)

# Scale the dataset
combined_data_scaled <- scale(combined_data_pca_clean)

# Perform PCA
pca_model <- prcomp(combined_data_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_model)

# Install necessary libraries
if (!requireNamespace("factoextra", quietly = TRUE)) {
  install.packages("factoextra")
}

library(factoextra)

# Extract the contributions for PC1 and PC2
contributions <- abs(pca_model$rotation[, 1:2])  # Absolute values of loadings for PC1 and PC2
contrib_sums <- rowSums(contributions)  # Sum of contributions for PC1 and PC2

# Identify top 10 contributing variables
top_vars <- names(sort(contrib_sums, decreasing = TRUE)[1:10])

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
###########################################################################################
# Extract PCA coordinates
pca_ind <- as.data.frame(pca_model$x)
pca_var <- as.data.frame(pca_model$rotation)

# Plot using ggplot
library(ggplot2)

ggplot() +
  geom_point(data = pca_ind, aes(x = PC1, y = PC2), color = "gray", alpha = 0.6) +
  geom_segment(data = pca_var, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
               arrow = arrow(length = unit(0.2, "cm")), color = "steelblue") +
  geom_text(data = pca_var, aes(x = PC1 * 5, y = PC2 * 5, label = rownames(pca_var)), 
            size = 3, vjust = -0.5) +
  theme_minimal() +
  labs(title = "Customized PCA Biplot")
#############################################
# Load necessary libraries
library(ggplot2)
library(FactoMineR)
library(factoextra)


# Perform PCA
pca_res <- PCA(combined_data_scaled, graph = FALSE)

# Scree Plot
fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 50)) + 
  ggtitle('Scree Plot')

# Cumulative Variance Plot
eig_values <- pca_res$eig
cum_var <- cumsum(eig_values[, 2])
plot(cum_var, type = 'b', xlab = 'Number of Components', ylab = 'Cumulative Variance (%)',
     main = 'Cumulative Variance Plot', pch = 16, col = 'blue')

# Biplot
fviz_pca_biplot(pca_res, repel = TRUE, 
                col.var = 'red', col.ind = 'white', #USED TO SEE ONLY THE VARIBLES WHILE THE INDIVIDUAL OBERVATION ARE MADE WHITE 
                title = 'PCA Biplot')

fviz_pca_biplot(pca_res, repel = TRUE, 
                col.var = 'red', col.ind = 'gray', 
                title = 'PCA Biplot')
########################################################Sparse PCA
install.packages("elasticnet")
library(elasticnet)



# Perform Sparse PCA
# Here, we set K = 2 for two components and use a moderate sparsity parameter
spca_res <- spca(combined_data_scaled, K = 2, sparse = "penalty", para = c(0.5, 0.5))

# Print the sparse loadings
print(spca_res$loadings)


# Perform Sparse PCA
set.seed(123) # For reproducibility
spca_res <- spca(combined_data_scaled, K = 2, sparse = "penalty", para = c(0.5, 0.5))

# Assuming `combined_data_scaled` is the original scaled data matrix
# Extract loadings
loadings <- spca_res$loadings

# Compute scores as the dot product of the scaled data and loadings
scores <- as.data.frame(combined_data_scaled %*% loadings)

# Assign appropriate column names
colnames(scores) <- paste0("PC", 1:ncol(scores))

# View the first few rows
head(scores)


# Convert to data frame for ggplot
scores_df <- as.data.frame(scores)
loadings_df <- as.data.frame(loadings)
colnames(scores_df) <- c("PC1", "PC2")
colnames(loadings_df) <- c("PC1", "PC2")

# Plotting
ggplot() +
  # Plot the scores (individuals)
  geom_point(data = scores_df, aes(x = PC1, y = PC2), color = "gray", alpha = 0.6) +
  
  # Plot the loadings (variables)
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1 * 3, yend = PC2 * 3), 
               arrow = arrow(length = unit(0.3, "cm")), color = "red") +
  geom_text(data = loadings_df, aes(x = PC1 * 3, y = PC2 * 3, label = rownames(loadings_df)), 
            color = "red", vjust = -0.5) +
  
  labs(title = "Sparse PCA Biplot", x = "PC1", y = "PC2") +         #PLOTS STILL NOT GOOD 
  theme_minimal()
#######################################################################################################################################
## Load necessary library
library(dplyr)

# Create depth group variable
combined_data_pca <- combined_data_pca %>%
  mutate(Depth_Group = case_when(
    INDEX_SITE_DEPTH < 2 ~ "<2",
    INDEX_SITE_DEPTH >= 2 & INDEX_SITE_DEPTH < 5 ~ "2-4.99",
    INDEX_SITE_DEPTH >= 5 & INDEX_SITE_DEPTH < 10 ~ "5-9.99",
    INDEX_SITE_DEPTH >= 10 ~ ">10"
  ))
names(combined_data_pca)
# Check the new column
head(combined_data_pca$Depth_Group)

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(factoextra)

# Create depth group variable as a factor
combined_data_pca <- combined_data_pca %>%
  mutate(Depth_Group = factor(case_when(
    INDEX_SITE_DEPTH < 2 ~ "<2",
    INDEX_SITE_DEPTH >= 2 & INDEX_SITE_DEPTH < 5 ~ "2-4.99",
    INDEX_SITE_DEPTH >= 5 & INDEX_SITE_DEPTH < 10 ~ "5-9.99",
    INDEX_SITE_DEPTH >= 10 ~ ">10"
  ), levels = c("<2", "2-4.99", "5-9.99", ">10")))

# Check the structure of the new column
str(combined_data_pca$Depth_Group)

# Perform PCA
# Ensure the dataset only includes numeric columns for PCA
data_for_pca <- combined_data_pca %>%
  select(-SITE_ID, -Depth_Group)  # Remove non-numeric columns


# Check for missing values
colSums(is.na(data_for_pca))

# Option 1: Remove rows with missing values
data_for_pca_clean <- na.omit(data_for_pca)

# Re-run PCA
pca_res <- prcomp(data_for_pca_clean, scale. = TRUE)

# Check PCA results
summary(pca_res)

# Synchronize rows
combined_data_pca_clean <- combined_data_pca[rownames(data_for_pca_clean), ]

# Check the dimensions to ensure they match
nrow(data_for_pca_clean)
nrow(combined_data_pca_clean)

# Run PCA
pca_res <- prcomp(data_for_pca_clean, scale. = TRUE)

# Add PCA scores to the dataset
pca_scores <- as.data.frame(pca_res$x)
pca_scores$Depth_Group <- combined_data_pca_clean$Depth_Group

# Plot PCA grouped by Depth_Group
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Depth_Group)) +
  geom_point(size = 4, alpha = 0.7) +
  labs(title = "PCA Plot Grouped by Depth Group", x = "PC1", y = "PC2") +
  theme_minimal() +
  theme(legend.position = "right")

###################################################
library(FactoMineR)
library(factoextra)

# Ensure Depth_Group is a factor
combined_data_pca_clean$Depth_Group <- factor(combined_data_pca_clean$Depth_Group)

# Perform PCA
pca_res2 <- PCA(data_for_pca_clean, graph = FALSE)

# Biplot grouped by Depth_Group
fviz_pca_biplot(pca_res2, repel = TRUE, 
                col.var = "black", 
                col.ind = combined_data_pca_clean$Depth_Group,  # Color by Depth_Group
                palette = c("#00AFBB", "#E7B800", "#FC4E07", "#0073C2"),  # Customize colors
                addEllipses = TRUE,  # Add concentration ellipses
                title = "PCA Biplot Grouped by Depth Group")
################################################################################################REGRESSION TREE MODELS###
# Load necessary libraries
library(rpart)
library(rpart.plot)


# Define the predictors
predictors <- combined_data_pca[, c("CHLA_RESULT", "total_phytoplankton_biovolume", "total_cyanobacteria_biovolume", 
                                    "PTOX_biovolume", "percent_cyanobacteria_biovolume", "percent_PTOX_biovolume", 
                                    "Shannon_Index", "Simpson_Index", "Evenness", "Temp_top1m", "Secchi", 
                                    "pH_top1m", "PH_RESULT", "ELEVATION", "INDEX_SITE_DEPTH", 
                                    "AMMONIA_N_RESULT", "ANC_RESULT", "CALCIUM_RESULT", "CHLORIDE_RESULT", 
                                    "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", "NTL_RESULT", 
                                    "PTL_RESULT", "NTL_DISS_RESULT", "PTL_DISS_RESULT", "SODIUM_RESULT", 
                                    "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")]

# Response variable
response <- combined_data_pca$MICX

# Combine predictors and response into one dataset
data_for_tree <- data.frame(MICX = response, predictors)

# Remove rows with missing values
data_for_tree <- na.omit(data_for_tree)

# Fit the regression tree model
set.seed(123)  # For reproducibility
tree_model <- rpart(MICX ~ ., data = data_for_tree, method = "anova")

# Plot the regression tree
rpart.plot(tree_model, main = "Regression Tree for MICX", type = 3, extra = 101, under = TRUE) #GOOD PLOT

# Display the model summary
print(tree_model)

# Complexity parameter table to determine optimal tree size
printcp(tree_model)

# Plot cross-validation results
plotcp(tree_model)

# Prune the tree based on optimal CP
optimal_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]
pruned_tree <- prune(tree_model, cp = optimal_cp)

# Plot the pruned tree
rpart.plot(pruned_tree, main = "Pruned Regression Tree for MICX", type = 3, extra = 101, under = TRUE)

#####################################################################################################################repeat FOR CYLSPER
# Response variable
response2 <- combined_data_pca$CYLSPER

# Combine predictors and response into one dataset
data_for_tree2 <- data.frame(CYLSPER = response2, predictors)

# Remove rows with missing values
data_for_tree2 <- na.omit(data_for_tree2)

# Fit the regression tree model
set.seed(123)  # For reproducibility
tree_model2 <- rpart(CYLSPER ~ ., data = data_for_tree2, method = "anova")

# Plot the regression tree
rpart.plot(tree_model2, main = "Regression Tree for CYLSPER", type = 3, extra = 101, under = TRUE) #GOOD PLOT

# Display the model summary
print(tree_model2)

# Complexity parameter table to determine optimal tree size
printcp(tree_model2)

# Plot cross-validation results
plotcp(tree_model2)

# Prune the tree based on optimal CP
optimal_cp2 <- tree_model2$cptable[which.min(tree_model2$cptable[,"xerror"]), "CP"]
pruned_tree2 <- prune(tree_model2, cp = optimal_cp2)

# Plot the pruned tree
rpart.plot(pruned_tree2, main = "Pruned Regression Tree for CYLSPER", type = 3, extra = 101, under = TRUE)
####################################################################################################################################cyanobacteria/CHLA/PHYTOPLANKON/genus species-eg microcystis
##remeber to replace group with biovolumes
names(combined_data_pca)
# Define the predictors
predictors_phy <- combined_data_pca[, c("Temp_top1m", "Secchi", 
                                    "pH_top1m", "PH_RESULT", "ELEVATION", "INDEX_SITE_DEPTH", 
                                    "AMMONIA_N_RESULT", "ANC_RESULT", "CALCIUM_RESULT", "CHLORIDE_RESULT", 
                                    "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", "NTL_RESULT", 
                                    "PTL_RESULT", "NTL_DISS_RESULT", "PTL_DISS_RESULT", "SODIUM_RESULT", 
                                    "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")]

# Response variable
response3 <- combined_data_pca$DOLICHOSPERMUM

# Combine predictors and response into one dataset
data_for_tree3 <- data.frame(DOLICHOSPERMUM = response3, predictors_phy)

# Remove rows with missing values
data_for_tree <- na.omit(data_for_tree3)

# Fit the regression tree model
set.seed(123)  # For reproducibility
tree_model3 <- rpart(DOLICHOSPERMUM ~ ., data = data_for_tree3, method = "anova")

# Plot the regression tree
rpart.plot(tree_model3, main = "Regression Tree for DOLICHOSPERMUM group", type = 3, extra = 101, under = TRUE) #GOOD PLOT ##remeber to replace group with biovolumes

# Display the model summary
print(tree_model3)

# Complexity parameter table to determine optimal tree size
printcp(tree_model3)

# Plot cross-validation results
plotcp(tree_model3)

# Prune the tree based on optimal CP
optimal_cp3 <- tree_model3$cptable[which.min(tree_model3$cptable[,"xerror"]), "CP"]
pruned_tree3 <- prune(tree_model3, cp = optimal_cp3)

# Plot the pruned tree
rpart.plot(pruned_tree3, main = "Pruned Regression Tree for MICX", type = 3, extra = 101, under = TRUE)

##remeber to replace group with biovolumes, and see if the trees are the same or different
##################################################################################################################################################trying GAMM MODELS

# FIXME
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(ggplot2)
library(mgcv)
library(fitdistrplus)


#Make-SITE_ID FACTOR
combined_data_pca$SITE_ID <-as.factor(combined_data_pca$SITE_ID)

# Remove NA values and convert MICX to a vector
x <- as.vector(na.omit(combined_data_pca$MICX))
# Fit a log-normal distribution
fit_log <- fitdist(x, distr = "lnorm", method = "mle") #fits a bit better then just gamma?
# Plot the fitted log-normal distribution
plot(fit_log)



#fist model for MIC #THIS ACTUALLY TAKES A LONG TIME TO RUN!#stopped the iteration at 63 after over 4hours, REDUCED INTERATION TO 10. THE R2 IS -VE. 
MIC_try <- gamm(
  formula = MICX ~ s(CHLA_RESULT) + s(total_phytoplankton_biovolume) + s(total_cyanobacteria_biovolume) + s(PTOX_biovolume) + s(percent_cyanobacteria_biovolume) + 
    s(Shannon_Index) + s(Simpson_Index) + s(Evenness) + s(Temp_top1m) + s(Secchi) + s(PH_RESULT) + s(ELEVATION) + s(INDEX_SITE_DEPTH) + s(ANC_RESULT) +s(CALCIUM_RESULT) + 
    s(CHLORIDE_RESULT) + s(COLOR_RESULT) + s(COND_RESULT) + s(DOC_RESULT) + s(MAGNESIUM_RESULT) + s(NTL_RESULT) + s(PTL_RESULT) + s(NTL_DISS_RESULT) + s(PTL_DISS_RESULT) +
    s(SODIUM_RESULT) + s(TURB_RESULT) + s(SULFATE_RESULT) + s(POTASSIUM_RESULT), 
  random = list(SITE_ID = ~1),
  family = Gamma(link = "log"),  
  data = combined_data_pca,
  niterPQL = 10) 

#REDUCED INTERATION TO 10. THE R2 OF THE MODEL -VE. 
######################################################################TRY AGAIN WITH GAUSSIAN FAMILY
# Apply log transformation to the response variable
combined_data_pca$log_MICX <- log(combined_data_pca$MICX + 1)  # Adding 1 to handle zeros

# Fit the model with Gaussian family and log link
MIC_try2 <- gamm(
  formula = log_MICX ~ s(CHLA_RESULT) + s(total_phytoplankton_biovolume) + s(total_cyanobacteria_biovolume) + s(PTOX_biovolume) + 
    s(percent_cyanobacteria_biovolume) + s(Shannon_Index) + s(Simpson_Index) + s(Evenness) + s(Temp_top1m) + s(Secchi) + 
    s(PH_RESULT) + s(ELEVATION) + s(INDEX_SITE_DEPTH) + s(ANC_RESULT) + s(CALCIUM_RESULT) + s(CHLORIDE_RESULT) + 
    s(COLOR_RESULT) + s(COND_RESULT) + s(DOC_RESULT) + s(MAGNESIUM_RESULT) + s(NTL_RESULT) + s(PTL_RESULT) + 
    s(NTL_DISS_RESULT) + s(PTL_DISS_RESULT) + s(SODIUM_RESULT) + s(TURB_RESULT) + s(SULFATE_RESULT) + s(POTASSIUM_RESULT), 
  random = list(SITE_ID = ~1),
  family = gaussian(link = "log"),  
  data = combined_data_pca,
  niterPQL = 10
)
#remove log_MICX use the MICX directly
MIC_try <- gamm(
  formula = MICX ~ s(CHLA_RESULT) + s(total_phytoplankton_biovolume) + s(total_cyanobacteria_biovolume) + s(PTOX_biovolume) + 
    s(percent_cyanobacteria_biovolume) + s(Shannon_Index) + s(Simpson_Index) + s(Evenness) + s(Temp_top1m) + s(Secchi) + 
    s(PH_RESULT) + s(ELEVATION) + s(INDEX_SITE_DEPTH) + s(ANC_RESULT) + s(CALCIUM_RESULT) + s(CHLORIDE_RESULT) + 
    s(COLOR_RESULT) + s(COND_RESULT) + s(DOC_RESULT) + s(MAGNESIUM_RESULT) + s(NTL_RESULT) + s(PTL_RESULT) + 
    s(NTL_DISS_RESULT) + s(PTL_DISS_RESULT) + s(SODIUM_RESULT) + s(TURB_RESULT) + s(SULFATE_RESULT) + s(POTASSIUM_RESULT), 
  random = list(SITE_ID = ~1),
  family = gaussian(link = "log"),  
  data = combined_data_pca,
  niterPQL = 10
)
#####################################################much better MIC_try2 r2 here is 0.252.but fixed effects are -ve?? except temp sig +ve
#View GAM summary

#summary.gam(MIC_try$gam) #WITHOUT Log MICX r2 = 46.2, BUT error is high and it may not make sense of the data.
#summary(MIC_try$lme)


#View GAM summary
summary.gam(MIC_try2$gam)

#view LME summary
summary(MIC_try2$lme)


#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MIC_try2, shade_color = "steelblue2") {
  plot(MIC_try2, shade = TRUE, shade.col = shade_color, pages = 8, main = "MIC_try2")}
plot_gam_custom(MIC_try2$gam)

#Plot LME 
plot(MIC_try2$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MIC_try2")
#######################################################################################
#FOR CYLSPER

# Apply log transformation to the response variable
combined_data_pca$log_CYLSPER <- log(combined_data_pca$CYLSPER + 1)  # Adding 1 to handle zeros

# Fit the model with Gaussian family and log link
MIC_try3 <- gamm(
  formula = log_CYLSPER ~ s(CHLA_RESULT) + s(total_phytoplankton_biovolume) + s(total_cyanobacteria_biovolume) + s(PTOX_biovolume) + 
    s(percent_cyanobacteria_biovolume) + s(Shannon_Index) + s(Simpson_Index) + s(Evenness) + s(Temp_top1m) + s(Secchi) + 
    s(PH_RESULT) + s(ELEVATION) + s(INDEX_SITE_DEPTH) + s(ANC_RESULT) + s(CALCIUM_RESULT) + s(CHLORIDE_RESULT) + 
    s(COLOR_RESULT) + s(COND_RESULT) + s(DOC_RESULT) + s(MAGNESIUM_RESULT) + s(NTL_RESULT) + s(PTL_RESULT) + 
    s(NTL_DISS_RESULT) + s(PTL_DISS_RESULT) + s(SODIUM_RESULT) + s(TURB_RESULT) + s(SULFATE_RESULT) + s(POTASSIUM_RESULT), 
  random = list(SITE_ID = ~1),
  family = gaussian(link = "log"),  
  data = combined_data_pca,
  niterPQL = 10
)

#View GAM summary
summary.gam(MIC_try3$gam)

#view LME summary
summary(MIC_try3$lme)


#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MIC_try3, shade_color = "steelblue2") {
  plot(MIC_try3, shade = TRUE, shade.col = shade_color, pages = 8, main = "MIC_try3")}
plot_gam_custom(MIC_try3$gam)

#Plot LME 
plot(MIC_try3$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in MIC_try3")

##########TRY CYSPER AGAIN WITH GAMMA DISTRIBUTION

#get MIC as a vector to check distribution
x<-as.vector(na.omit(combined_data_pca$CYLSPER))
fit <- fitdist(x, distr = "gamma", method = "mle")
plot(fit)

# Fit the model with GAMMA
MIC_try4 <- gamm(
  formula = CYLSPER ~ s(CHLA_RESULT) + s(total_phytoplankton_biovolume) + s(total_cyanobacteria_biovolume) + s(PTOX_biovolume) + 
    s(percent_cyanobacteria_biovolume) + s(Shannon_Index) + s(Simpson_Index) + s(Evenness) + s(Temp_top1m) + s(Secchi) + 
    s(PH_RESULT) + s(ELEVATION) + s(INDEX_SITE_DEPTH) + s(ANC_RESULT) + s(CALCIUM_RESULT) + s(CHLORIDE_RESULT) + 
    s(COLOR_RESULT) + s(COND_RESULT) + s(DOC_RESULT) + s(MAGNESIUM_RESULT) + s(NTL_RESULT) + s(PTL_RESULT) + 
    s(NTL_DISS_RESULT) + s(PTL_DISS_RESULT) + s(SODIUM_RESULT) + s(TURB_RESULT) + s(SULFATE_RESULT) + s(POTASSIUM_RESULT), 
  random = list(SITE_ID = ~1),
  family = Gamma(link = "log"),  
  data = combined_data_pca,
  niterPQL = 10
)

#View GAM summary
summary.gam(MIC_try4$gam)

#view LME summary
summary(MIC_try4$lme)


#Plot GAM with customized color for aesthetic (page number may be adjusted)
plot_gam_custom <- function(MIC_try4, shade_color = "pink") {
  plot(MIC_try4, shade = TRUE, shade.col = shade_color, pages = 8, main = "CYLSPER_try")} #MIC_try4 RENAMED CYLSPER_try
plot_gam_custom(MIC_try4$gam)

#Plot LME 
plot(MIC_try4$lme, cex = 2, pch = 19, main = "Fitted Values Vs. Standardized Residuals in CYLSPER_try") # THIS plot FOR MICTRY4 LOOK MORE BETTER WITH GAMMA AS IT SHOWS DOTTED LIKE WITH NO CLEAR PATTERNS AROUND 0. 


#####################################################################################CONDITIONAL INFERENCE TREE

#conditional inference tree
# Load the necessary libraries

library(party) #install if neccesary
library(dplyr)

# Ensure MICX is numeric
combined_data_pca$MICX <- as.numeric(combined_data_pca$MICX)

# Define the predictors
predictors <- c("CHLA_RESULT", "total_phytoplankton_biovolume", "total_cyanobacteria_biovolume", "PTOX_biovolume", 
                "percent_cyanobacteria_biovolume", "Shannon_Index", "Simpson_Index", "Evenness", "Temp_top1m", 
                "Secchi", "PH_RESULT", "ELEVATION", "INDEX_SITE_DEPTH", "ANC_RESULT", "CALCIUM_RESULT", 
                "CHLORIDE_RESULT", "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", 
                "NTL_RESULT", "PTL_RESULT", "NTL_DISS_RESULT", "PTL_DISS_RESULT", "SODIUM_RESULT", 
                "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")

# Create the formula
formula <- as.formula(paste("MICX ~", paste(predictors, collapse = " + ")))

# Fit the conditional inference tree
micx_ctree <- ctree(formula, data = combined_data_pca)

# Plot the tree
plot(micx_ctree, main = "Conditional Inference Tree for MICX")

# Print the summary of the tree
summary(micx_ctree)
print(micx_ctree) #TO SEE THE RESULTS IN EACH NODES




###FOR CYLSPER####################################################################
# Ensure CYLSPER is numeric
combined_data_pca$CYLSPER <- as.numeric(combined_data_pca$CYLSPER)

# Define the predictors
predictors <- c("CHLA_RESULT", "total_phytoplankton_biovolume", "total_cyanobacteria_biovolume", "PTOX_biovolume", 
                "percent_cyanobacteria_biovolume", "Shannon_Index", "Simpson_Index", "Evenness", "Temp_top1m", 
                "Secchi", "PH_RESULT", "ELEVATION", "INDEX_SITE_DEPTH", "ANC_RESULT", "CALCIUM_RESULT", 
                "CHLORIDE_RESULT", "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", 
                "NTL_RESULT", "PTL_RESULT", "NTL_DISS_RESULT", "PTL_DISS_RESULT", "SODIUM_RESULT", 
                "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")

# Create the formula
formula2 <- as.formula(paste("CYLSPER ~", paste(predictors, collapse = " + ")))

# Fit the conditional inference tree
cylsper_ctree <- ctree(formula2, data = combined_data_pca)
#####################


# Plot the tree
plot(cylsper_ctree, main = "Conditional Inference Tree for CYLSPER")

# Print the summary of the tree
summary(cylsper_ctree)
print(cylsper_ctree) #TO SEE THE RESULTS IN EACH NODES
###################################################################################for cynobacteria biovolume
# Ensure total_cyanobacteria_biovolume is numeric
combined_data_pca$total_cyanobacteria_biovolume <- as.numeric(combined_data_pca$total_cyanobacteria_biovolume)

# Define the predictors
predictors2 <- c("Temp_top1m", 
                "Secchi", "PH_RESULT", "ELEVATION", "INDEX_SITE_DEPTH", "ANC_RESULT", "CALCIUM_RESULT", 
                "CHLORIDE_RESULT", "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", 
                "NTL_RESULT", "PTL_RESULT", "NTL_DISS_RESULT", "PTL_DISS_RESULT", "SODIUM_RESULT", 
                "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")

# Create the formula
formula3 <- as.formula(paste("total_cyanobacteria_biovolume ~", paste(predictors2, collapse = " + ")))

# Fit the conditional inference tree
cyano_ctree <- ctree(formula3, data = combined_data_pca)

# Plot the tree
plot(cyano_ctree, main = "Conditional Inference Tree for cyanobacteria biovolume")

# Print the summary of the tree
summary(cyano_ctree)
print(cyano_ctree) #TO SEE THE RESULTS IN EACH NODES
#######################################################################
#quick summary of the data
summary(combined_data_pca) #Max. MICX = 355.0 # #Max. CYLSPER = 3.550

################################For total phytopklanton 

# Ensure total_phytoplankton_biovolume is numeric
combined_data_pca$total_phytoplankton_biovolume <- as.numeric(combined_data_pca$total_phytoplankton_biovolume)

# Define the predictors
predictors2 <- c("Temp_top1m", 
                 "Secchi", "PH_RESULT", "ELEVATION", "INDEX_SITE_DEPTH", "ANC_RESULT", "CALCIUM_RESULT", 
                 "CHLORIDE_RESULT", "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", 
                 "NTL_RESULT", "PTL_RESULT", "NTL_DISS_RESULT", "PTL_DISS_RESULT", "SODIUM_RESULT", 
                 "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")

# Create the formula
formula4 <- as.formula(paste("total_phytoplankton_biovolume ~", paste(predictors2, collapse = " + ")))

# Fit the conditional inference tree
phyto_ctree <- ctree(formula4, data = combined_data_pca)

# Plot the tree
plot(phyto_ctree, main = "Conditional Inference Tree for total_phytoplankton_biovolume")

# Print the summary of the tree
summary(phyto_ctree)
print(phyto_ctree) #TO SEE THE RESULTS IN EACH NODES
########################################################################For CHLA


# Ensure CHLA_RESULT is numeric
combined_data_pca$CHLA_RESULT <- as.numeric(combined_data_pca$CHLA_RESULT)

# Define the predictors
predictors2 <- c("Temp_top1m", 
                 "Secchi", "PH_RESULT", "ELEVATION", "INDEX_SITE_DEPTH", "ANC_RESULT", "CALCIUM_RESULT", 
                 "CHLORIDE_RESULT", "COLOR_RESULT", "COND_RESULT", "DOC_RESULT", "MAGNESIUM_RESULT", 
                 "NTL_RESULT", "PTL_RESULT", "NTL_DISS_RESULT", "PTL_DISS_RESULT", "SODIUM_RESULT", 
                 "TURB_RESULT", "SULFATE_RESULT", "POTASSIUM_RESULT")


# Remove rows with missing response values
combined_data_pca_clean <- combined_data_pca[!is.na(combined_data_pca$CHLA_RESULT), ]

# Create the formula
formula5 <- as.formula(paste("CHLA_RESULT ~", paste(predictors2, collapse = " + ")))

# Fit the conditional inference tree
CHLA_ctree <- ctree(formula5, data = combined_data_pca_clean)

# Plot the tree
plot(CHLA_ctree, main = "Conditional Inference Tree for CHLA_RESULT")

# Print the summary of the tree
summary(CHLA_ctree)
print(CHLA_ctree) #TO SEE THE RESULTS IN EACH NODES
#############################################################################################Cyanobacteria genus vs MIC AND CYLSPER
#later-use depth group "Depth_Group" 
names(combined_data_pca)

# Ensure MICX is numeric
combined_data_pca$MICX <- as.numeric(combined_data_pca$MICX)

# Define the predictors
predictors3 <- c("CHLA_RESULT", "total_phytoplankton_biovolume", "total_cyanobacteria_biovolume", "PTOX_biovolume", 
                "percent_cyanobacteria_biovolume", "Shannon_Index", "Simpson_Index", "Evenness",
                "ANABAENOPSIS","ANABAENA", "APHANIZOMENON", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM",                  
                "CUSPIDOTHRIX","RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DOLICHOSPERMUM", "GEITLERINEMA",                   
                "GLOEOTRICHIA", "LEPTOLYNGBYA","LIMNOTHRIX", "MERISMOPEDIA", "PHORMIDIUM", "MICROCYSTIS",
                "LYNGBYA","NOSTOC","OSCILLATORIA", "PLANKTOTHRIX","PSEUDANABAENA", "RADIOCYSTIS", "ROMERIA",
                "SNOWELLA", "SPHAEROSPERMOPSIS","SYNECHOCOCCUS", "SYNECHOCYSTIS","WORONICHINIA")


                    


# Create the formula
formula1B <- as.formula(paste("MICX ~", paste(predictors3, collapse = " + ")))

# Fit the conditional inference tree
micx_ctree1b <- ctree(formula1B, data = combined_data_pca)

# Plot the tree
plot(micx_ctree1b, main = "Conditional Inference Tree for MICX")

# Print the summary of the tree
summary(micx_ctree1b)
print(micx_ctree1b) #TO SEE THE RESULTS IN EACH NODES
############################################# CYSPER

# Ensure CYLSPER is numeric
combined_data_pca$CYLSPER <- as.numeric(combined_data_pca$CYLSPER)

# Define the predictors
predictors3 <- c("CHLA_RESULT", "total_phytoplankton_biovolume", "total_cyanobacteria_biovolume", "PTOX_biovolume", 
                 "percent_cyanobacteria_biovolume", "Shannon_Index", "Simpson_Index", "Evenness",
                 "ANABAENOPSIS","ANABAENA", "APHANIZOMENON", "APHANOCAPSA", "ARTHROSPIRA", "CHRYSOSPORUM",                  
                 "CUSPIDOTHRIX","RAPHIDIOPSIS", "CYLINDROSPERMOPSIS", "DOLICHOSPERMUM", "GEITLERINEMA",                   
                 "GLOEOTRICHIA", "LEPTOLYNGBYA","LIMNOTHRIX", "MERISMOPEDIA", "PHORMIDIUM", "MICROCYSTIS",
                 "LYNGBYA","NOSTOC","OSCILLATORIA", "PLANKTOTHRIX","PSEUDANABAENA", "RADIOCYSTIS", "ROMERIA",
                 "SNOWELLA", "SPHAEROSPERMOPSIS","SYNECHOCOCCUS", "SYNECHOCYSTIS","WORONICHINIA")





# Create the formula
formula1C <- as.formula(paste("CYLSPER ~", paste(predictors3, collapse = " + ")))

# Fit the conditional inference tree
micx_ctree1c <- ctree(formula1C, data = combined_data_pca)

# Plot the tree
plot(micx_ctree1c, main = "Conditional Inference Tree for CYLSPER")

# Print the summary of the tree
summary(micx_ctree1c)
print(micx_ctree1c) #TO SEE THE RESULTS IN EACH NODES
####################################################################
#plot with removed CHLA- and other predictors REMAIN ONLY CYANO GENUS- plots makes no sense hence ignore.
######################################################################
