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
