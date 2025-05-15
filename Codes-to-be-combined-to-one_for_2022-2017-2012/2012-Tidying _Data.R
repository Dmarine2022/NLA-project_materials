
#cOPIED FROM 2017 FOR 2012 DATA-find and replace all done!
##Ongoing work: update regularly
#Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr) #for correlation tests
library(lubridate)


#FOR CLEAN NAMES (USE LATER) e.g. clean_names(poorly_named_df)
library(janitor)


#2012 DATA 
#Load in rawdata from Github ##remember to use that raw link (this appears to create a one time token, that have to be repeated everytime)######

##NLA12_waterchem data
WaterChem2012 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2012_dataset/nla2012_waterchem_wide.csv?token=GHSAT0AAAAAAC65NYVZQMRAPXOWNXMTYQ6S2BFADUQ')

##NLA12_CHLA data
CHLA2012 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2012_dataset/nla2012_chla_wide.csv?token=GHSAT0AAAAAAC65NYVZTKSYTHKLPIZJN5SE2BFAWBA')

##NLA12_Toxin data (MICROCYSTINS ONLY)
toxin2012 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2012_dataset/nla2012_algaltoxins_08192016.csv?token=GHSAT0AAAAAAC65NYVZVJX5K24HZ2FHYBDY2BFBAIA')

##NLA17_Secchi data
secchi2012 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2012_dataset/nla2012_secchi_08232016.csv?token=GHSAT0AAAAAAC65NYVYSZSDYTR4VKNXBQ2G2BFBD5A')

##NLA12_profile data
profile2012 <- read_csv("https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2012_dataset/nla2012_profile_wide.csv?token=GHSAT0AAAAAAC65NYVZUSOK5JOVL3YUTKT62BFBFXA")

##NLA12_siteinfo data
siteinfo2012 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2012_dataset/nla2012_wide_siteinfo_08232016.csv?token=GHSAT0AAAAAAC65NYVY7MK2NGNFD7VFRTJC2BFBIZA')

#NLA12_Phytoplankton data
phytoplanktoncount2012_data <- read.csv("https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2012_dataset/nla2012_wide_phytoplankton_count_02122014.csv?token=GHSAT0AAAAAAC65NYVYSH3MC6DEWNO5SBLE2BFBPYQ")  

#NLA12_watershed data available on Github file, but i'm not sure we need anything in it as elevation, area and other relevant column are already in site information data. Also we dont have landscaope/watershed dataset for 2017.
#############################################################

#Check2
# check the column names
##2012
names(WaterChem2012)
names(CHLA2012)
names(toxin2012)
names(secchi2012)
names(profile2012)
names(siteinfo2012)
names(phytoplanktoncount2012_data)

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
  select(UID, AMMONIA_N_RESULT, ANC_RESULT, CALCIUM_RESULT, #NO CHL-A here
         CHLORIDE_RESULT, COLOR_RESULT, COND_RESULT, DOC_RESULT, MAGNESIUM_RESULT, NITRATE_N_RESULT, 
         NITRATE_NITRITE_N_RESULT, NITRITE_N_RESULT, NTL_RESULT, PH_RESULT, POTASSIUM_RESULT,
         PTL_RESULT, SODIUM_RESULT, SULFATE_RESULT, TURB_RESULT)

#Select CHL-A
#2012

names(CHLA2012)
CHLA2012_subset <- CHLA2012 %>% 
  select(UID, CHLX_RESULT)


#Select Toxin
#2012
names(toxin2012)
toxin2012_subset <- toxin2012 %>% 
  select(UID, SITE_ID, VISIT_NO, DATE_COL, MICX_RESULT)


# Select SECCHI
#2012
names(secchi2012)  
secchi2012_sebset <- secchi2012 %>% 
  select(UID, SITE_ID, DATE_COL, VISIT_NO, SECCHI)


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
#2012 -(INDEX_SITE_DEPTH not found in site information data but is available in secchi data)
names(siteinfo2012) 
siteinfo2012_subset <- siteinfo2012 %>% 
  select(UID, SITE_ID, AREA_HA, ELEVATION,LAKE_ORIGIN, LAT_DD83, LON_DD83) 








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
    percent_PTOX_biovolume = (PTOX_biovolume / total_cyanobacteria_biovolume) * 100  # % PTOX biovolume relative to total_cyanobacteria_biovolume
  )

# View
print(phyto2012_summary)




#View the first few rows
#2012
head(WaterChem2012_subset)
head(CHLA2012_subset)
head(toxin2012_subset)
head(secchi2012_sebset)
#head profiles
head(mean_profiles2012_alldepths)
head(mean_profiles2012_top1m) #Note: I will likely be using this top1m in the combined dataset
head(mean_profiles2012_1to2m)
head(mean_profiles2012_2to4m)
head(mean_profiles2012_below4m)
head(mean_profiles2012_below5m)
##siteinfo2012
head(siteinfo2012_subset)
#Phyto
head(phyto2012_summary)




#CHECK NAs
#count missing values (NAs)
colSums(is.na(WaterChem2012_subset))
colSums(is.na(toxin2012_subset))
colSums(is.na(secchi2012_sebset))
#profiles
colSums(is.na(mean_profiles2012_alldepths))
colSums(is.na(mean_profiles2012_top1m))
colSums(is.na(mean_profiles2012_1to2m))
colSums(is.na(mean_profiles2012_2to4m))
colSums(is.na(mean_profiles2012_below4m))
colSums(is.na(mean_profiles2012_below5m))
#siteinfo
colSums(is.na(siteinfo2012_subset)) 
#phyto
colSums(is.na(phyto2012_summary)) 

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
                                    by = c("UID", "SITE_ID", "DATE_COL"))

#join site information

combined_data5_NLA2012 <- left_join(combined_data4_NLA2012, siteinfo2012_subset, 
                                     by = c("UID", "SITE_ID"),
                                     relationship = "many-to-many") #note

#view the newly combined dataset columns-change print to view
print(combined_data5_NLA2012)

#Join phyto data

combined_data6_NLA2012 <- left_join(combined_data5_NLA2012, phyto2012_summary, 
                                    by = c("SITE_ID", "DATE_COL"),
                                    relationship = "many-to-many") #note
#Check names
names(combined_data6_NLA2012)
#view the newly combined dataset columns-change print to view
print(combined_data6_NLA2012)

#############################
#############################
####cHECK QUICK RELATIONSHIP PLOTS

combined_data6_NLA2012 %>%
  ggplot(aes(x = percent_PTOX_biovolume, y = MICX_RESULT)) + #Change variables as many times as possible
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+ #Note: R VALUES IN THE PLOT REPRESENT RHO VALUE.
  theme_bw()
#####################
#DELETED THE REST OF THE PLOTS AND EXPLORATORY CODES. THEY ARE AVAILABE IN 20222 SCRIPTS. 