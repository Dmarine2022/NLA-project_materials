##Ongoing work: update regularly
#Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr) #for correlation tests
library(lubridate)


#FOR CLEAN NAMES (USE LATER) e.g. clean_names(poorly_named_df)
library(janitor)


#2017 DATA (landscape data not usable, we may not need it as we already got elevation and others in site information data)
#Load in rawdata from Github ##remember to use that raw link (this appears to create a one time token, that have to be repeaat everytime######

##NLA17_waterchem data
WaterChem2017 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2017_dataset/nla_2017_water_chemistry_chla-data.csv?token=GHSAT0AAAAAAC65NYVZWFWVLD6ARFIHLZ7E2BDRAEA')

##NLA17_Toxin data
toxin2017 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2017_dataset/nla_2017_algal_toxin-data.csv?token=GHSAT0AAAAAAC65NYVZ2FXXUCVVJLHGQC4Q2BDGEAQ')

##NLA17_Secchi data
secchi2017 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2017_dataset/nla_2017_secchi-data.csv?token=GHSAT0AAAAAAC65NYVY2PANMN7M4D6TQQ7U2BDGFWQ')

##NLA17_profile data
#file too large to load using raw from Github it always download on the local machine. 
profile2017 <- read_csv("C:/Users/Yusuf_Olaleye1/Downloads/nla_2017_profile-data.csv")

##NLA17_siteinfo data
siteinfo2017 <- read_csv('https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2017_dataset/nla_2017_site_information-data.csv?token=GHSAT0AAAAAAC65NYVYYX6MD3CMTVNZFGJI2BDGV4A')

#NLA17_Phytoplankton data
phytoplanktoncount2017_data <- read.csv("https://raw.githubusercontent.com/Dmarine2022/NLA-project_materials/refs/heads/main/NLA2017_dataset/nla_2017_phytoplankton_count-data.csv?token=GHSAT0AAAAAAC65NYVZKFQ5UKIHSUN7TZLQ2BDGYBA")  

#############################################################

#Check2
# check the column names
##2017
names(WaterChem2017)
names(toxin2017)
names(secchi2017)
names(profile2017)
names(siteinfo2017)
names(phytoplanktoncount2017_data)

#pIVOT WHERE NECESSARY##################

#Pivot waterchem data (remember chl-a is inside)
#Note: BATCH_ID in the ANALYTE is null, but causing error. bELOW ARE STEPS TAKEN TO RESOLVE THE ISSUE.
# Check for instances where ANALYTE is "BATCH_ID"
WaterChem2017 %>% 
  filter(ANALYTE == "BATCH_ID")
# Remove rows where ANALYTE is "BATCH_ID"
WaterChem2017 <- WaterChem2017 %>% 
  filter(ANALYTE != "BATCH_ID")


# Aggregate by taking the mean of RESULT
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

names(WaterChem2017_wide)
WaterChem2017_subset <- WaterChem2017_wideb %>%         #Note the waterchem data here is the aggregated one
  select(UID, SITE_ID, DATE_COL, AMMONIA_N, ANC, CALCIUM, CHLA, #Visit has been taken off
         CHLORIDE, COLOR, COND, DOC, MAGNESIUM, NITRATE_N, 
         NITRATE_NITRITE_N, NITRITE_N, NTL, PH, POTASSIUM,
         PTL, SODIUM, SULFATE, TURB)


#Select Toxin
#2017
names(toxin2017_wide)
toxin2017_subset <- toxin2017_wide %>% 
  select(UID, SITE_ID, DATE_COL, VISIT_NO, MICX, CYLSPER)


# Select SECCHI
#2017
names(secchi2017_cal)  

secchi2017_sebset <- secchi2017_cal %>% 
  select(UID, SITE_ID, DATE_COL, VISIT_NO, Secchi, INDEX_SITE_DEPTH)



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
  select(UNIQUE_ID, SITE_ID, AREA_HA, ELEVATION,LAKE_ORGN, LAT_DD83, LON_DD83) #UID was showing many NA(i.e. empty cells) so we use the UNIQUE_ID here instead. Joining using SITE_D alone. 


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
                   "MICROSEIRA", "OSCILLATORIA","SPHAEROSPERMOPSIS","UMEZAKIA")


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
    percent_PTOX_biovolume = (PTOX_biovolume / total_cyanobacteria_biovolume) * 100  # % PTOX biovolume relative to total_cyanobacteria_biovolume
  )

# View
print(phyto2017_summary)





#View the first few rows
#2017
head(WaterChem2017_subset)
head(toxin2017_subset)
head(secchi2017_sebset)
#head profiles
head(mean_profiles2017_alldepths)
head(mean_profiles2017_top1m) #Note: I will likely be using this top1m in the combined dataset
head(mean_profiles2017_1to2m)
head(mean_profiles2017_2to4m)
head(mean_profiles2017_below4m)
head(mean_profiles2017_below5m)
##siteinfo2017
head(siteinfo2017_subset)
#Phyto
head(phyto2017_summary)





#count missing values (NAs)
colSums(is.na(WaterChem2017_subset))
colSums(is.na(toxin2017_subset))
colSums(is.na(secchi2017_sebset))
#profiles
colSums(is.na(mean_profiles2017_alldepths))
colSums(is.na(mean_profiles2017_top1m))
colSums(is.na(mean_profiles2017_1to2m))
colSums(is.na(mean_profiles2017_2to4m))
colSums(is.na(mean_profiles2017_below4m))
colSums(is.na(mean_profiles2017_below5m))
#siteinfo
colSums(is.na(siteinfo2017_subset)) 
#phyto
colSums(is.na(phyto2017_summary)) 





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

#############################
#############################
####cHECK QUICK RELATIONSHIP PLOTS

combined_data5_NLA2017 %>%
  ggplot(aes(x = Temp_top1m, y = MICX)) + #Change variables as many times as possible
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 1, label.y = 2, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+ #Note: R VALUES IN THE PLOT REPRESENT RHO VALUE.
  theme_bw()
#####################
#DELETED THE REST OF THE PLOTS AND EXPLORATORY CODES. THEY ARE AVAILABE IN 20222 SCRIPTS. 