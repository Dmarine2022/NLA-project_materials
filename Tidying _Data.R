##Ongoing work: update regularly
#Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggpubr) #for correlation tests

#FOR CLEAN NAMES (USE LATER)
library(janitor)



#Load in rawdata from Github ##remember to use that raw link ######
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
#############################
#############################
####cHECK QUICK RELATIONSHIP PLOTS
#Load library
library(ggpubr)

combined_data5b %>%
  ggplot(aes(x = CHLA_RESULT, y = MICX)) + #Change variables as many times as possible
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  stat_cor(method = "spearman", label.x = 0.5, label.y = 2, aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")))+ #Note: R VALUES IN THE PLOT REPRESENT RHO VALUE.
  theme_bw()

#confirm values with
cor.test(combined_data5b$CHLA_RESULT, combined_data5b$MICX, method = "spearman")
