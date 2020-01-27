# Load required packages (if you dont have them use install.packag --------

## Load required packages (if you dont have them use install.packages("nameofpackage") first
library(haven)
library(tidyverse)
library(openxlsx)
library(readxl)
library(labelled)

# Import data set and create codebook -------------------------------------

#change directory to where your files are
setwd("C:\\CHdataimprovement")
#import SPSS data set
data  <- read_sav("MLI_201909_ENSAN_external.sav")
#change values to value labels 
data<- to_factor(data)
# create codebook so you can see variables and variable labels
a <- var_label(data)
codebook <- a %>% 
  unlist(recursive = FALSE) %>% 
  enframe() %>% 
  unnest()



# Select and rename the key variables and values used for the analalysis --------

#standardize names of key variables
data <- data  %>% select(ADMIN1Name = q11a_nom_region, #first adminstrative division
                                            ADMIN2Name = q12_nom_cercle, #second administrative division
                                            HDDS = SDAM_ENSAN022018, #Household Dietary Diversity Score
                                            FCSCat = FCClass, #Food Consumption Groups from the Food Consumption Score
                                            HHS = HHSscore, #Household Hunger Score
                                            rCSI = CSI_reduit, #reduced coping strategies
                                            LhHCSCat = max_coping_strat, #livelihood coping strategies
                                            Weight = Weigth_new1, #survey weights - if none - delete this line
                                            choc_subi = q101a_chocs_subis_derniers6_mois #one example of contributing factor
                                            )


# # standardize naming of categorical values -----------------------------

#standardize naming of categorical values and also convert numeric values to Cadre Harmonise thresholds 
data <- data %>%  mutate(FCG = case_when(
  FCG == "Pauvre" ~ "Poor", 
  FCG == "Limite" ~ "Borderline",       
  FCG == "Acceptable" ~ "Acceptable"
),
LhHCSCat = case_when(
  LhHCSCat == "Pas de stratégies" ~ "NoStrategies", # Put accents to stratigies so that the code works properly
  LhHCSCat == "Stratégies de stress" ~ "StressStrategies",
  LhHCSCat == "Stratégies de crise" ~ "CrisisStrategies",
  LhHCSCat == "Stratégies d'urgence" ~ "EmergencyStrategies"
),
CH_HDDS = case_when(
  HDDS >= 5 ~ "Phase1", 
  HDDS == 4 ~ "Phase2",       
  HDDS == 3 ~ "Phase3",
  HDDS == 2 ~ "Phase4",
  HDDS < 2 ~ "Phase5"
),
CH_rCSI = case_when(
  rCSI <= 3 ~ "Phase1", 
  rCSI >= 4 & rCSI <= 18 ~ "Phase2",       
  rCSI >= 19 ~ "Phase3"    
),
CH_HHS =
  case_when(
    HHS == 0 ~ "Phase1",
    HHS == 1 ~ "Phase2",
    HHS == 2 | HHS == 3 ~ "Phase3",
    HHS == 4 ~ "Phase4",  
    HHS >= 5 ~ "Phase5"
  ))



# create tables of the proportion by administrative -----------------------


###create tables of the proportion by administrative areas and then apply the CH indicator specific and 20% rule to each indicator
#Household Dietarty Diversity Score
HDDStable <- data %>% 
  group_by(ADMIN1Name, ADMIN2Name) %>%
  count(CH_HDDS,wt=Weight) %>%
  drop_na() %>%
  mutate(n = 100 * n / sum(n)) %>%
  ungroup() %>%
  spread(key = CH_HDDS, value = n) %>% replace(., is.na(.), 0) %>% mutate_if(is.numeric, round, 1)
#Apply the 20% rule (if it is 20% in that phase or the sum of higher phases equals 20%) 
CH_HDDStable <- HDDStable %>% mutate(indicator = "HDDS", 
                                     phase2345 = `Phase2` + `Phase3` + `Phase4` + `Phase5`, #this variable will be used to see if phase 2 and higher phases equals 20% or more
                                     phase345 = `Phase3` + `Phase4` + `Phase5`, #this variable will be used to see if phase 3 and higher phases equal 20% or more
                                     phase45 = `Phase4` + `Phase5`, #this variable will be used to see if phase 3 and higher phases equal 20% or more
                                     finalphase = case_when(
                                       `Phase5` >= 20 ~ 5, #if 20% or more is in phase 5 then assign phase 5
                                       `Phase4` >= 20 | phase45 >= 20 ~ 4, #if 20% or more is in phase 4 or the sum of phase4 and 5 is more than 20% then assign phase 4
                                       `Phase3` >= 20 | phase345 >= 20 ~ 3, #if 20% or more is in phase 3 or the sum of phase3, 4 and 5 is more than 20% then assign phase 3
                                       `Phase2` >= 20 | phase2345 >= 20 ~ 2, #if 20% or more is in phase 2 or the sum of phase 2, 3, 4 and 5 is more than 20% then assign phase 2
                                       TRUE ~ 1)) %>% #otherwise assign phase 1
                                       select(indicator, ADMIN1Name, ADMIN2Name, HDDS_Phase1 = Phase1, HDDS_Phase2 = Phase2, HDDS_Phase3 = Phase3, HDDS_Phase4 = Phase4, HDDS_Phase5 = Phase5, HDDS_finalphase = finalphase) #select only relevant variables, rename them with indicator name and order in proper sequence

#Food Consumption Groups
FCGtable <- data %>% 
  group_by(ADMIN1Name, ADMIN2Name) %>%
  count(FCG,wt=Weight) %>%
  drop_na() %>%
  mutate(n = 100 * n / sum(n)) %>%
  ungroup() %>%
  spread(key = FCG, value = n) %>% replace(., is.na(.), 0) %>% mutate_if(is.numeric, round, 1) 
#Apply the Cadre Harmonise rules for phasing the Food Consumption Groups 
CH_FCGtable <-  FCGtable %>%  mutate(indicator = "FCG", PoorBorderline = Poor + Borderline, finalphase = case_when(
  Poor < 5 ~ 1,  #if less than 5% are in the poor food group then phase 1
  Poor >= 20 ~ 4, #if 20% or more are in the poor food group then phase 4
  between(Poor,5,10) ~ 2, #if % of people are between 5 and 10%  then phase2
  between(Poor,10,20) & PoorBorderline < 30 ~ 2, #if % of people in poor food group are between 20 and 20% and the % of people who are in poor and borderline is less than 30 % then phase2
  between(Poor,10,20) & PoorBorderline >= 30 ~ 3)) %>% #if % of people in poor food group are between 20 and 20% and the % of people who are in poor and borderline is less than 30 % then phase2
  select(indicator, ADMIN1Name, ADMIN2Name, FCG_Poor = Poor, FCG_Borderline = Borderline, FCG_Acceptable = Acceptable, FCG_finalphase = finalphase) #select only relevant variables and order in proper sequence

#Household Hunger Score
HHStable <- data %>% 
  group_by(ADMIN1Name, ADMIN2Name) %>%
  count(CH_HHS,wt=Weight) %>%
  drop_na() %>%
  mutate(n = 100 * n / sum(n)) %>%
  ungroup() %>%
  spread(key = CH_HHS, value = n) %>% replace(., is.na(.), 0) %>% mutate_if(is.numeric, round, 1)
#Apply the 20% rule (if it is 20% in that phase or the sum of higher phases equals 20%) 
CH_HHStable <- HHStable %>% mutate(indicator = "HHS", phase2345 = `Phase2` + `Phase3` + `Phase4` + `Phase5`,
                                   phase345 = `Phase3` + `Phase4` + `Phase5`,
                                   phase45 = `Phase4` + `Phase5`,
                                   finalphase = case_when(
                                     Phase5 >= 20 ~ 5,
                                     Phase4 >= 20 | phase45 >= 20 ~ 4,
                                     Phase3 >= 20 | phase345 >= 20 ~ 3,
                                     Phase2 >= 20 | phase2345 >= 20 ~ 2,
                                     TRUE ~ 1)) %>% 
                                    select(indicator, ADMIN1Name, ADMIN2Name, HHS_Phase1 = Phase1, HHS_Phase2 = Phase2, HHS_Phase3 = Phase3, HHS_Phase4 = Phase4, HHS_Phase5 = Phase5, HHS_finalphase = finalphase)

#reduced consumption score
rCSItable <- data %>% 
  group_by(ADMIN1Name, ADMIN2Name) %>%
  count(CH_rCSI,wt=Weight) %>%
  drop_na() %>%
  mutate(n = 100 * n / sum(n)) %>%
  ungroup() %>%
  spread(key = CH_rCSI, value = n) %>% replace(., is.na(.), 0) %>% mutate_if(is.numeric, round, 1)
#Apply the 20% rule (if it is 20% in that phase or the sum of higher phases equals 20%) 
CH_rCSItable <- rCSItable %>% mutate(indicator = "rCSI", 
                                     rcsi23 = Phase2 + Phase3,
                                     finalphase =
                                       case_when(
                                         Phase3 >= 20 ~ 3, 
                                         Phase2 >= 20 | rcsi23 >= 20 ~ 2,
                                         TRUE ~ 1
                                       )) %>% select(indicator, ADMIN1Name, ADMIN2Name, rCSI_Phase1 = Phase1, rCSI_Phase2 = Phase2, rCSI_Phase3 =Phase3, rCSI_finalphase = finalphase)


#Livelihood Coping Strategies
LhHCSCattable <- data %>% 
  group_by(ADMIN1Name, ADMIN2Name) %>%
  count(LhHCSCat,wt=Weight) %>%
  drop_na() %>%
  mutate(n = 100 * n / sum(n)) %>%
  ungroup() %>%
  spread(key = LhHCSCat, value = n) %>% replace(., is.na(.), 0) %>% mutate_if(is.numeric, round, 1)
#Apply the Cadre Harmonise rules for phasing the Livelihood Coping Strategies 
CH_LhHCSCattable <- LhHCSCattable %>% mutate(indicator = "LhHCSCat", stresscrisisemergency = StressStrategies + CrisisStrategies + EmergencyStrategies,
                                     crisisemergency = CrisisStrategies + EmergencyStrategies,
                                     finalphase = case_when(
                                       EmergencyStrategies >= 20 ~ 4,
                                       crisisemergency >= 20 & EmergencyStrategies < 20 ~ 3,  
                                       NoStrategies < 80 & crisisemergency < 20 ~ 2,
                                       NoStrategies >= 80 ~ 1
                                     )) %>% select(indicator, ADMIN1Name, ADMIN2Name, LhHCSCat_NoStrategies = NoStrategies, LhHCSCat_StressStrategies = StressStrategies, LhHCSCat_CrisisStategies = CrisisStrategies, LhHCSCat_EmergencyStrategies = EmergencyStrategies, LhHCSCat_finalphase = finalphase)



# Add contributing factors variables --------------------------------------

##Add contributing factors variables (different from the Food Security direct evidence above, these variables will depend country to country)
##so that the contributing factors can be imported into the proper category, the final variable names should be given a prefix (e.g. 01_, 02_)
##"Hazards & Vulnerability" = 01 - 10
##"Availibility" = 11 - 25
##"Accessibility" = 26 - 40
##"Utilization including access to clean water" = 41 - 55
##"Stability" = 56 - 70

#Create a table of the proportion of people who "experienced a shock in the last six months"
choc_subitable <- data %>% 
  group_by(ADMIN1Name, ADMIN2Name) %>%  mutate(indicator = "Pendant les six dernier mois, le menage a-t-il subi un choc?") %>%
  count(choc_subi) %>%
  drop_na() %>%
  mutate(n = 100 * n / sum(n)) %>%
  ungroup() %>%
  spread(key = choc_subi, value = n) %>% replace(., is.na(.), 0) %>% mutate_if(is.numeric, round, 1) %>%
  select(ADMIN1Name, ADMIN2Name, `01_choc_subi_Non` = Non, `01_choc_subi_Oui` = Oui)


# Merge key variables -----------------------------------------------------

### Merge key variables from Direct Evidence and Contributing factor tables together 
matrice_intermediaire <- bind_cols(
  select(CH_FCGtable,-"indicator"),# select all variables except indicator 
  select(CH_HDDStable,-c("indicator","ADMIN1Name","ADMIN2Name")),# select all variables except indicator, ADMIN1Name and ADMIN2Name because the latter two are already selected from the table CH_FCGtable
  select(CH_HHStable,-c("indicator","ADMIN1Name","ADMIN2Name")),
  select(CH_LhHCSCattable,-c("indicator","ADMIN1Name","ADMIN2Name")),
  select(CH_rCSItable,-c("indicator","ADMIN1Name","ADMIN2Name")),
  select(choc_subitable,-c("ADMIN1Name","ADMIN2Name"))
)


# create a blank space for other variables --------------------------------

###create a blank space for other variables that will be used in the excel analyis but do not come from the survey data above
# function for other variables (nutrition, mortality, HEA) not in database
othervariable <- function(x){
  a <- rep(" ",nrow(x))
  return(a)
}

# create variables 
Z1_DPME_C <- othervariable(matrice_intermediaire) # DPME_zone1(courant)
Z1_DPME_pop_C <- othervariable(matrice_intermediaire) #% Pop DPME_Zone1(courant)
Z1_DS_C <- othervariable(matrice_intermediaire) # DS_Zone1(courant)
Z1_Pop_DS_C <- othervariable(matrice_intermediaire) # %Pop DS_Zone1(courant)
Z1_DPME_P <- othervariable(matrice_intermediaire) # DPME_Zone1(projetee)
Z1_Pop_DPME_P <- othervariable(matrice_intermediaire) # %Pop DPME_Zone1(projetee)
Z1_DS_P <- othervariable(matrice_intermediaire) # DS_Zone1(projetee)
Z1_pop_DS_P <- othervariable(matrice_intermediaire) # %Pop DS_Zone1(projetee)

Z2_DPME_C <- othervariable(matrice_intermediaire) # DPME_zone2(courant)
Z2_DPME_pop_C <- othervariable(matrice_intermediaire) # %Pop DPME_Zone2(courant)
Z2_DS_C <- othervariable(matrice_intermediaire) # DS_Zone2(courant)
Z2_Pop_DS_C <- othervariable(matrice_intermediaire) # %Pop DS_Zone2(courant)
Z2_DPME_P <- othervariable(matrice_intermediaire) # DPME_Zone2(projetee)
Z2_Pop_DPME_P <- othervariable(matrice_intermediaire) # %Pop DPME_Zone2(projetee)
Z2_DS_P <- othervariable(matrice_intermediaire) # DS_Zone2(projetee)
Z2_pop_DS_P <- othervariable(matrice_intermediaire) # %Pop DS_Zone2(projetee)

Z3_DPME_C <- othervariable(matrice_intermediaire) # DPME_zone3(courant)
Z3_DPME_pop_C <- othervariable(matrice_intermediaire) #% Pop DPME_Zone3(courant)
Z3_DS_C <- othervariable(matrice_intermediaire) # DS_Zone3(courant)
Z3_Pop_DS_C <- othervariable(matrice_intermediaire) # %Pop DS_Zone3(courant)
Z3_DPME_P <- othervariable(matrice_intermediaire) # DPME_Zone3(projetee)
Z3_Pop_DPME_P <- othervariable(matrice_intermediaire) # %Pop DPME_Zone3(projetee)
Z3_DS_P <- othervariable(matrice_intermediaire) # DS_Zone3(projetee)
Z3_pop_DS_P <- othervariable(matrice_intermediaire) # %Pop DS_Zone3(projetee)

Z4_DPME_C <- othervariable(matrice_intermediaire) # DPME_zone4(courant)
Z4_DPME_pop_C <- othervariable(matrice_intermediaire) #% Pop DPME_Zone4(courant)
Z4_DS_C <- othervariable(matrice_intermediaire) # DS_Zone4(courant)
Z4_Pop_DS_C <- othervariable(matrice_intermediaire) # %Pop DS_Zone4(courant)
Z4_DPME_P <- othervariable(matrice_intermediaire) # DPME_Zone4(projetee)
Z4_Pop_DPME_P <- othervariable(matrice_intermediaire) # %Pop DPME_Zone4(projetee)
Z4_DS_P <- othervariable(matrice_intermediaire) # DS_Zone1(projetee)
Z4_pop_DS_P <- othervariable(matrice_intermediaire) # %Pop DS_Zone1(projetee)

Proxy_cal <- othervariable(matrice_intermediaire) # Proxy calorique
MAG_pt <- othervariable(matrice_intermediaire) # MAG-P/T
MAG_Pharv <- othervariable(matrice_intermediaire) # MAG-Midian
MAG_Soud <- othervariable(matrice_intermediaire) # MAG-Midiane soudure
IMC <- othervariable(matrice_intermediaire) # IMC
MUAC <- othervariable(matrice_intermediaire) # MAG-MUAC 
TBM <- othervariable(matrice_intermediaire) # TBM
TMM5 <- othervariable(matrice_intermediaire) # TMM5
Population <- othervariable(matrice_intermediaire) # Population
Geocode <- othervariable(matrice_intermediaire) # Geocode


# Add the other variables to table containing direct evidence and contributing factors  --------

matrice_intermediaire <- cbind(matrice_intermediaire, Z1_DPME_C, Z1_DPME_P, Z1_DPME_pop_C, Z1_DS_C,
                               Z1_DS_P, Z1_Pop_DPME_P, Z1_Pop_DS_C, Z1_pop_DS_P,
                               Z2_DPME_C, Z2_DPME_P, Z2_DPME_pop_C, Z2_DS_C,
                               Z2_DS_P, Z2_Pop_DPME_P, Z2_Pop_DS_C,Z2_pop_DS_P,
                               Z3_DPME_C, Z3_DPME_P, Z3_DPME_pop_C, Z3_DS_C,
                               Z3_DS_P, Z3_Pop_DPME_P, Z3_Pop_DS_C, Z3_pop_DS_P,
                               Z4_DPME_C, Z4_DPME_P, Z4_DPME_pop_C, Z4_DS_C,
                               Z4_DS_P, Z4_Pop_DPME_P, Z4_Pop_DS_C, Z4_pop_DS_P,
                               Proxy_cal, MAG_pt, MAG_Pharv, MAG_Soud,
                               IMC, MUAC, TBM, TMM5,Population,Geocode)

#re-orders the table
matrice_intermediaire <- matrice_intermediaire %>% 
  select(ADMIN1Name,ADMIN2Name,Population,Geocode,everything())


# saving final data as excel sheet ----------------------------------------

write.xlsx(matrice_intermediaire,file = "Matrice_intermediaire.xlsx",sheetName = "Matrice intermidiaire",append = F)


