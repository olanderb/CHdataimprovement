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

#standardize names of key variables using WFP VAM's Assesment Codebook
data <- data  %>% select(ADMIN1Name = q11a_nom_region, #first adminstrative division
                         ADMIN2Name = q12_nom_cercle, #second administrative division
                         HDDScore = SDAM_ENSAN022018, #Household Dietary Diversity Score
                         FCSCat = FCClass, #Food Consumption Groups from the Food Consumption Score 21/35 - normal threshold
                         HHScore = HHSscore, #Household Hunger Score
                         rCSIScore = CSI_reduit, #reduced coping strategies
                         LhHCSCat = max_coping_strat, #livelihood coping strategies
                         WeightHHS = Weigth_new1, #survey WeightHHSs - if none - delete this line
                         choc_subi = q101a_chocs_subis_derniers6_mois #one example of contributing factor
)


#Creat a table then a graph of Food Consumption Groups by Admin1 and Admin2 - this is too complicated
FCGtable1 <- data %>%
  group_by(ADMIN1Name, ADMIN2Name) %>%
  count(FCSCat, wt = WeightHHS) %>%
  drop_na() %>%
  mutate(perc = 100 * n / sum(n)) %>% 
  ungroup() %>%
  mutate_if(is.numeric, round, 1) %>% 
  arrange(ADMIN1Name, desc(perc)) %>% 
  mutate(ADMIN1Name = fct_reorder(ADMIN1Name, desc(perc)))


x <- filter(FCGtable1, FCSCat == "Acceptable") %>% 
  rename(perc_acceptable = perc) %>% 
  select(ADMIN1Name, ADMIN2Name, perc_acceptable) %>% 
  distinct() %>% 
  right_join(FCGtable1)

FCSplot <- x %>% ggplot(aes(fill=FCSCat, y=perc, x=reorder(ADMIN2Name, perc_acceptable))) +geom_bar(position="fill", stat="identity")  +theme_minimal() +theme(axis.text.x = element_text(angle = 90)) +scale_fill_manual(values=c("#27AE60","#F1C40F","#C0392B")) + scale_y_continuous(labels = scales::percent)+ theme(axis.title.x = element_blank(),
                                                                                                                                                                                                                                                                                                                   axis.title.y = element_blank()) 
FCSplot <- FCSplot  +ggtitle("Food Consumption Groups of Households by Local Government Area") +theme(plot.title = element_text(hjust = 0.5))
FCSplot <- FCSplot  +facet_grid(~ ADMIN1Name, scales = "free_x")
