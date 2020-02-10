# Load required packages (if you dont have them use install.packag --------

## Load required packages (if you dont have them use install.packages("nameofpackage") first 
library(haven)
library(tidyverse)
library(openxlsx)
library(readxl)
library(labelled)


#' Create a codebook
#'
#' @param data data.frame
#' @return data.frame with two columns: (name, value)
#' @examples \dontrun{
#' library(labelled)
#' library(haven)
#' 
#' codebook <- read_sav("MLI_201909_ENSAN_external.sav") %>%
#'   to_factor() %>%
#'   create_codebook()
#' }
#' @importFrom tidyverse %>%
create_codebook <- function(data) {
  var_label(data) %>%
    unlist(recursive = FALSE) %>% 
    enframe() %>% 
    unnest()
}


#' Standardize names of key variables using WFP VAM's Assessment Codebook
#' 
#' @param data data.frame
#' @return data.frame
#' @examples \dontrun{
#' codebook <- read_sav("MLI_201909_ENSAN_external.sav") %>%
#'  standardize_names()
#' }
#' @importFrom tidyverse %>%
standardize_names <- function(data) {
  data %>% 
    select(
      ADMIN1Name = q11a_nom_region,  # First administrative division
      ADMIN2Name = q12_nom_cercle,   # Second administrative division
      HDDScore = SDAM_ENSAN022018,   # Household Dietary Diversity Score
      FCSCat = FCClass,  # Food Consumption Groups from the Food Consumption Score 21/35 - normal threshold
      HHScore = HHSscore,            # Household Hunger Score
      rCSIScore = CSI_reduit,        # Reduced coping strategies
      LhHCSCat = max_coping_strat,   # Livelihood coping strategies
      WeightHHS = Weigth_new1,       # Survey WeightHHSs - if none - delete this line
      choc_subi = q101a_chocs_subis_derniers6_mois  # One example of contributing factor
  )
}



#Create a table then a graph of Food Consumption Groups by Admin1 and Admin2 - this is too complicated
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
