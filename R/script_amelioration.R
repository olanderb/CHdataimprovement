
# Chargemente des librairies ----------------------------------------------

library(foreign)
library(readxl)
library(tidyverse)
library(haven)
library(labelled)
library(data.table)
library(reshape2)

# Importation de la base et sélection des variables d'intérêt -------------

setwd("C:\\Users\\idrissa.dabo\\OneDrive - World Food Programme\\Documents\\job PAM\\Tchad")
CHD_ENSA_HH_OCTOBRE_2019 <- read_sav("3_ProcessedData/CHD_ENSA_HH_OCTOBRE_2019_Final_analyse_partage.sav")
CHD_ENSA_HH_OCTOBRE_2019<- to_factor(CHD_ENSA_HH_OCTOBRE_2019)

remove_attributes <- function(x) {
  attr(x, "label") <- NULL
  attr(x, "format.spss") <- NULL
  attr(x, "display_width") <- NULL
  x
}

CHD_ENSA_HH_OCTOBRE_2019 %>% mutate_if(is.labelled, ~ as.character(as_factor(.x))) %>%  mutate_all(funs(remove_attributes)) -> CHD_ENSA_HH_OCTOBRE_2019


data <- CHD_ENSA_HH_OCTOBRE_2019  %>%select(adm1_name = region,
                                            adm2_name = Strate_analyse,
                                            HDDS = HDDS,
                                            FCG = Gpe_SCA_Calcule_CH,
                                            HHS = Echelle_faim_cor,
                                            rCSI = rCSI,
                                            LHCS = Strategie_moyen_existence)
                                            # choc_subi=choc_subi,
                                            # nature_choc = nature_choc,
                                            # contracte_dette = contracte_dette,
                                            # possibilite_contrate_dette = possibilite_contrate_dette,
                                            # biens_non_productif = biens_non_productif,
                                            # comparaison_campagne = comparaison_campagne,
                                            # energie_cuisson_aliment,
                                            # source_eau_boisson = source_eau_boisson,
                                            # type_toilette = type_toilette)

# Les différents Facteurs contributifs
# Pendant les six dernier mois, le ménage a-t-il subi un choc ? choc_subi
# Quel est le type de choc subis? nature_choc
# Avez-vous contracté des dettes que vous devez rembourser ? contracte_dette
# Avez-vous actuellement la possibilité de contracter une dette auprès de quelqu'un/d'une structure en cas de besoin ? possibilite_contrate_dette
# Avez-vous vendu des biens non productifs (ex. mobilier, bijoux, etc.) par manque de nourriture? biens_non_productif
# Comment jugez-vous votre production de la campagne agricole en cours par rapport à la campagne 2018/2019 ? comparaison_campagne
# Quelle est la principale source d'énergie pour la cuisson des aliments dans votre ménage ? energie_cuisson_aliment
# Quelle est la principale source d'eau de boisson de votre ménage ? source_eau_boisson
# Quel est le principal type de toilette que votre ménage utilise ? type_toilette



# recoder les modalités des variables -------------------------------------

data$FCG <- fct_recode(data$FCG,Poor ="Pauvre",Bordeline="Limite",Acceptable="Acceptable")

data$CH_HHS <- fct_recode(data$HHS,Phase1 = "Nulle",Phase2="Faible",Phase3="Modere",
                       Phase4="Grave",Phase5="Tres grave")

data$LHCS <- fct_recode(data$LHCS,"No Strategies"="Aucune strategie",
                        "Stress Strategies"="Strategie de stress","Crisis Stategies"="Strategie de crise",
                        "Emergency Strategies"="Strategie durgence")

data <- data %>%
  mutate(CH_HDDS = case_when(
    HDDS >= 5 ~ "Phase1", 
    HDDS == 4 ~ "Phase2",       
    HDDS == 3 ~ "Phase3",
    HDDS == 2 ~ "Phase4",
    HDDS < 2 ~ "Phase5"
  ),
  CH_rCSI = case_when(
    rCSI <= 3 ~ "Phase1", 
    rCSI >= 4 & rCSI <= 18 ~ "Phase2",       
    rCSI >= 19 ~ "Phase3" ))

# Tableaux des indicateurs de preuves directes ----------------------------

table_names <- c("HDDStable","FCGtable","HHStable","rCSItable","LHCStable")
indicator_names <- list("CH_HDDS","FCG","CH_HHS","CH_rCSI","LHCS")

data <- data.frame(data)
for (i in 1:length(table_names)) {
  nameTab <- table_names[i]
  indname <- indicator_names[[i]]
  attach(data)
  b <- aggregate(data[,indname],by=list(adm1_name = data$adm1_name,adm2_name = data$adm2_name,n=data$indname),FUN= length)
 attach(b)
  colnames(b)[4] <- "x"
  d <- dcast(b,adm1_name+adm2_name~n,value.var = "x")
  d <- d %>% replace(., is.na(.), 0) %>% 
    mutate_if(is.numeric,prop.table(1))
  d <-cbind(d[1:2], round(100*d[-(1:2)]/rowSums(d[-(1:2)]),1))
  assign(nameTab,d)
}


attach(b)
b <- aggregate(data[,"LHCS"],by=list(adm1_name = data$adm1_name,adm2_name = data$adm2_name,n=data$LHCS),FUN= length)
attach(b)
colnames(b)[4] <- "x"
d <- dcast(b,adm1_name+adm2_name~n,value.var = "x")
d <- d %>% replace(., is.na(.), 0)
 d <-cbind(d[1:2], round(100*d[-(1:2)]/rowSums(d[-(1:2)]),1))

Ind_table <- function(base,t){
  as.data.frame(base)
  attach(base)
  y <- aggregate(base[,t],by=list(adm1_name = base$adm1_name,adm2_name = base$adm2_name,n=base$t),FUN= length)
  attach(y)
  colnames(y)[4] <- "x"
  z <- dcast(y,adm1_name+adm2_name~n,value.var = "x")
  z <- z %>% replace(., is.na(.), 0)
  z <-cbind(z[1:2], round(100*z[-(1:2)]/rowSums(z[-(1:2)]),1))
  return(z)
}

a <- Ind_table(data,"CH_HDDS")

essai <- function(base,t){
  base <- as.data.frame(base)
  attach(base)
  y <- aggregate(base[,t],by=list(adm1_name = base$adm1_name,adm2_name = base$adm2_name,n=base$t),FUN= length)
  return(y)
}
 
a <- essai(data,"CH_HDDS")

attach(data)
View(data[,"CH_HDDS"]) 
 
ncol(data$CH_HDDS) 

attach(data)
 
   
