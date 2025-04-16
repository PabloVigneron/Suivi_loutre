################################################################################
###                     ANALYSE - LOUTRE - PROTOCOLE                         ###  
################################################################################

################################################################################
### Import packages
library(tidyverse)
library(gridExtra)
library(lubridate)

################################################################################
### Data importation
greve_points <-
  readxl::read_xls(path = 'raw_data/Suivi loutre Lieue de Greve octobre 24.xls', range = 'A2:H66') %>% 
  select(-`Présence de marquage`)


  
  
  