# Livescope estimate calculation 
# January 2024


# Load libraries ---------------------------
library(tidyverse)
library(here)
library(readxl)
library(imputeTS)


# Load data ---------------------------






######################################################################################################################################################

# =============== 1. INFILLING MISSING HOUR COUNTS ===============













# =============== 2. NET U/S CALC ===============

LSC.net <- LSC %>% 
  group_by(Date, Bank, `Count Hour`, `Portion of hour`) %>% 
  summarize(`Time counted` = unique(`Time counted`),
            netUS = count_US-count_DS, 
            hourlyNetUS = case_when(`Time counted` > 0 ~ (netUS/`Time counted`)*60,
                                    TRUE ~ 0),
            `#counts` = unique(`Obs Count #`)) %>% 
  print()




