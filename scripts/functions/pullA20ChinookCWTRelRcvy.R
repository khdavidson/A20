
# Dump and combine Area 20 CWT release/recovery data 


# Load libraries ------------------------
library(saaWeb)
library(tidyverse)
library(writexl)
library(here)

# Helper ------------------------
"%notin%" <- Negate("%in%")




# ============================ 1. DUMP CWT RECOVERIES (release JOIN recovery) ============================

# Chinook release tagcodes ------------------------
a20cwtRcvy <- saaWeb:::runCwtExtractorQuery(here("scripts", "json", "mrpExtractor_RelRcvy_SJ-gordon-lens-CN.json"), config_file=here("saaWeb.config"), 
                                             user_name = NULL, password=NULL) %>%
  print()





# ============================ 2. EXPORT ============================


# Export to github ------------------------
writexl::write_xlsx(a20cwtRcvy, here("outputs", paste0("R_OUT - Area 20 CWT Recoveries JOIN Releases - CHINOOK RY", 
                                                       min(a20cwtRcvy$`(RC) Recovery Year`), 
                                                       "-", 
                                                       max(a20cwtRcvy$`(RC) Recovery Year`), 
                                                       ".xlsx")))


















