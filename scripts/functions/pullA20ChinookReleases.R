
# Dump and combine Area 20 CWT release/recovery data 


# Load libraries ------------------------
library(tidyverse)


# Helper ------------------------
"%notin%" <- Negate("%in%")




# ============================ 1. DUMP CWT RECOVERIES (release JOIN recovery) ============================

# Chinook release tagcodes ------------------------
a20Releases <- saaWeb:::runCwtExtractorQuery(here::here("scripts", "json", "mrpExtractor_Releases_SJ-gordon-lens-CN.json"), config_file=here("saaWeb.config"), 
                                             user_name = NULL, password=NULL) %>%
  print()





# ============================ 2. EXPORT ============================


# Export to github ------------------------
writexl::write_xlsx(a20Releases, here::here("outputs", paste0("R_OUT - Area 20 MRP Releases ", 
                                                       min(a20Releases$`Brood Year`), 
                                                       "-", 
                                                       max(a20Releases$`Brood Year`), 
                                                       ".xlsx")))


















