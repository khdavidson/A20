# esacpement tracking table 

library(tidyverse)
library(writexl)


analysis_year <- 2023

# Create empty table 
a20tbl <- data.frame(
  year=analysis_year,
  area="20",
  system=c("Gordon", "San Juan", "Renfrew", "Harris", "Lens"),
  species=c("Chinook", "Coho", "Chum", "Sockeye", "Pink")) %>% 
  complete(year, area, system, species) %>%
  mutate(Access_DB = NA,
         NewEscIndex = NA,
         WCVIStreamSummary = NA,
         NuSEDS = NA)



# Export
# write_xlsx(a20tbl, path=paste0("//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/ESCAPEMENT/Data/",
#                                analysis_year,
#                                "/SILs/Area 20/Area 20 Final escapement tracker ",
#                                analysis_year,
#                                ".xlsx"
#                                ))

# Locked for editing until 2024! 

