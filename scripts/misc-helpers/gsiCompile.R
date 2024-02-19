# GSI compile 
# Feb 2024


# Load libraries ------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(writexl)




# Load and join data ------------------------------------
gsi.IDs <- full_join(
  readxl::read_excel(here("data", "juvenile", "GSI", "PID20230066(4)_2023_WCVI_FTF_Juv_SS(23)_4_sc520_2024-01-15.xlsx"), 
                     sheet="extraction_sheet") %>% 
    select(indiv, Vial, CatchJulDate, CatchYear),
  
  readxl::read_excel(here("data", "juvenile", "GSI", "PID20230066(4)_2023_WCVI_FTF_Juv_SS(23)_4_sc520_2024-01-15.xlsx"), 
                     sheet="collection_table_ids") 
) %>% 
  rename(DNA_vial = Vial) %>% 
  left_join(.,
            readxl::read_excel(here("data", "juvenile", "GSI", "PID20230066(4)_2023_WCVI_FTF_Juv_SS(23)_4_sc520_2024-01-15.xlsx"), 
                               sheet="species_ID") %>%
              select(indiv, species)) %>%
  left_join(., 
            readxl::read_excel(here("data", "juvenile", "GSI", "PID20230066(4)_2023_WCVI_FTF_Juv_SS(23)_4_sc520_2024-01-15.xlsx"), 
                               sheet="sex_ID") %>%
              select(indiv, sex_ID, notes)) %>% 
  rename(genetic_species = species)





