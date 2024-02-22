# GSI compile 
# Feb 2024


# Load libraries ------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(writexl)




# Load and join MGL files ------------------------------------
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
  rename(genetic_species = species) %>% 
  print()




# Link juvi biodata + metadata + MGL IDs ------------------------------------
juvi.bioMeta <- left_join(juvi.bio,
                          juvi.meta,
                          na_matches="never",
                          by="usid") %>% 
  left_join(.,
            gsi.IDs,
            by="DNA_vial", na_matches="never") %>% 
  mutate(`(R) ORIGIN` = case_when(ID_Source=="PBT" ~ "Hatchery",
                                  ad_clip=="Y" ~ "Hatchery",
                                  prob.1 >= 75 & grepl("HATCHERY", collection.1) ~ "Hatchery",
                                  grepl("HAT", usid) ~ "Hatchery",
                                  grepl("RST", gear) ~ "Natural",
                                  ID_Source=="GSI" & ad_clip=="N" & !grepl("RST", gear) ~ "Natural (assumed)",
                                  ID_Source=="Failed to amplify" ~ "Unknown",
                                  
                                  is.na(DNA_vial) | is.na(indiv) ~ NA,
                                  
                                  TRUE ~ "FLAG"),
         `(R) STOCK ID` = case_when(prob.1 >= 75 ~ str_to_title(gsub(collection.1, pattern="_", replacement=" ")),
                                    grepl("RST", gear) & is.na(collection.1) ~ "San Juan River",
                                    grepl("HAT", usid) ~ "San Juan River",
                                    ID_Source=="Failed to amplify" ~ "Unknown",
                                    prob.1 < 75 & (repunit.1%in%c("SWVI", "NWVI") | repunit.1%in%c("SWVI", "NWVI") | repunit.1%in%c("SWVI", "NWVI")) ~ 
                                      "Uncertain WCVI origin",
                                    is.na(DNA_vial) | is.na(indiv) ~ NA,
                                    TRUE ~ "FLAG")) %>%
  unite(col=`(R) STOCK-ORIGIN`, c(`(R) ORIGIN`, `(R) STOCK ID`), sep=" ", remove=F, na.rm=T) %>% 
  mutate(yday = lubridate::yday(date_end)) %>%
  print()




# ================= EXPORT ================= 
# To github ------------------------------------
writexl::write_xlsx(juvi.bioMeta, 
                    path = here("outputs", "R_OUT - PFN_DFO_FTFjuvi_2023_verified WITH RESULTS.xlsx"))


