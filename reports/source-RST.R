# San Juan 2024 RST explorations
# Aug 2024


# Load libraries -------------------
library(tidyverse)

# Load helpers -------------------
"%notin%" <- Negate("%in%")

# ========================= LOAD 2024 EPICOLLECT DATA =========================
# Form 1: Metadata/survey details -------------------
rst24.1 <- readxl::read_excel(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/RST/form-1_RSTmaster - verified.xlsx",
                              guess_max=20000) %>% 
  mutate(R_date = lubridate::dmy(str_sub(title, start=1, end=10))) %>% 
  rename(ec5_parent_uuid=ec5_uuid)


# Form 2: Enumeration -------------------
rst24.2 <- readxl::read_excel(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/RST/form-2_RSTmaster - verified.xlsx",
                              guess_max=20000) 

rst24.releases <- rst24.2 %>% 
  select(X217_Are_you_marking_:Comments)

rst24.2 <- rst24.2 %>% 
  select(ec5_uuid:Species_etc)


# Form 3: Catch sampling -------------------
rst24.3 <- readxl::read_excel(path="//dcbcpbsna01a.ENT.dfo-mpo.ca/SCD_Stad/WCVI/JUVENILE_PROJECTS/Area 20-San Juan juveniles/Data management/Epicollect Data Downloads/2024/RST/form-3_RSTmaster - verified.xlsx",
                              guess_max=20000)





# ========================= JOIN 2024 EPICOLLECT DATA =========================
rst24.metaEnum <- left_join(rst24.1, rst24.2,
                            by="ec5_parent_uuid") %>% 
  mutate(DOY = lubridate::yday(R_date),
         across(c(CN_smlt_cl_nobis:Anchovy), ~case_when(.=="NA" ~ NA,
                                                        TRUE ~ as.numeric(.)))) %>% 
  pivot_longer(cols=c(CN_smlt_cl_nobis:Anchovy), names_to="spp_stage_condition", values_to="count") %>% 
  mutate(species = case_when(grepl("CO", spp_stage_condition) ~ "Coho",
                             grepl("CN", spp_stage_condition) ~ "Chinook",
                             grepl("SK", spp_stage_condition) ~ "Sockeye",
                             grepl("CM", spp_stage_condition) ~ "Chum",
                             grepl("Pk|PK", spp_stage_condition) ~ "Pink",
                             grepl("SH", spp_stage_condition) ~ "Steelhead",
                             grepl("other", spp_stage_condition) ~ "Other"),
         stage = case_when(grepl("fry", spp_stage_condition) ~ "Fry",
                           grepl("smlt|smolt", spp_stage_condition) ~ "Smolt",
                           TRUE ~ "FLAG"),
         condition = case_when(grepl("mort|mrt", spp_stage_condition) ~ "Mort",
                               TRUE ~ "Live"),
         tag_status = case_when(grepl("nobis|nobi", spp_stage_condition) ~ "Untagged",
                                grepl("_bis", spp_stage_condition) ~ "Tag recovery",
                                TRUE ~ "FLAG"),
         clip_status = case_when(grepl("cl", spp_stage_condition) ~ "Ad-clipped",
                                 TRUE ~ "Unclipped"),
         count = case_when(is.na(count) ~ 0,
                           TRUE ~ as.numeric(count)))

         
         
         
############################################################################################################################################################

# Exploration --- raw total catch (live + morts)

ggplot(data=rst24.metaEnum %>% 
         group_by(DOY, species, stage) %>%
         summarize(n=sum(count, na.rm=T)) %>%
         filter(!is.na(species) & !is.na(stage) & n>0 & species%in%c("Chinook", "Chum", "Coho")), 
       aes(x=DOY, y=n, group=interaction(species, stage, sep=" "), fill=interaction(species, stage, sep=" "))) +
  geom_bar(stat="identity") +
  theme_bw()
  


# exploration -- morts 

ggplot(data=rst24.metaEnum %>% 
         filter(condition=="Mort") %>% 
         group_by(DOY, species, stage) %>%
         summarize(n=sum(count, na.rm=T)) %>%
         filter(species%in%c("Chinook", "Chum", "Coho") & n>0), 
       aes(x=DOY, y=n, group=interaction(species, stage, sep=" "), fill=interaction(species, stage, sep=" "), colour=interaction(species, stage, sep=" "))) +
  geom_bar(stat="identity", width=1, alpha=0.6) +
  labs(x="", y="Total catch (uncorrected)", group="Species/stage", fill="Species/stage", colour="Species/stage") +
  theme_bw()


# mort rate 

ggplot() +
  geom_bar(data=rst24.metaEnum %>% 
             filter(!is.na(species)) %>%
             group_by(DOY, species, stage, condition) %>%
             summarize(n=sum(count, na.rm=T)) %>%
             group_by(DOY) %>% 
             mutate(total=sum(n)) %>%
             filter(condition=="Mort" & total>0) %>% 
             mutate(mort_rate=n/total) %>%
             filter(n>0),
           aes(x=DOY, y=mort_rate, 
               group=interaction(species, stage, sep=" "), fill=interaction(species, stage, sep=" "), colour=interaction(species, stage, sep=" ")), 
           stat="identity", width=1, alpha=0.5, position=position_stack(vjust=1)) +
  geom_text(data=rst24.metaEnum %>% 
              filter(!is.na(species)) %>%
              group_by(DOY, species, stage, condition) %>%
              summarize(n=sum(count, na.rm=T)) %>%
              
              group_by(DOY) %>% 
              mutate(total=sum(n)) %>% 
              ungroup() %>% 
              filter(total>0 & condition=="Mort") %>%
              mutate(mort_rate = n/total) %>% 
              filter(n>0),
            aes(x=DOY, y=mort_rate, 
                group=interaction(species, stage, sep=" "), fill=interaction(species, stage, sep=" "), colour=interaction(species, stage, sep=" "),
                label=n), position=position_stack(vjust=0.5)) +
  labs(x="", y="Mortality rate", group="Species/stage", fill="Species/stage", colour="Species/stage") +
  theme_bw()




