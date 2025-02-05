
library(tidyverse)
"%notin%" <- Negate("%in%")


WCVIcrestBDWR <- read.csv(file=list.files(path="//ENT.dfo-mpo.ca/DFO-MPO/GROUP/PAC/PBS/Operations/SCA/SCD_Stad/WCVI/CHINOOK/WCVI_TERMINAL_RUN/Annual_data_summaries_for_RunRecons/CREST-BDWRcompile_base-files/2-Export-from-R/",
                                          pattern="^R_OUT - Biological_Data_With_Results \\(.*\\) \\d{4}-\\d{4}\\.csv$", 
                                          full.names=T), check.names=F) %>%
  mutate(yday = lubridate::yday(COLLECTION_DATE),
         LEGAL_STATUS = case_when(LENGTH_MM > 450 & LENGTH_MM < 800 ~ "LEGAL",
                                  LENGTH_MM <= 450 ~ "SUBLEGAL",
                                  LENGTH_MM >= 800 & yday%in%c(196:212) ~ "SUPERLEGAL",
                                  LENGTH_MM >= 800 & yday%notin%c(196:212) ~ "LEGAL"),
         isSJ1 = case_when(grepl("San Juan", `(R) Term Run Group 3`) ~ `(R) Term Run Group 3`,
                           !grepl("San Juan", `(R) Term Run Group 3`) & `(R) Origin`!="Unknown" ~ 
                             paste0(`(R) Origin`, " ", "Non-San Juan"),
                           !grepl("San Juan", `(R) Term Run Group 3`) & `(R) Origin`=="Unknown" ~ 
                             "Unknown Non-San Juan"),
         isSJ2 = case_when(grepl("Non-San Juan", isSJ1) ~ "Other",
                           TRUE ~ isSJ1)) %>% 
  print()




# wedge stock comp
wedge <- WCVIcrestBDWR %>% 
  filter(SAMPLE_TYPE=="Sport", SPECIES==124, AREA==20, FISHING_LOCATION=="The Wedge") %>% 
  group_by(YEAR, MONTH, isSJ2) %>% 
  filter(!grepl("Unknown", isSJ2)) %>%
  summarize(n=n()) %>%
  group_by(YEAR, MONTH) %>% 
  mutate(total_n = sum(n),
         propn = n/total_n) %>%
  print()


# ages of wedge SJ fish
WCVIcrestBDWR %>% 
  filter(SAMPLE_TYPE=="Sport", SPECIES==124, AREA==20, FISHING_LOCATION=="The Wedge") %>% 
  group_by(YEAR, MONTH, isSJ2, `(R) Resolved total age`) %>% 
  filter(!grepl("Unknown", isSJ2)) %>%
  summarize(n=n()) %>%
  filter(isSJ2!="Other")


# wedge non-SJ comp
WCVIcrestBDWR %>% 
  filter(SAMPLE_TYPE=="Sport", SPECIES==124, AREA==20, FISHING_LOCATION=="The Wedge") %>% 
  filter(!grepl("San Juan", `(R) Term Run Group 3`)) %>%
  group_by(YEAR, MONTH, `(R) Term Run Group 2`) %>% 
  summarize(n=n()) %>%
  filter(!grepl("Unknown", `(R) Term Run Group 2`))



pdf(file = here::here("outputs", "figures", "The Wedge SJ stock composition - monthly proportion.pdf"),   
    width = 11, # The width of the plot in inches
    height = 8.5) # The height of the plot in inches

 ggplot() +
   geom_bar(data=wedge, aes(x=MONTH, y=propn, fill=isSJ2, colour=isSJ2), colour="black", stat='identity', position="dodge", alpha=0.7, width=0.5) +
   geom_text(data=wedge, aes(x=MONTH, y=propn+0.05, label=n, group=isSJ2), position=position_dodge(width=0.5), size=6) +
   scale_fill_manual(breaks=waiver(), values=c(scales::hue_pal()(2)[1], scales::hue_pal()(2)[2], "gray70")) +
   scale_y_continuous(labels = scales::percent_format()) +
   labs(x="", y="Proportion of biosamples", title="The Wedge creel biosamples") +
   theme_bw() +
   theme(axis.text = element_text(colour="black", size=20),
         axis.title = element_text(face="bold", size=22),
         strip.text = element_text(size=20),
         legend.title=element_blank(),
         legend.text=element_text(size=20),
         legend.background = element_rect(colour="black"),
         legend.position=c(0.8, 0.35)) +
   facet_wrap(~YEAR, nrow=2)
 
 dev.off()
 
 
 
 
 
 
 
 # wedge stock comp
 wedgeDOY <- WCVIcrestBDWR %>% 
   filter(SAMPLE_TYPE=="Sport", SPECIES==124, AREA==20, FISHING_LOCATION=="The Wedge") %>% 
   group_by(YEAR, DAYOFYEAR, isSJ2) %>% 
   filter(!grepl("Unknown", isSJ2)) %>%
   summarize(n=n()) %>%
   group_by(YEAR, DAYOFYEAR) %>% 
   mutate(total_n = sum(n),
          propn = n/total_n) %>%
   print()
 
 

 
 pdf(file = here::here("outputs", "figures", "The Wedge SJ stock composition - daily.pdf"),   
     width = 11, # The width of the plot in inches
     height = 8.5) # The height of the plot in inches
 
 ggplot() +
   geom_bar(data=wedgeDOY, aes(x=as.Date(DAYOFYEAR, origin="2021-12-31"), y=n, fill=isSJ2, colour=isSJ2), 
            stat='identity', position="dodge", alpha=0.8, width=0.7, size=0.7) +
   scale_fill_manual(breaks=waiver(), values=c(scales::hue_pal()(2)[1], scales::hue_pal()(2)[2], "gray80")) +
   scale_colour_manual(breaks=waiver(), values=c(scales::hue_pal()(2)[1], scales::hue_pal()(2)[2], "gray80")) +
   scale_x_date(date_labels = "%b %d", date_breaks="2 day") +
   labs(x="", y="Number of creel samples from The Wedge") +
   theme_bw() +
   theme(axis.text = element_text(colour="black", size=20),
         axis.text.x = element_text(angle=45, hjust=1),
         axis.title = element_text(face="bold", size=22),
         strip.text = element_text(size=20),
         legend.title=element_blank(),
         legend.text=element_text(size=20),
         legend.background = element_rect(colour="black"),
         legend.position=c(0.8, 0.35)) +
   facet_wrap(~YEAR, nrow=2)

 
 dev.off()