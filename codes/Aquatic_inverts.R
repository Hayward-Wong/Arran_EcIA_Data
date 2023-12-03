#Libraries----
library(ggplot2)
library(tidyverse)
library(stringr)

#Style----
style <- function(){ #setting the style for my plots
  font <- "Helvetica"
  theme(plot.title = element_text(family = font, size = 14, face = "bold", color = "#222222", hjust = 0.5), 
        plot.subtitle = element_text(family = font, size = 12, margin = margin(9, 0, 9, 0)), 
        plot.caption = element_blank(),
        plot.margin = unit(c(1,1,1,1), units = , "cm"),
        legend.text.align = 0, 
        legend.position = "bottom",
        legend.title = element_text(family = font, size = 9, face = "bold", color = "#222222",  hjust = 0.5), 
        legend.key = element_blank(), 
        legend.text = element_text(family = font, size = 9, color = "#222222"),
        axis.text = element_text(family = font, size = 9, color = "#222222"), 
        axis.text.x = element_text(margin = margin(5, b = 10)), 
        axis.title = element_text(family = font, size = 12, face = "bold", color = "#222222"), 
        axis.ticks = element_blank(), 
        axis.line = element_line(colour = "black"),
        strip.text = element_text(size = 12, hjust = 0))
}

#Importing raw data----
ainv<-read.csv("occurrence_dat/aquatic_inverts_occurence.csv")#loading the aquatic inverts data
#Aquatic inverts 

ainv <-ainv %>% 
  select(eventID,individualCount,phylum,class,order,family,genus) %>% #keeping the needed columns only
  mutate(Plot = case_when( #creating a new column called plot
    grepl("^N", eventID, ignore.case = TRUE) ~ "Northern Plot", #classifying eventID with "N" as Northern Plot
    grepl("^S", eventID, ignore.case = TRUE) ~ "Southern Plot", #classifying eventID with "S" as Southern Plot
    TRUE ~ "Unknown"
  )) %>%
  mutate(family = ifelse(family == "", "unidentified", family))



Stream<-ainv %>%
  filter(str_detect(eventID, "Stream"))

Marsh<-ainv %>%
  filter(str_detect(eventID, "Marsh"))

(Stream_ab <- ggplot(Stream, aes(fill=family,y=individualCount,x=order))+ #plotting the individual birds observed per plot
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
    labs(x = "Invertebrate Orders", y = "Individuals Observed", 
         title = "Aquatic Invertebrates sampled in streams")+
    style()+
    scale_fill_manual(values = c("Baetidae" = "#f8766d", 
                                 "Heptageniidae (synonym: Ecdyonuridae)" = "#00bc51",
                                 "Philopotamidae" = "#a58aff",
                                 "Simuliidae" = "#fb61d7",
                                 "unidentified" = "#6a6a6a",
                                 "Dixidae" = "#89ac00",
                                 "Leptoceridae" = "#29a3ff",
                                 "Capniidae" = "#d09400",
                                 "Hydrophilidae" = "#00c0b2"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))




(Marsh_ab <- ggplot(Marsh, aes(fill=family,y=individualCount,x=order))+ #plotting the individual birds observed per plot
    geom_bar(stat = "identity",position = "stack",orientation = "x")+
    facet_wrap(~Plot)+ #seperating the Northerna and Southern Plots for comparision
    labs(x = "Invertebrate Orders", y = "Individuals Observed", 
         title = "Aquatic Invertebrates sampled in marshes")+
    style()+
    scale_fill_manual(values = c("unidentified" = "#6a6a6a"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))


ainv_total_rich <- ainv %>% 
  distinct(order,Plot) %>% 
  mutate(richness=1)

(Ainv_total_rich <- ggplot(ainv_total_rich, aes(y=richness, x=Plot))+#Plotting the species richness by point count
    geom_bar(stat = "identity",orientation = "x")+
    labs(x = "Plots", y = "Order Richness", 
         title = "Total Aquatic Order richness sampled\nin Northern and Southern Plots")+
    style())
