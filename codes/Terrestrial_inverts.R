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
tinv<-read.csv("occurrence_dat/terrestrial_inverts_occurence.csv")#loading the terretrial inverts data
#1. Terrestrial inverts data preparation----
tinv <-tinv %>% 
  select(eventID,occurrenceStatus,individualCount,scientificName,class,order,family,genus,species) %>% #keeping the needed columns only
  mutate(Plot = case_when( #creating a new column called plot
    grepl("^N", eventID, ignore.case = TRUE) ~ "Northern Plot", #classifying eventID with "N" as Northern Plot
    grepl("^S", eventID, ignore.case = TRUE) ~ "Southern Plot", #classifying eventID with "S" as Southern Plot
    TRUE ~ "Unknown"
  )) %>% 
  mutate(method = case_when( #creating a new column for sampling methods
    grepl("P", eventID) ~ "pitfall", #eventID with "P" for pitfall
    TRUE ~ "sweepnet"#eventID without "P" is for sweepnet
  ))%>% 
  mutate(richness = ifelse(occurrenceStatus == "present", 1, 0))

pitfall<-tinv %>% #Keeping only pitfall
  filter(method == "pitfall")

snet<-tinv %>%
  filter(method == "sweepnet") %>% #Keeping only sweepnet
  mutate(family = ifelse(family == "", "unidentified", family)) %>% #changing the empty cells into unidentified as only pitfalls have absent data
  mutate(order = ifelse(order == "", "unidentified", family)) %>% 
  mutate(Trans = case_when( #creating a column for transact
    grepl("L", eventID, ignore.case = TRUE) ~ "Low",
    grepl("M", eventID, ignore.case = TRUE) ~ "Mid",    
    grepl("H", eventID, ignore.case = TRUE) ~ "High",
    TRUE ~ "Unknown"))#sorting the samples into different transacts by their eventID 

#2. Pitfall
pitfall



