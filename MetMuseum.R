library(tidyverse)
library(readr)
library(data.table)

if(getwd()!="G:/DataAnalysis/learn"){setwd("G:/DataAnalysis/learn")}
MetObjects <- read_csv("G:/DataAnalysis/learn/MetObjects.csv",trim_ws = TRUE)

mo<-MetObjects %>% filter(!is.na(Country))
mo$Country<-sapply(sapply((strsplit(mo$Country, "|", fixed=TRUE)), unlist), "[[", 1)
mo$Country<-gsub("\\([:punct:]*|[:alnum:]*\\)", "", mo$Country)
mo$Country<-gsub("\\([:punct:]|[:alnum:]\\)", "", mo$Country)
mo$Country<-gsub("\\(\\?)", "", mo$Country)
mo$Country<-gsub("probably |possibly |present-day | possibly| ossibly| robably", "", mo$Country, ignore.case = TRUE)
mo <-mo %>% separate(Country, into=c("Country1", "Country2", "Country3"),sep=" or ")
unique(mo$Country1)

us<-mo[grepl("United States", mo$Country),]
syr<-mo[grepl("Syria", mo$Country),]
jewels<-mo[which(mo$`Object Name` %in% c("Brooch", "Earring","Earrings",  "Bracelet","Bracelets", "Necklace", "Bead", "Beads", "Ring", "Watch")),]
unique(jewels$Country1)
unique(jewels$`Object Name`)

lost_jewels<-jewels %>% group_by(`Object Name`) %>% 
  summarise(lost=n())

# write.csv(mo, "G:/DataAnalysis/learn/cleanMet.csv", row.names = FALSE)
