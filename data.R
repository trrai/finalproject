library("dplyr")
library("ggplot2")
data<-read.csv("inc_occ_gender.csv", stringsAsFactors = FALSE)
data.wage.time <- read.csv("WageTime.csv", stringsAsFactors = FALSE) %>% 
                  filter(SUBJECT != "SELFEMPLOYED")
data$M_weekly <- strtoi(data$M_weekly) #Convert to Int for ease of use
data$F_weekly <- strtoi(data$F_weekly) #Convert to Int for ease of use

#Clean dataframe
all.data<-data %>% filter(F_weekly != "Na") %>% 
  filter(M_weekly != "Na") %>% 
  filter(All_weekly != "Na") %>% 
  mutate(diff = (M_weekly - F_weekly)) %>% 
  mutate(favor = ifelse(diff > 0, "Male", "Female"))


data<-all.data %>% filter(Occupation != "ALL OCCUPATIONS")

upper.case<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O",
              "P","Q","R","S","T","U","V","W","X","Y","Z")
grouped.data<-data[substr(data$Occupation,2,2) %in% upper.case,] #Gets groups of occupations
single.data<-data[substr(data$Occupation,2,2) %in% tolower(upper.case),]

#Overall 
female.data<-data %>% select(Occupation, F_workers, F_weekly) 
male.data<-data %>% select(Occupation, M_workers, M_weekly)
both.sexes.data <- data %>% select(Occupation, All_workers, All_weekly, diff)

#Breakdown of wage gap difference
male.favored.difference<-single.data %>% filter(diff > 0)
female.favored.difference<-single.data %>% filter(diff < 0)
top.ten.percent.male.difference<-single.data %>% filter(diff > (quantile(diff, .90)))
top.ten.percent.female.difference<-single.data %>% filter(diff < 0) %>% filter(diff < quantile(diff, .90))
bottom.ten.percent.difference<-single.data %>% mutate(abs.diff = abs(diff)) %>% filter(abs.diff < (quantile(abs.diff, .10)))

overall.male.stats<-male.favored.difference %>% summarize(mean = mean(diff), median = median(diff), max.male.difference = max(diff))
overall.female.stats<-female.favored.difference %>% summarize(mean = mean(diff), median = median(diff), max.female.difference = min(diff))

