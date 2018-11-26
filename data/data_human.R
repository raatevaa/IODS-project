# Antti Raatevaara
# 26.11.2018
# Chapter 4

data1 <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
data2 <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# Explore the datasets: see the structure and dimensions of the data. 
str(data1)
str(data2)

# Create summaries of the variables. 
summary(data1)
summary(data2)

# Rename the variables with (shorter) descriptive names
colnames(data1)[3] <- "hdi"
colnames(data1)[4] <- "Life.expectancy"
colnames(data1)[5] <- "edu_expectancy"
colnames(data1)[6] <- "edu_inYears"
colnames(data1)[7] <- "gni"
colnames(data1)[8] <- "gni_minus_hdi"

colnames(data2)[3] <- "gii" 
colnames(data2)[4] <- "mmr" 
colnames(data2)[5] <- "abr" 
colnames(data2)[6] <- "F.inParl" 
colnames(data2)[7] <- "edu_2F" 
colnames(data2)[8] <- "edu_2M" 
colnames(data2)[9] <- "work_F"  
colnames(data2)[10] <- "work_M"

data2 <- mutate(data2, eduRatio = edu_2F / edu_2M) 
data2 <- mutate(data2, workRatio = work_F / work_M)
library(dplyr)
human <- inner_join(data1, data2, by = "Country") 
write.csv(human, file="human.csv", row.names = FALSE) 
