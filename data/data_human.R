# Antti Raatevaara
# 26.11.2018
# Chapter 4

data1 <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
data2 <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

str(data1)
summary(data1)

str(data2)
summary(data2)

colnames(data1)<-c("rank","country","gii","mmr","birth","parliament", "edu_2F","edu_2M","Lab_F","Lab_M")
colnames(data2)<-c("rank","country","HDI","LE","EYoE","MYoE","gni","GNIm_HDI")

data2 <- mutate(data2, eduRatio = edu_2F / edu_2M) 
data2 <- mutate(data2, workRatio = work_F / work_M)
library(dplyr)
human <- inner_join(data1, data2, by = "Country") 
write.csv(human, file="human.csv", row.names = FALSE) 
