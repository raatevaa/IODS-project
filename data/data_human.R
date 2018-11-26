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
colnames(data1)[3] <- "HDI"    # Human Development Index
colnames(data1)[4] <- "Life.expectancy"
colnames(data1)[5] <- "edu.expectancy"
colnames(data1)[6] <- "edu.inYears"
colnames(data1)[7] <- "GNI"    # Gross National Income
colnames(data1)[8] <- "GNI_minus_HDI"

colnames(data2)[3] <- "GII" 
colnames(data2)[4] <- "MMR" 
colnames(data2)[5] <- "ABR" 
colnames(data2)[6] <- "F.inParl" 
colnames(data2)[7] <- "edu2F" 
colnames(data2)[8] <- "edu2M" 
colnames(data2)[9] <- "workF"  
colnames(data2)[10] <- "workM"

data2 <- mutate(data2, eduRatio = edu2F / edu2M) 
data2 <- mutate(data2, workRatio = workF / workM)
library(dplyr)
human <- inner_join(gii, hd, by = "Country") # 195 observations and 19 variables. 
write.csv(human, file="human.csv", row.names = FALSE) # write data to file
