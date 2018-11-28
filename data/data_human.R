# Antti Raatevaara
# 26.11.2018
# Chapter 4

#data1 <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
#data2 <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#str(data1)
#summary(data1)

#str(data2)
#summary(data2)

#colnames(data1)<-c("rank","country","gii","mmr","birth","parliament", "edu_2F","edu_2M","Lab_F","Lab_M")
#colnames(data2)<-c("rank","country","HDI","LE","EYoE","MYoE","gni","GNIm_HDI")

#data2 <- mutate(data2, eduRatio = edu_2F / edu_2M) 
#data2 <- mutate(data2, workRatio = work_F / work_M)
#library(dplyr)
#human <- inner_join(data1, data2, by = "Country") 
#write.csv(human, file="human.csv", row.names = FALSE) 

#-------------------------------------------------------------------------------------------------------------


# Chapter 5, data wrangling
# 28.11.2018
# Antti Raatevaara

human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt",sep  =",", header = T)

# look at the (column) names of human
names(human)
dim(human)
summary(human)

# Human data consists of 19 attributes and 195 observations (countries). Varibales describe
# countries "Human development index":
# The Human Development Index (HDI) is a summary measure of average achievement in key dimensions of human development:
# a long and healthy life, being knowledgeable and have a decent standard of living.
# The HDI is the geometric mean of normalized indices for each of the three dimensions.

# HDI = mean(Long and healthy life, Knowledge, A decent standard of living)
# Variables are multidimensional

library(tidyr); library(stringr)
human$GNI
typeof(human$GNI)
human$GNI[[3]] 

# GNI-variable is treated as factor (string with comma)
# remove the commas from GNI and print out a numeric version of it
human$GNI = str_replace(human$GNI, pattern=",", replace ="")

# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
# select the 'keep' columns
human <- select(human, one_of(keep))
# print out a completeness indicator of the 'human' data
#complete.cases(human)
# if value is TRUE - row does not have NA-values
# print out the data along with a completeness indicator as the last column
#human = data.frame(human[-1], comp = complete.cases(human))

# filter out all rows with NA values
#human_ <- filter(human, comp == TRUE)

#More sophisticated method to drop Na-rows
human_ <- human %>% drop_na()
human_$GNI = str_replace(human_$GNI, pattern=",", replace ="")
human_ = transform(human_, GNI = as.numeric(GNI))
human_ = transform(human_, Mat.Mor = as.numeric(Mat.Mor))
str(human_)
# look at the last 10 observations of human
tail(human,10)
# 10 last country names
tail(human_,10)$Country


human_ <- human_[!regs]

# define the last indice we want to keep
last <- nrow(human_) - 7
# choose everything until the last 7 observations
human_ <- human_[-last,]
# add countries as rownames
rownames(human_) <- human_$Country
# Drop regions
regions <- c("Central African Republic","East Asia and the Pacific","Europe and Central Asia","Latin America and the Caribbean","Arab States","South Asia","Sub-Saharan Africa","World")
human_ = human_[!(row.names(human_) %in% regions), ]

# remove the Country variable
human_ <- select(human_, -Country)

# Access GGally
library(GGally);
lowerFn <- function(data, mapping, ...){p <- ggplot(data = data, mapping = mapping) +geom_point(color = 'blue', alpha=0.3, size=1) +geom_smooth(color = 'black', method='lm', size=1,...)}
g <- ggpairs(data = human_, lower = list(continuous =  wrap(lowerFn)), upper = list(continuous = wrap("cor", size = 4)))

g <- g + theme(
  axis.text = element_text(size = 4),
  axis.title = element_text(size = 4),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "grey95")
)
print(g, bottomHeightProportion = 0.5, leftWidthProportion = .5)

cor(human_)
library(corrplot)
corrplot(as.matrix(human_), method="circle")

write.csv(human_, file="create_Human.csv", row.names = TRUE) 
