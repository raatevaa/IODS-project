#Antti Raatevaara
#04.11.2018
#This r-script holds the code for data wrangling exercise.

# Read a delimited file
df <- read.delim("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t") 

#Short description of data frame:
#Data is a table, which has 183 rows and 60 columns. Each column seems to represent a variable that has integer values, except
#column gender is string (F/M). Age, attitude, points are continous variables, and other columns are probably categorical (1-5).

#--------------

#Select variables gender, age, attitude, deep, stra, surf and point
#9.11, Some of the columns are missing from data, using what there is

myvars <- c("gender", "Age", "Attitude","Points")
#myvars2 <- c("gender", "Age", "Attitude","Deep","Stra","Surf","Points")
#Subset
newdata <- df[myvars]
#Clear zeros
newdata <- subset(newdata, Points > 0) 

#Set working directory and write csv without indexing row
setwd("/Users/Omistaja/Documents/GitHub/IODS-project/data")
write.csv(newdata, file = "learning2014.csv",row.names=FALSE)
#Read data with headers
data_read <- read.csv(file="learning2014.csv", header=TRUE, sep=",")
