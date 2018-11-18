#Author: Antti Raatevaara
#Date 13.11.2018

#Code description:
#This is the data wrangling part of IODS, chapter 3. Original data can be found from: UCI Machine Learning Repository, 
#Student Performance Data (incl. Alcohol consumption) page.
#Data consists two .csv files (student-mat.csv) and (student-por.csv).
library(dplyr)
library(ggplot2)
setwd("C:/Users/L1636/Documents/IODS/IODS-project/data")
# Read the csv's
df_mat <- read.csv(file="student-mat.csv", header=TRUE, sep=";")
df_por <- read.csv(file="student-por.csv", header=TRUE, sep=";")

str(df_mat)
#df_mat and por contains both categorical (school, sex) and discrete variables (studytime, age). Variables are same for both datas.
dim(df_mat)
#in df_mat data there is 33 columns (variables) and 365 rows (observations)
dim(df_por)
#in df_mat data there is 33 columns (variables) and 649 rows (observations)

#Identifiers
identfiers = c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")
joined_data <- inner_join(df_mat, df_por, by = identfiers)

dim(joined_data)
#New, joined data has 382 rows and 53 cols. This means that we discovered 382 observations which could be found from both dataframes.
#Joined data has considerably more variables, cols with same name have been separated by (.x / .y). X-cols are from df_por-data and Y's correspondingly
#from df_mat.

# From datacamp:
# create a new data frame with only the joined columns
alc <- select(joined_data, one_of(identfiers))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(df_mat)[!colnames(df_mat) %in% identfiers]

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(joined_data, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

glimpse(alc)
#Observations: 382
#Variables: 33

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)
glimpse(alc)
#Data has right number of observations and variables (33+2)

#Set working directory and write csv without indexing row

write.csv(alc, file = "alc_use.csv",row.names=FALSE)




