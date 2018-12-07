# Chapter 6 data wrangling
# Antti Raatevaara
# 7.12.2018



### FIRST PART (BPRS)


# Read the BPRS data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)

# Look at the (column) names of BPRS
names(BPRS) 

# Look at the structure of BPRS
dim(BPRS)

# Print out summaries of the variables
summary(BPRS)


# The data BPRS is available
# Access the packages dplyr and tidyr
library(dplyr)
library(tidyr)

# Factor treatment & subject
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

# Convert to long form
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)

# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks,5,6)))

# Take a glimpse at the BPRSL data
glimpse(BPRSL)


# Individuals on the plot   
# dplyr, tidyr packages and BPRSL are available

#Access the package ggplot2
library(ggplot2)

# Draw the plot
ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))


# The Golden Standardise
# dplyr, tidyr and ggplot2 packages and BPRSL are available

# Standardise the variable bprs
BPRSL <- BPRSL %>%
  group_by(week) %>%
  mutate(stdbprs = as.numeric(scale(bprs))) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL)

# Plot again with the standardised bprs
ggplot(BPRSL, aes(x = week, y = stdbprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  scale_y_continuous(name = "standardized bprs")


# Good things come in Summary graphs
# dplyr, tidyr & ggplot2 packages and BPRSL are available

# Number of weeks, baseline (week 0) included
n <- BPRSL$week %>% unique() %>% length()

# Summary data with mean and standard error of bprs by treatment and week 
BPRSS <- BPRSL %>%
  group_by(treatment, week) %>%
  summarise(mean = mean(bprs), se = sd(bprs)/sqrt(n)) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSS)

# Plot the mean profiles
ggplot(BPRSS, aes(x = week, y = mean, linetype = treatment, shape = treatment)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(bprs) +/- se(bprs)")


#Find the outlaw... Outlier!
# dplyr, tidyr & ggplot2 packages and BPRSL are available

# Create a summary data by treatment and subject with mean as the summary variable (ignoring baseline week 0).
BPRSL8S <- BPRSL %>%
  filter(week > 0) %>%
  group_by(treatment, subject) %>%
  summarise( mean=mean(bprs) ) %>%
  ungroup()

# Glimpse the data
glimpse(BPRSL8S)

# Draw a boxplot of the mean versus treatment
ggplot(BPRSL8S, aes(x = treatment, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 1-8")

# Create a new data by filtering the outlier and adjust the ggplot code the draw the plot again with the new data
# Filter out subject 11 (0ver 70 bprs)
BPRSL8S1 <- BPRSL8S  %>%
  filter(subject != 11)

# Draw a boxplot of the mean versus treatment
ggplot(BPRSL8S1, aes(x = treatment, y = mean)) +
  geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(bprs), weeks 1-8")


# T for test and A for Anova
# dplyr, tidyr & ggplot2 packages and BPRSL8S & BPRSL8S1 data are available

# Perform a two-sample t-test
t.test(mean ~ treatment, data = BPRSL8S1, var.equal = TRUE)

# Add the baseline from the original data as a new variable to the summary data
BPRSL8S2 <- BPRSL8S %>%
  mutate(baseline = BPRS$week0)

# Fit the linear model with the mean as the response 
fit <- lm(mean ~ baseline + treatment, data = BPRSL8S2)

# Compute the analysis of variance table for the fitted model with anova()
anova = anova(fit)
anova

# Seems like baseline (week 0 bprs-value) has strong statistical significance in the model.




### SECOND PART (RATS)

# dplyr is available

# read the RATS data
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

# Factor variables ID and Group
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)
# Glimpse the data
glimpse(RATS)

# Linear Mixed Effects Models: Gather 'round
# dplyr, tidyr and RATS are available

# Convert data to long form
RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,4))) 

# Glimpse the data
glimpse(RATSL)


# Plot first, ask questions later
# dplyr, tidyr and RATSL are available

# Check the dimensions of the data

# Plot the RATSL data
ggplot(RATSL, aes(x = Time, y = Weight, group = ID,color=Group)) +
  geom_line() +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) +
  scale_y_continuous(name = "Weight (grams)") +
  theme(legend.position = "top")
                       
# Plot interpretation: Group 1 has significantly lower weight than groups 2 and 3. Also it seems that weight
# does not increase over time as much as in other groups. 


# Holding on to independence: The Linear model
# dplyr, tidyr, RATS and RATSL are available

# create a regression model RATS_reg
RATS_reg <- lm(Weight ~ Time + Group, data = RATSL)

# print out a summary of the model

summary(RATS_reg)

# There is strong correlation between weight and Time in groups 2 and 3. The significance of the regression
# is much lower in group 1, as I mentioned before. 

# The Random Intercept Model
# dplyr, tidyr, RATS and RATSL are available

# access library lme4
library(lme4)

# Create a random intercept model
RATS_ref <- lmer(Weight ~ Time + Group + (1 | ID), data = RATSL, REML = FALSE)

# Print the summary of the model
summary(RATS_ref)

# Std.Dev. in ID = 32.953
# There is quite high variability between observations (ID´s)


# Slippery slopes: Random Intercept and Random Slope Model
# dplyr, tidyr, lme4, ggplot2, RATS and RATSL are available

# create a random intercept and random slope model
RATS_ref1 <- lmer(Weight ~ Time + Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model
summary(RATS_ref1)

# perform an ANOVA test on the two models
anova(RATS_ref1, RATS_ref)

#  chi-squared statistic = 142.94 (high)
#  p-value of the likelihood ratio 2.2e-16 ***, statistically signif.


# Time to interact: Random Intercept and Random Slope Model with interaction

# dplyr, tidyr, lme4, ggplot2, RATS and RATSL are available

# create a random intercept and random slope model with the interaction
RATS_ref2 <- lmer(Weight ~ Time * Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model


# perform an ANOVA test on the two models
anova(RATS_ref2, RATS_ref1)

# chi-squared statistic = 12.361 (low)
# p-value of the likelihood ratio 0.00207 **, statistically signif. (but lower than before)


# draw the plot of RATSL with the observed Weight values
ggplot(RATSL, aes(x = Time, y = Weight, group = ID, color=Group)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Observed weight (grams)") +
  theme(legend.position = "top")



# Create a vector of the fitted values
Fitted <- fitted(RATS_ref2)

RATSL <- RATSL  %>%
  mutate(fitted = as.numeric(Fitted)) 



# draw the plot of RATSL with the Fitted values of weight
ggplot(RATSL, aes(x = Time, y = fitted, group = ID, color=Group)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Fitted weight (grams)") +
  theme(legend.position = "top")


# "serious look" -at the data

# Data consists of measurements made over time. Longitudinal data -> repeated measurements
# Wikipedia: A longitudinal study (or longitudinal survey, or panel study) is a research design
#that involves repeated observations of the same variables (e.g., people) over short or long periods
#of time (i.e., uses longitudinal data). It is often a type of observational study, although they can
#also be structured as longitudinal randomized experiments.[1]

# In both data´s we have different treatment groups, so we are able to see the difference and effectiveness
# of the treatments.



