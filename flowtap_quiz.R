
#################################################################################
# QUIZ for Flowtap Application
# https://www.flowtap.com/data-science-projecta1//folder2/c67.html
# Robert Scharf, 02.05.2018
#################################################################################


#################################################################################
# 1. Engineering a data Set

# Drawing n = 10000 from negative binomial distribution, where
#################################################################################

# Create a list of numbers with requested properties
mu = 1000
sd = 10
n = 1000

x <- rnorm(n = n, mean = mu, sd = sd)

mean(x) # mean of all values is 1000
sd(x) # mtandard deviation is 10
length(unique(x)) # minimum 100 disctint values (1000)


#################################################################################
# 2. Writing a Simulation
#################################################################################
# r = 1 is the number of failures (girls born) until experiment is stopped
# p = 0.5 is the sucess probability in each experiment (birth)
# y is the number of girls per family

r = 1
p = 0.5 
n = 10000

y <- rnbinom(n = n, size = r, prob = p) 

# the mean is the expected number of girls in each family

mean(y)

# Note: Analytically the expected mean is simply: p*r/(1-p)

mu <- p*r / (1-p)

# Since the number of boys per family is 1 by declaration
# the long term ratio of boys to girls in the country is simply 1

mu / 1 

#################################################################################
# 3. Analyzing a Data Set
#################################################################################

# Getting data from URL

install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")

library(data.table)
library(dplyr)
library(ggplot2)

mydata <- fread("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

# name variable names
colnames(mydata) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status",
                      "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss",
                      "hours_per_week", "native-country", "over50k")

# change variables to dummies 
mydata$over50k[mydata$over50k == ">50K"] <- 1
mydata$over50k[mydata$over50k == "<=50K"] <- 0
mydata$over50k <- as.numeric(mydata$over50k)



# 1 Visualize two interesting findings of your choice 
#################################################################################

# 1.1
# Share of people earning over 50k by race and gender
shares <- mydata %>% group_by(race,sex) %>% summarise(over50k_share = weighted.mean(over50k, fnlwgt))

# Barplot 
ggplot(data = shares, aes(x=race, y= over50k_share, fill=sex)) +
  geom_bar(stat="identity", position = position_dodge()) +
  ggtitle("Share of people earning over 50k by race and sex")

# The first plot shows the share of people making over 50k by race and gender
# as one see the shares vary through races by level and by gender gap


#1.2 Hours worked per week by age and gender
ggplot(data = mydata, aes(x=age, y = hours_per_week, fill=sex, weight = fnlwgt)) +
  geom_smooth(model = lm) + xlim(18,65) +
  ggtitle("Hours worked per week by age and gender")

# The second plot shows kernel regressions of how the hours worked per week
# depend on age for both sexes - as one can see the relationship appears
# to have the shape of a bell curve with a kink between the age of ~30 (women)
# and ~38 that might be related to childbirth


#1.3 BONUS
# Densitiy plot of age distribution by marital status
ggplot(data = mydata, aes(x=age, fill = marital_status, weight = fnlwgt)) +
  geom_density(alpha = 0.3) +
  ggtitle("Age distribution by marital status")

# The third plot shows the age distributions by marital status
# as one can see the density varies a lot through marital status
# e.g. people who are never married tend to be much younger, while
# people who are widowed tend to be much older, which should be of no surprise



#2 Build a linear model to predict whether a person makes over 50k a year
#################################################################################

# Change variables to numeric

mydata$female <- 0
mydata$female[mydata$sex == "Female"] <- 1

mydata$black <- 0
mydata$black[mydata$race == "Black"] <- 1

mydata$asian <- 0
mydata$asian[mydata$race == "Asian-Pac-Islander"] <- 1

mydata$native <- 0
mydata$native[mydata$race == "Amer-Indian-Eskimo"] <- 1

mydata$agesqr <- mydata$age * mydata$age

mydata$hours_per_week_sqr <- mydata$hours_per_week * mydata$hours_per_week


# Estimating linear model

lmodel <- glm(over50k ~ age + agesqr + education_num + hours_per_week + hours_per_week_sqr + 
               female + black + asian + native, family = binomial(link ="logit"), data = mydata)

# predicting the probability of an household earning more than 50k

mydata$prob.predict = (predict(lmodel, type = "response"))

# This can now be interpreted as the probability that a certain person makes over 50k a year
# by setting a threshold of choice one can predict wheter a person makes over 50k a year


