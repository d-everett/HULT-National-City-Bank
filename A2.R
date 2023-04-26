#A2: National City Bank
#Author: Dalton Lee Everett
#Date: Monday March 20th, 2023

########################
# GLossary
#
#
# (1) - Setting up
#   (1.a) reading the files
#   (1.b) merging the files
# (2) - EDA
# (3) - Data Engineering
# (4) - Decision Tree
# (5) - Random Forest
# (6) - Logistic Regression
# (7) - Prospective Customers
#   (7.a) - Merging Tables
#   (7.b) - EDA
# (8) - Predicting
# (9) - EDA on top 100

################################################################################################
# (1) Setting up
#
#

# Set WD
setwd("~/Desktop/School/Hult/Boston/Spring/Visualize R/hult_class/personalFiles")
options(scipen=999)

# Libs
#install.packages('mlr3learners')
#install.packages('glmnet')
#install.packages('vtreat')
#install.packages('dplyr')
#install.packages('ggplot2')
#install.packages('skimr')
#install.packages('caret')
#install.packages('rpart.plot')
#install.packages('mlr3learners')
#install.packages('ranger')
#install.packages('ggthemes')
#install.packages("tidymodels")

library(vtreat)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(skimr)
library(caret)
library(caTools)
library(readr)
library(rpart.plot)
library(mlr3learners)
library(MLmetrics)
library(glmnet)
library(dplyr)
library(randomForest)
library(ranger)
library(ggthemes)
library(pROC)
options(scipen = 999)



##########################################
# (1.a) reading the files
#
#
# (1.a.1) dataDictionary_csv
# (1.a.2) ProspectiveCustomers.csv
# (1.a.3) training
#   (1.a.3.1) CurrentCustomerMktgResults.csv
#   (1.a.3.2) householdAxiomData.csv
#   (1.a.3.3) householdCreditData.csv
#   (1.a.3.4) householdVehicleData.csv

#################
#(1.a.1)
# description of column meanings
#
#data_dict <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/dataDictionary.csv')
#
# this is just reading the names and giving descriptions of them

#################
# (1.a.2)
# This is a very good one that we will use for our modeling
#
prospective_customer <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/ProspectiveCustomers.csv')
#4 columns
# "var", "description", "Source", "Type"

#################
# (1.a.3)
# we will be merging these
# but first,
# lets figure our what we will be merging
# and figuring out how in the heck we do it
#

# (1.a.3.1)
# CurrentCustomerMktgResults.csv
#
#
cust_mkresults <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/CurrentCustomerMktgResults.csv')
head(cust_mkresults)
dim(cust_mkresults)
# 4000 x 12
names(cust_mkresults)


# (1.a.3.2)
# householdAxiomData.csv
#
#
hh_axiom <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdAxiomData.csv')
head(hh_axiom)
dim(hh_axiom)
# 5000 x 11
names(hh_axiom)


# (1.a.3.3)
# householdCreditData.csv
#
#
hh_credit <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdCreditData.csv')
dim(hh_credit)
# 5000 x 5
names(hh_credit)
head(hh_credit)


# (1.a.3.4)
# householdCreditData.csv
#
#
hh_vehicle <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/DD1_Case_Info/A2_NationalCityBank/training/householdVehicleData.csv')
dim(hh_vehicle)
# 5000 x 5
names(hh_vehicle)

##########################################
# (1.b) merging the files
#
#
# these are the ones were merging
# 1, cust_mkresults
# 2, hh_axiom
# 3, hh_credit
# 4, hh_vehicle
#
# Merge them on HHuniqueID

training_data_1 <- merge(cust_mkresults,hh_axiom, by=c('HHuniqueID'),all.x=T)
names(training_data_1)

training_data_2 <- merge(hh_credit,hh_vehicle, by=c('HHuniqueID'),all.x=T)
names(training_data_2)

training_data <- merge(training_data_1,training_data_2, by=c('HHuniqueID'),all.x=T)
names(training_data)
#bingo baby

dim(training_data)
# 4000 x 29

################################################################################################
# (2) EDA
#
#
#
# I am just exploring the data here, i apologize for the messiness in this section
#


# Age
#
#
summary(training_data$Age)

#min,18
#1st, 32
#median, 39
#mean, 41
#3rd, 49
#max 95

# Create a new variable for age groups
AgeGroup <- cut(training_data$Age, breaks = seq(18, 91, by = 5), right = FALSE)

# Count the number of observations in each age group
age_counts <- table(AgeGroup)

# Print the age counts
age_counts

# Count the number of observations in each age group
age_counts <- table(training_data$Age)

# Create a bar chart of the age counts
barplot(age_counts, 
        main = "Age Distribution", # Set the title of the plot
        xlab = "Age", # Set the label for the x-axis
        ylab = "Count", # Set the label for the y-axis
        col = "blue" # Set the color of the bars
)    

# Marital
#
#
names(training_data$Marital)

# Count the number of observations in each age group
marital_counts <- table(training_data$Marital)

# Percentage mix
marital_perc <- marital_counts/sum(marital_counts) * 100
# Percentage mix
marital_perc

# Create a bar chart of the age counts
barplot(marital_counts, 
        main = "Marital Status",
        xlab = "Status",
        ylab = "Count",
        col = "purple" # Set the color of the bars
)

# Education
#
#

# Count the number of observations in each age group
education_counts <- table(training_data$Education)

# Create a bar chart of the age counts
barplot(education_counts, 
        main = "Education Status",
        xlab = "Status",
        ylab = "Count",
        col = "green" # Set the color of the bars
)

# Percentage mix
education_perc <- education_counts/sum(education_counts) * 100
# Percentage mix
education_perc

# Job
#
#

# Calculate the percentage of people who have a job
job_percentage <- mean(!is.na(training_data$Job)) * 100

# View the result
job_percentage

# CarMake
#
#

# Create a dataframe with the top 10 carMake categories, sorted by percentage in descending order
top_carMake <- training_data %>%
  group_by(carMake) %>%
  summarize(carMake_percentage = n()/nrow(training_data)*100) %>%
  arrange(desc(carMake_percentage)) %>%
  head(6)

# Create a bar graph of the top 10 carMake categories, in descending order
ggplot(top_carMake, aes(x = reorder(carMake, -carMake_percentage), y = carMake_percentage)) +
  geom_bar(stat = "identity", fill = "orange") +
  ggtitle("Top 10 Car Brands") +
  xlab("Brand") +
  ylab("Percentage")

################################################################################################
# (3) Data Engineering
#
#
#
# We want to see the success of this model, 
# so we are comparing everything to the dependent variable = 'Y_AcceptedOffer'

#lets figure out v-treat
names(training_data)
head(training_data)
skim(training_data)

# drop calll duration columns 
training_data$CallEnd <- NULL
training_data$CallStart <- NULL

# Store the variable names in the `informativeFeatures` vector
informativeFeatures <- names(training_data)[-c(1, 2, 10)]
summary(informativeFeatures)
targetVariable      <- names(training_data)[10]
head(targetVariable)
successClass        <- 'Accepted'


# Pass the cleaned-up informativeFeatures vector to the designTreatmentsC function
# Automated variable processing
# for **categorical** outcomes 
# i. e.will the prospective client Accept Offer Y = 1
# DATA, NAMES OF INFORMATIVE VARS, RESPONSE VAR, SUCCESS CLASS

# For categorical targets the possible variable types are as follows:
# Clean, is_BAD, lev, cat_B, cat_P
# cat_P : a “prevalence fact” about a categorical level. 
# Tells us if the original level was rare or common and is not good for direct use in a model, 
#   ref: https://cran.r-project.org/web/packages/vtreat/vignettes/vtreatVariableTypes.html


plan <- designTreatmentsC(training_data, 
                          informativeFeatures,
                          targetVariable, 
                          successClass,
                          codeRestriction = c('lev','catN','clean','isBAD'))
# Apply the plan
categorical_treatedData <- vtreat::prepare(plan, training_data)

# Lots more appended vars; still need to drop redundant flags but much faster and robust!
summary(categorical_treatedData)
names(categorical_treatedData)
####################
skim(categorical_treatedData)

################################################################################################
# (4) Decision Tree
# 
#
# 73.88%
#

# Partitioning

# To save time in class, we are only training on 50% of the data
splitpercent <- round(nrow(categorical_treatedData) %*% .80)
totalrecords <- 1:nrow(categorical_treatedData)
set.seed(1234)
idex <- sample(totalrecords, splitpercent)

traindat <- categorical_treatedData[idex,]
testdat  <- categorical_treatedData[-idex,]


## EDA
summary(traindat)
head(traindat)

# No modification needed in this cleaned up data set.  One could engineer some interactions though.

# Fit a decision tree with caret
set.seed(1234)
fit <- train(as.factor(Y_AcceptedOffer) ~., #formula based
             data = traindat, #data in
             #"recursive partitioning (trees)
             method = "rpart", 
             #Define a range for the CP to test
             tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
             #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
             control = rpart.control(minsplit = 1, minbucket = 2)) 

# Examine
fit

# Plot the CP Accuracy Relationship to adust the tuneGrid inputs
plot(fit)

# Plot a pruned tree
prp(fit$finalModel, extra = 1)

# Make some predictions on the training set
traincaret <- predict(fit, traindat)
head(traincaret)

# Get the conf Matrix
confusionMatrix(traincaret, as.factor(traindat$Y_AcceptedOffer))
# accuracy - 73.88%

# Now more consistent accuracy & fewer rules!
testCaret <- predict(fit,testdat)
confusionMatrix(testCaret,as.factor(testdat$Y_AcceptedOffer))

# calculate accuracy, precision, recall, and F1 score for the test set predictions
dec_accuracy <- Accuracy(testCaret, testdat$Y_AcceptedOffer)
dec_precision <- Precision(testCaret, testdat$Y_AcceptedOffer)
dec_f1_score <- F1_Score(testCaret, testdat$Y_AcceptedOffer)
# print the evaluation metrics
cat("Accuracy:", dec_accuracy, "\n")
cat("Precision:", dec_precision, "\n")
cat("F1 Score:", dec_f1_score, "\n")

# 72.13%
# 44.83%
# 57.69%

################################################################################################
# (5) Random Forest
# 
# 
# 61.25%
# 

# Prep and non prep
set.seed(2022)
idxprep        <- sample(1:nrow(categorical_treatedData),.1*nrow(categorical_treatedData))
prepdata    <- categorical_treatedData[idxprep,]
nonprepdata <- categorical_treatedData[-idxprep,]
head(idxprep)
names(prepdata)

# Treatment
targetvar       <- names(prepdata)[71]
informativevars <- names(prepdata)[1:70]


# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(prepdata, 
                          informativevars,
                          targetvar,'Accepted')

# Partition to avoid overfitting
set.seed(1234)
idx        <- sample(1:nrow(nonprepdata),.8*nrow(nonprepdata))
train      <- nonprepdata[idx,]
validation <- nonprepdata[-idx,]



# Now apply the variable treatment plan
treatedtrain <- vtreat::prepare(plan, train)
treatedtest  <- vtreat::prepare(plan, validation)

# Fit a random forest model with Ranger
# Ranger is a fast implementation of random forests (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data.
moreCust <- ranger(as.factor(Y_AcceptedOffer) ~ .,
                     data  = treatedtrain, 
                     num.trees = 120,
                     importance = 'permutation',
                     mtry  = 1)

# Look at improved var importance
varimpdf <- data.frame(variables = names(importance(moreCust)),
                       importance = importance(moreCust),
                       row.names = NULL)
varimpdf <- varimpdf[order(varimpdf$importance, decreasing = T),]

#lets add color to the graph below to see neagtive and positive
varimpdf$sign <- ifelse(varimpdf$importance < 0, "Negative", "Positive")

ggplot(varimpdf, aes(x=importance, y = reorder(variables, importance), fill = sign)) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + 
  theme_gdocs() +
  scale_fill_manual(values=c("red", "green"))



# Confusion Matrix
trainclass <- predict(moreCust, treatedtrain)
confusionMatrix(trainclass$predictions, as.factor(treatedtrain$Y_AcceptedOffer))

### Now let's apply to the validation test set
idkwhattonamethis <- predict(moreCust, treatedtest)

# Accuracy Comparison from MLmetrics
Accuracy(treatedtest$Y_AcceptedOffer, idkwhattonamethis$predictions)
# 61.25%

# calculate accuracy, precision, recall, and F1 score for the test set predictions
ran_accuracy <- Accuracy(idkwhattonamethis$predictions, treatedtest$Y_AcceptedOffer)
ran_precision <- Precision(idkwhattonamethis$predictions, treatedtest$Y_AcceptedOffer)
ran_f1_score <- F1_Score(idkwhattonamethis$predictions, treatedtest$Y_AcceptedOffer)
# print the evaluation metrics
cat("Accuracy:", ran_accuracy, "\n")
cat("Precision:", ran_precision, "\n")
cat("F1 Score:", ran_f1_score, "\n")

# 61.25%
# 7.02%
# 13.08%

################################################################################################
# (6) Logistic Regression
#
#
# 73.25%
#



categorical_treatedData$Y_AcceptedOffer <- ifelse(categorical_treatedData$Y_AcceptedOffer == "Accepted", 1, 0)

# Data partitioning and Modeling
#########################################
# set seed for reproducibility
set.seed(123)

# split the data into train and test sets
split = sample.split(categorical_treatedData$Y_AcceptedOffer, SplitRatio = 0.8)

# create the train and test set
train = subset(categorical_treatedData, split == TRUE)
test = subset(categorical_treatedData, split == FALSE)

# fit logistic regression model to the train set
logistic_model <- glm(Y_AcceptedOffer ~ ., data = train, family = binomial())

# make predictions on the test set
test_predictions <- predict(logistic_model, newdata = test, type = "response")

# convert predictions to binary (0 or 1) based on a threshold of 0.5
test_predictions_binary <- ifelse(test_predictions > 0.5, 1, 0)

conf_mat <- confusionMatrix(factor(test_predictions_binary), factor(test$Y_AcceptedOffer))
conf_mat$table

roc_data <- roc(test$Y_AcceptedOffer, test_predictions)
plot(roc_data, print.thres = "best", print.thres.best.method = "closest.topleft")



# calculate accuracy, precision, recall, and F1 score for the test set predictions
accuracy <- Accuracy(test_predictions_binary, test$Y_AcceptedOffer)
precision <- Precision(test_predictions_binary, test$Y_AcceptedOffer)
f1_score <- F1_Score(test_predictions_binary, test$Y_AcceptedOffer)
# print the evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")

# 73.25%
# 89.56%
# 80.04%


################################################################################################
# (7) Prospective Customers
#   (7.a) Merging Tables
#   (7.b) EDA

#########################################
# (7.a) Merging Tables
#
#
#

untreated_prosp_cust <- merge(prospective_customer, hh_axiom, by='HHuniqueID', all.x=TRUE)
untreated_prosp_cust <- merge(untreated_prosp_cust, hh_credit, by='HHuniqueID', all.x=TRUE)
untreated_prosp_cust <- merge(untreated_prosp_cust, hh_vehicle, by='HHuniqueID', all.x=TRUE)

skim(untreated_prosp_cust)
names(untreated_prosp_cust)

################
#  Below i was trying to do vtreat on the variables we have, but could not get it to work

# Apply the plan
#treatedData <- vtreat::prepare(plan, untreated_prosp_cust)
#dim(treatedData)

# model prediction with prospective customers
#offerPredictions <- predict(logistic_model, treatedData)
#predictionResults <- data.frame( actual = treatedData, probs = offerPredictions)
#treatedData$offerAccepted <- predictionResults$probs
#tail(treatedData)
#########################################
# (7.b) EDA
#
#
# Create a new variable for age groups
AgeGroup <- cut(untreated_prosp_cust$Age, breaks = seq(18, 91, by = 5), right = FALSE)
summary(untreated_prosp_cust$Age)
# Count the number of observations in each age group
age_counts <- table(AgeGroup)
# Print the age counts
age_counts
# Create a bar chart of the age counts
barplot(age_counts, 
        main = "Age Distribution", # Set the title of the plot
        xlab = "Age", # Set the label for the x-axis
        ylab = "Count", # Set the label for the y-axis
        col = "blue" # Set the color of the bars
)    

# Marital
#
#
names(untreated_prosp_cust$Marital)
# Count the number of observations in each age group
marital_counts <- table(untreated_prosp_cust$Marital)
# Percentage mix
marital_perc <- marital_counts/sum(marital_counts) * 100
# Percentage mix
marital_perc
# Create a bar chart of the age counts
barplot(marital_counts, 
        main = "Marital Status",
        xlab = "Status",
        ylab = "Count",
        col = "purple" # Set the color of the bars
)

# Education
#
#

# Count the number of observations in each age group
education_counts <- table(untreated_prosp_cust$Education)
# Create a bar chart of the age counts
barplot(education_counts, 
        main = "Education Status",
        xlab = "Status",
        ylab = "Count",
        col = "green" # Set the color of the bars
)
# Percentage mix
education_perc <- education_counts/sum(education_counts) * 100
# Percentage mix
education_perc

# Job
#
#
# Calculate the percentage of people who have a job
job_percentage <- mean(!is.na(untreated_prosp_cust$Job)) * 100
# View the result
job_percentage

# CarMake
#
#

# Create a dataframe with the top 10 carMake categories, sorted by percentage in descending order
p_top_carMake <- untreated_prosp_cust %>%
  group_by(carMake) %>%
  summarize(carMake_percentage = n()/nrow(untreated_prosp_cust)*100) %>%
  arrange(desc(carMake_percentage)) %>%
  head(6)
# Create a bar graph of the top 10 carMake categories, in descending order
ggplot(p_top_carMake, aes(x = reorder(carMake, -carMake_percentage), y = carMake_percentage)) +
  geom_bar(stat = "identity", fill = "orange") +
  ggtitle("Top 10 Car Brands") +
  xlab("Brand") +
  ylab("Percentage")

# Online activity
#
#

# Create a table of the DigitalHabits_5_AlwaysOn variable and their counts
habits_counts <- table(untreated_prosp_cust$DigitalHabits_5_AlwaysOn)

# Create a table of the DigitalHabits_5_AlwaysOn variable and their percentages
habits_percentages <- prop.table(habits_counts) * 100

# Convert habits_percentages to a data frame
habits_df <- data.frame(DigitalHabits_5_AlwaysOn = names(habits_percentages),
                        Percentage = habits_percentages)

# Create the bar chart
ggplot(habits_df, aes(x = DigitalHabits_5_AlwaysOn, y = habits_percentages)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  ggtitle("Percentage mix of Digital Habits") +
  xlab("Digital Habits Compared against the average") +
  ylab("Percentage")


################################################################################################
# (8) Prediction
#
# Highest accuracy was the decision tree with 73.88% 
# 2nd Logistic Regression with 73.25% but had readings for F1 and Prcision
# 3rd Random Forest with 61.25%

# When we start looking at other measurements (precision & F1)
# we can conclude that Logistic Regression is the best choice because it has a significantly higher precision and 
# F1-score than Decision tree

# <model name>
# accuracy
# precision
# f1-score

log_pros_cust <- untreated_prosp_cust
dec_pros_cust <- untreated_prosp_cust
#ran_pros_cust <- prosp_cust_working

# Logistic Regression
# 73.25%
# 89.56%
# 80.04%
log_pros_cust$Y_AcceptedOffer  <- ifelse(is.na(log_pros_cust$Y_AcceptedOffer), test_predictions,log_pros_cust$Y_AcceptedOffer)
cutoff      <- 0.6
log_pros_cust$Y_AcceptedOffer  <- ifelse(log_pros_cust$Y_AcceptedOffer  >= cutoff, "Accepted","DidNotAccept")
log_allAccepted <- subset(log_pros_cust,Y_AcceptedOffer == "Accepted" )

# Subset test_predictions to include only rows where Y_AcceptedOffer == "Accepted"
test_predictions_subset <- test_predictions[log_pros_cust$Y_AcceptedOffer == "Accepted"]
# Add corresponding probabilities to each row
log_allAccepted_with_prob <- cbind(log_allAccepted, predicted_prob = test_predictions_subset)
# Order by predicted_prob in descending order
log_allAccepted_with_prob <- log_allAccepted_with_prob[order(-log_allAccepted_with_prob$predicted_prob), ]
head(log_allAccepted_with_prob, n=100)


####### Lets see if we get different results #######

# Decision Tree
# 72.13%
# 44.83%
# 57.69%
dec_pros_cust$Y_AcceptedOffer  <- ifelse(is.na(dec_pros_cust$Y_AcceptedOffer), traincaret,dec_pros_cust$Y_AcceptedOffer)
cutoff      <- 0.6
dec_pros_cust$Y_AcceptedOffer  <- ifelse(dec_pros_cust$Y_AcceptedOffer  >= cutoff, "Accepted","DidNotAccept")
dec_allAccepted <- subset(dec_pros_cust,Y_AcceptedOffer == "Accepted" )
head(dec_allAccepted, n=10)
#Amazing, these different models result in different predictions



# so, lets print csv the model we are selecting
# Lets view the top 100 of Logistic regression and save it to csv
head(log_allAccepted_with_prob, n = 100)
write.csv(log_allAccepted_with_prob[1:100, ], file = "A2_prediction_results.csv")



# Random Forest
# 61.25%
# 7.02%
# 13.08%
#i cannot get this to work to save my life
#ran_pros_cust$Y_AcceptedOffer  <- ifelse(is.na(ran_pros_cust$Y_AcceptedOffer), idkwhattonamethis,ran_pros_cust$Y_AcceptedOffer)
#cutoff      <- 0.21
#ran_pros_cust$Y_AcceptedOffer  <- ifelse(ran_pros_cust$Y_AcceptedOffer  >= cutoff, "Accepted","DidNotAccept")
#ran_allAccepted <- subset(ran_pros_cust,Y_AcceptedOffer == "Accepted" )
#head(ran_allAccepted)

################################################################################################
# (9) EDA on 100
#
#

top_100 <- log_allAccepted_with_prob[1:100, ]

names(top_100)

# Age
#
#
AgeGroup <- cut(top_100$Age, breaks = seq(18, 91, by = 5), right = FALSE)
summary(top_100$Age)
# Count the number of observations in each age group
age_counts <- table(AgeGroup)
# Print the age counts
age_counts
# Create a bar chart of the age counts
barplot(age_counts, 
        main = "Age Distribution", # Set the title of the plot
        xlab = "Age", # Set the label for the x-axis
        ylab = "Count", # Set the label for the y-axis
        col = "blue" # Set the color of the bars
)  

# CarMake
#
#
# Create a dataframe with the top 10 carMake categories, sorted by percentage in descending order
top_carMake <- top_100 %>%
  group_by(carMake) %>%
  summarize(carMake_percentage = n()/nrow(top_100)*100) %>%
  arrange(desc(carMake_percentage)) %>%
  head(6)
# Create a bar graph of the top 10 carMake categories, in descending order
ggplot(top_carMake, aes(x = reorder(carMake, -carMake_percentage), y = carMake_percentage)) +
  geom_bar(stat = "identity", fill = "orange") +
  ggtitle("Top 10 Car Brands") +
  xlab("Brand") +
  ylab("Percentage")


# Create a bar chart of 'AffluencePurchases'
ggplot(top_100, aes(x = AffluencePurchases)) +
  geom_bar() +
  labs(title = "Distribution of AffluencePurchases", x = "Affluence Purchases", y = "Frequency")
