################## HEADER #######################
#  Company    : Stevens 
#  Project    : CS 513 Final Project
#  Purpose    : Perform C5.0 to predict arrest likelihood
#  First Name  : Justin
#  Last Name  : Tsang
#  Id			    : 
#  Date       : October 29, 2018
#  Comments   : NULLs and outliers removed

rm(list=ls())
#################################################
###### Load data #####
file_path <- "/Users/justint/Documents/2018-Fall/CS-513/Project/1_remove_null_outlier/1-Categorized.csv"

# df <- read.csv(
#   file=file_path,
#   header=TRUE,
#   sep=",",
#   na.strings=c(""),
#   stringsAsFactors = FALSE
# )

df <- read.csv(
  file=file_path,
  header=TRUE,
  sep=",",
  na.strings=c("(null)", "", "V", "("),
  stringsAsFactors = FALSE
)

# features <- c("STOP_WAS_INITIATED", "ISSUING_OFFICER_RANK", "SUPERVISING_OFFICER_RANK", "SUSPECTED_CRIME_DESCRIPTION",
#               "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
#               "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
#               "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
#               "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
#               "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
#               "CATEGORIZED_SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "CATEGORIZED_SUSPECT_HEIGHT", "CATEGORIZED_SUSPECT_WEIGHT",
#               "STOP_LOCATION_PRECINCT")
features <- c("STOP_WAS_INITIATED", "ISSUING_OFFICER_RANK", "SUPERVISING_OFFICER_RANK", "SUSPECTED_CRIME_DESCRIPTION",
              "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
              "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
              "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
              "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
              "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
              "CATEGORIZED_SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "CATEGORIZED_SUSPECT_HEIGHT", "CATEGORIZED_SUSPECT_WEIGHT",
              "STOP_LOCATION_PRECINCT")
dependent <- c("SUSPECT_ARRESTED_FLAG")

ranks <- c("POF", "POM", "DT1", "DT2", "DT3", "DTS", "SSA", "SGT", "SDS", "LSA", "LT", "CPT", "DI", "LCD")
sqf_df <- df[c(features, dependent)]

##### CLEANUP DATA #####
library(modeest)

for (feature in features) {
  na_rows <- is.na(sqf_df[, feature])
  if (feature == "FIREARM_FLAG" || feature == "KNIFE_CUTTER_FLAG" || feature == "OTHER_WEAPON_FLAG" || feature == "WEAPON_FOUND_FLAG" ||
      feature == "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG" || feature == "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG" ||
      feature == "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG" || feature == "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG" ||
      feature == "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG" || feature == "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG") {
    sqf_df[na_rows, feature] <- "N"
  }
  # } else if (feature == "SUSPECT_REPORTED_AGE") {
  #   mode_age <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
  #   sqf_df[na_rows, feature] <- mode_age$M
  # } else if (feature == "SUSPECT_SEX") {
  #   sqf_df[sqf_df$SUSPECT_SEX == "MALE" | sqf_df$SUSPECT_SEX == "FEMALE", "SUSPECT_SEX"]
  # }
}

sqf_df = na.omit(sqf_df) # Remove any rows with missing value

##### Cast to correct data type #####
for (feature in c(features, dependent)) {
  # Should be factor
  if (feature == "FRISKED_FLAG" || feature == "SEARCHED_FLAG" || feature == "OTHER_CONTRABAND_FLAG" ||
      feature == "FIREARM_FLAG" || feature == "KNIFE_CUTTER_FLAG" || feature == "OTHER_WEAPON_FLAG" || feature == "WEAPON_FOUND_FLAG" ||
      feature == "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG" || feature == "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG" ||
      feature == "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG" || feature == "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG" ||
      feature == "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG" || feature == "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG" ||
      feature == "SUSPECT_ARRESTED_FLAG") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("Y", "N"))
  } else if (feature == "ISSUING_OFFICER_RANK" || feature == "SUPERVISING_OFFICER_RANK") {
    sqf_df[, feature] <- factor(sqf_df[, feature], ranks)
  } else if (feature == "STOP_WAS_INITIATED" || feature == "SUSPECTED_CRIME_DESCRIPTION") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  } else if (feature == "SUSPECT_SEX") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("MALE", "FEMALE"))
  }  else if (feature == "SUSPECT_RACE_DESCRIPTION" || feature == "CATEGORIZED_SUSPECT_REPORTED_AGE" ||
              feature == "CATEGORIZED_SUSPECT_HEIGHT" || feature == "CATEGORIZED_SUSPECT_WEIGHT" ||
              feature == "STOP_LOCATION_PRECINCT") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  }
}

##### Split data ######
df_rows <- nrow(sqf_df)
idx <- sample(x=df_rows, size=as.integer(0.20*df_rows))
test <- sqf_df[idx, ]
training <- sqf_df[-idx, ]

##### Install packages #####
# install.packages('e1071', dependencies = TRUE)
library(class)
library(e1071)

##### Main function #####
class(sqf_df)
prop.table

# Get table of percentage for class and survived
##### Naive bayes #####
nBayes_arrest <- naiveBayes(
  SUSPECT_ARRESTED_FLAG ~ .,
  data=training
)
##### Predict tests ####
# Use predict function to predict
predict_arrest <- predict(nBayes_arrest, test, type="class")
test_arrest <- test$SUSPECT_ARRESTED_FLAG
table_k <- table(test_arrest, predict_arrest)
accuracy_k <- sum(diag(table_k)) / sum(table_k)
print("Table Naive Bayes")
print(table_k)
print(paste("Accuracy: ", accuracy_k))
