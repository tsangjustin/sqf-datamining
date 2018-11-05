################## HEADER #######################
#  Company    : Stevens 
#  Project    : CS 513 Final Project
#  Purpose    : Perform kNN to predict arrest likelihood
#  First Name  : Justin
#  Last Name  : Tsang
#  Id			    : 
#  Date       : October 29, 2018
#  Comments   : NULLs and outliers removed

rm(list=ls())
#################################################
###### Load data #####
file_path <- "/Users/justint/Documents/2018-Fall/CS-513/Project/2_estimate_nulls/SQF-estimation.csv"

df <- read.csv(
  file=file_path,
  header=TRUE,
  sep=",",
  na.strings=c("(null)", "", "V", "(", "#N/A", "<NA>"),
  stringsAsFactors = FALSE
)

# features <- c("STOP_WAS_INITIATED", "ISSUING_OFFICER_RANK", "SUPERVISING_OFFICER_RANK", "SUSPECTED_CRIME_DESCRIPTION",
#               "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
#               "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
#               "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
#               "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
#               "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
#               "SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "SUSPECT_HEIGHT", "SUSPECT_WEIGHT",
#               "STOP_LOCATION_PRECINCT")
features <- c("STOP_FRISK_TIME_SECONDS", "SUSPECTED_CRIME_DESCRIPTION",
              "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
              "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
              "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
              "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
              "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
              "SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION",
              "STOP_LOCATION_PRECINCT")
# features <- c("SUSPECTED_CRIME_DESCRIPTION",
#               "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
#               "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG")
dependent <- c("SUSPECT_ARRESTED_FLAG")

ranks <- c("POF", "POM", "DT1", "DT2", "DT3", "DTS", "SSA", "SGT", "SDS", "LSA", "LT", "CPT", "DI", "LCD")
sqf_df <- df[c(features, dependent)]

##### Normalize #####
mmnorm <- function(x, minx, maxx) {
  z <- (x-minx) / (maxx-minx)
  return(z)
}

##### CLEANUP DATA #####
library(modeest)
library(kknn)

for (feature in features) {
  na_rows <- is.na(sqf_df[, feature])
  if (feature == "FIREARM_FLAG" || feature == "KNIFE_CUTTER_FLAG" || feature == "OTHER_WEAPON_FLAG" || feature == "WEAPON_FOUND_FLAG" ||
       feature == "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG" || feature == "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG" ||
       feature == "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG" || feature == "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG" ||
       feature == "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG" || feature == "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG") {
    sqf_df[na_rows, feature] <- "N"
  } else if (feature == "STOP_FRISK_TIME_SECONDS" || feature == "SUSPECT_REPORTED_AGE") {
    sqf_df[, feature] <- as.numeric(sqf_df[, feature])
    mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    mode_feature <- mlv_feature$M # Get the mode value
    sqf_df[na_rows, feature] <- mode_feature
  } else if (feature == "SUSPECT_SEX") {
    NA_sex <- is.na(sqf_df$SUSPECT_SEX)
    
    feature_col <- c("SUSPECT_WEIGHT", "SUSPECT_HEIGHT", "SUSPECT_HAIR_COLOR", "SUSPECT_RACE_DESCRIPTION")
    predict_col <- c("SUSPECT_SEX")
    predict_set <- df[, c(feature_col, predict_col)]
    
    for (feature in c(feature_col, predict_col)) {
      if (feature == "SUSPECT_HAIR_COLOR") {
        na_rows <- is.na(predict_set[, feature])
        predict_set[na_rows, feature] <- "XXX"
        predict_set[, feature] <- factor(predict_set[, feature])
      } else if (feature == "SUSPECT_RACE_DESCRIPTION") {
        na_rows <- is.na(predict_set[, feature])
        predict_set[, feature] <- factor(predict_set[, feature])
        train_race <- predict_set[!na_rows, ]
        predict_race <- predict_set[na_rows, ]
        k <- 3
        f <- as.formula(paste("SUSPECT_RACE_DESCRIPTION ~ ", paste(c("SUSPECT_WEIGHT", "SUSPECT_HEIGHT", "SUSPECT_HAIR_COLOR"), collapse = " + ")))
        predict_race <- kknn(
          formula=f,
          train=train_race,
          test=predict_race,
          k=k,
          kernel="rectangular"
        )
        predict_race <- fitted(predict_race)
        predict_set[na_rows, feature] <- predict_race
        # print(paste("Predict race: ", predict_race))
        sqf_df[na_rows, feature] <- predict_race
      } else if (feature == "SUSPECT_SEX") {
        predict_set[, feature] <- factor(predict_set[, feature])
      } else if (feature == "SUSPECT_WEIGHT" || feature == "SUSPECT_HEIGHT") {
        na_rows <- is.na(predict_set[, feature])
        mean_feature <- mean(predict_set[, feature], na.rm = TRUE)
        print(paste("Mean of ", feature, ": ", mean_feature))
        predict_set[na_rows, feature] <- mean_feature
        min_feature <- min(predict_set[, feature])
        max_feature <- max(predict_set[, feature])
        predict_set[, feature] <- mmnorm(predict_set[, feature], min_feature, max_feature)
      }
    }
    
    predict_sex <- predict_set[NA_sex, ]
    train_sex <- predict_set[!NA_sex, ]
    
    k <- 3
    predict_k <- kknn(
      formula=SUSPECT_SEX ~ .,
      train=train_sex,
      test=predict_sex,
      k=k,
      kernel="rectangular"
    )
    predict_sex <- fitted(predict_k)
    sqf_df[NA_sex, feature] <- as.character(predict_sex)
  }
}

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
  }  else if (feature == "SUSPECT_RACE_DESCRIPTION") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  } else if (feature == "STOP_FRISK_TIME_SECONDS" || feature == "SUSPECT_REPORTED_AGE" ||
             feature == "STOP_LOCATION_PRECINCT") {
    min_feature <- min(sqf_df[, feature])
    max_feature <- max(sqf_df[, feature])
    sqf_df[, feature] <- mmnorm(sqf_df[, feature], min_feature, max_feature)
  }
}

##### Split data ######
df_rows <- nrow(sqf_df)
idx <- sample(x=df_rows, size=as.integer(0.25*df_rows))
test <- sqf_df[idx, ]
training <- sqf_df[-idx, ]

##### kNN #####
library(kknn)

k_seq = c(1, 2, 5, 10, 15, 20)
for (k in k_seq) {
  predict_k <- kknn(
    formula=SUSPECT_ARRESTED_FLAG ~ .,
    train=training,
    test=test,
    k=k,
    kernel="rectangular"
  )
  predict_arrest <- fitted(predict_k)
  test_arrest <- test$SUSPECT_ARRESTED_FLAG
  table_k <- table(test_arrest, predict_arrest)
  accuracy_k <- sum(diag(table_k)) / sum(table_k)
  print(paste("Table, k=", k, ":"))
  print(table_k)
  print(paste("Accuracy: ", accuracy_k))
}
