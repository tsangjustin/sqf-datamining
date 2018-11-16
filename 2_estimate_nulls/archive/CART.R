################## HEADER #######################
#  Company    : Stevens 
#  Project    : CS 513 Final Project
#  Purpose    : Perform CART to predict arrest likelihood
#  First Name  : Justin
#  Last Name  : Tsang
#  Id			    : 
#  Date       : October 29, 2018
#  Comments   : NULLs and outliers removed

rm(list=ls())
#################################################
###### Load data #####
file_path <- "/Users/justint/Documents/2018-Fall/CS-513/Project/2_estimate_nulls/SQF-estimation-categorized.csv"

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
features <- c("STOP_FRISK_TIME_MINUTES", "ISSUING_OFFICER_RANK", "SUPERVISING_OFFICER_RANK", "SUSPECTED_CRIME_DESCRIPTION",
              "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
              "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
              "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
              "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
              "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
              "CATEGORIZED_SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "CATEGORIZED_SUSPECT_HEIGHT", "CATEGORIZED_SUSPECT_WEIGHT", "SUSPECT_RACE_DESCRIPTION",
              "STOP_LOCATION_PRECINCT")
# features <- c("STOP_WAS_INITIATED", "ISSUING_OFFICER_RANK", "SUPERVISING_OFFICER_RANK", "SUSPECTED_CRIME_DESCRIPTION",
#               "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
#               "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
#               "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
#               "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
#               "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
#               "CATEGORIZED_SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "CATEGORIZED_SUSPECT_HEIGHT", "CATEGORIZED_SUSPECT_WEIGHT",
#               "STOP_LOCATION_PRECINCT")
dependent <- c("SUSPECT_ARRESTED_FLAG")

ranks <- c("POF", "POM", "DT1", "DT2", "DT3", "DTS", "SSA", "SGT", "SDS", "LSA", "LT", "CPT", "DI", "LCD")
sqf_df <- df[c(features, dependent)]

##### Normalize #####
mmnorm <- function(x, minx, maxx) {
  z <- (x-minx) / (maxx-minx)
  return(z)
}

categorize_age <- function(ages) {
  if (ages <= 18) {
    return("<=18")
  } else if (ages < 30) {
    return("<30")
  } else if (ages < 40) {
    return("<40")
  } else if (ages < 50) {
    return("<50")
  } else if (ages < 60) {
    return("<60")
  } else {
    return("+60")
  }
}

categorize_height <- function(heights) {
  if (heights < 5) {
    return("<5")
  } else if (heights < 5.6) {
    return("<5'6")
  } else if (heights < 6) {
    return("<6'")
  } else if (heights < 6.6) {
    return("<6'6")
  } else if (heights < 7) {
    return("<7'")
  } else {
    return(">=7'")
  }
}

categorize_weight <- function(weights) {
  if (weights < 100) {
    return("<100")
  } else if (weights < 150) {
    return("<150")
  } else if (weights < 200) {
    return("<200")
  } else if (weights < 250) {
    return("<250")
  } else {
    return(">=250'")
  }
}

categorize_stop_time <- function(time) {
  if (time < 360) {
    return("Nightwatcher")
  } else if (time >= 360 && time < 720) {
    return("Morning")
  } else if (time >= 720 && time < 1080) {
    return("Afternoon")
  } else if (time >= 1080) {
    return("Evening")
  } else {
    return ("Outlier")
  }
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
  } else if (feature == "STOP_FRISK_TIME_MINUTES") {
    sqf_df[, feature] <- as.numeric(sqf_df[, feature])
    mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    mode_feature <- mlv_feature$M # Get the mode value
    sqf_df[na_rows, feature] <- mode_feature
    sqf_df[, feature] <- sapply(df[, feature], categorize_stop_time)
  } else if (feature == "CATEGORIZED_SUSPECT_REPORTED_AGE") {
    na_rows <- is.na(sqf_df[, feature])
    orig_value <- df[, "SUSPECT_REPORTED_AGE"]
    orig_value <- as.numeric(orig_value)
    mlv_feature <- mlv(orig_value, method="mfv", na.rm=TRUE)
    mode_feature <- mlv_feature$M
    df[na_rows, "SUSPECT_REPORTED_AGE"] <- mode_feature
    sqf_df[na_rows, feature] <- categorize_age(df[na_rows, "SUSPECT_REPORTED_AGE"])
  } else if (feature == "CATEGORIZED_SUSPECT_HEIGHT") {
    na_rows <- is.na(sqf_df[, feature])
    orig_value <- df[, "SUSPECT_HEIGHT"]
    orig_value <- as.numeric(orig_value)
    mean_feature <- mean(orig_value, na.rm=TRUE)
    df[na_rows, "SUSPECT_HEIGHT"] <- mean_feature
    sqf_df[na_rows, feature] <- categorize_height(df[na_rows, "SUSPECT_HEIGHT"])
  } else if (feature == "CATEGORIZED_SUSPECT_WEIGHT") {
    na_rows <- is.na(sqf_df[, feature])
    orig_value <- df[, "SUSPECT_WEIGHT"]
    orig_value <- as.numeric(orig_value)
    mean_feature <- mean(orig_value, na.rm=TRUE)
    df[na_rows, "SUSPECT_WEIGHT"] <- mean_feature
    sqf_df[na_rows, feature] <- categorize_weight(df[na_rows, "SUSPECT_WEIGHT"])
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
        na_race_rows <- is.na(predict_set[, feature])
        predict_set[, feature] <- factor(predict_set[, feature])
        train_race <- predict_set[!na_race_rows, ]
        predict_race <- predict_set[na_race_rows, ]
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
        predict_set[na_race_rows, feature] <- predict_race
        # print(paste("Predict race: ", predict_race))
        sqf_df[na_race_rows, feature] <- predict_race
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
    predict_arrest <- fitted(predict_k)
    sqf_df[NA_sex, feature] <- as.character(predict_arrest)
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
  }  else if (feature == "SUSPECT_RACE_DESCRIPTION" || feature == "STOP_FRISK_TIME_MINUTES" ||
              feature == "CATEGORIZED_SUSPECT_HEIGHT" || feature == "CATEGORIZED_SUSPECT_WEIGHT" ||
              feature == "CATEGORIZED_SUSPECT_REPORTED_AGE" || feature == "STOP_LOCATION_PRECINCT") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  }
}

##### Split data ######
df_rows <- nrow(sqf_df)
idx <- sample(x=df_rows, size=as.integer(0.25*df_rows))
test <- sqf_df[idx, ]
training <- sqf_df[-idx, ]
##### CART #####
library(rpart)
library(rpart.plot) # Enhance tree plot
library(rattle) # Fancy tree plot
library(RColorBrewer) # Color needed for rattle

myTree <- rpart(
  SUSPECT_ARRESTED_FLAG ~ ., # Build model where SUSPECT_ARRESTED_FLAG dependent on rest features
  data=sqf_df  
)

##### Visualize tree
prp(myTree)

# fancyRpartPlot(myTree)

test_arrest <- test$SUSPECT_ARRESTED_FLAG
predict_arrest <- predict(myTree, test, type="class")
table_k <- table(test_arrest, predict_arrest)
accuracy_k <- sum(diag(table_k)) / sum(table_k)
print("Table CART D-Tree")
print(table_k)
print(paste("Accuracy: ", accuracy_k))
