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
file_path <- "/Users/justint/Documents/2018-Fall/CS-513/Project/data/sqf-2017.csv"

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

features <- c("STOP_WAS_INITIATED", "ISSUING_OFFICER_RANK", "SUPERVISING_OFFICER_RANK", "SUSPECTED_CRIME_DESCRIPTION",
              "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
              "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
              "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
              "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
              "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
              "SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "SUSPECT_HEIGHT", "SUSPECT_WEIGHT",
              "STOP_LOCATION_PRECINCT")
dependent <- c("SUSPECT_ARRESTED_FLAG")

###### Remove rows with bad values #####
# for (feature in features) {
#   if (feature == "STOP_WAS_INITIATED") {
#     na_rows <- is.na(df[, feature])
#     df <- df[!na_rows, ]
#   } else if (feature == "SUSPECT_SEX") {
#     valid_sex <- sqf_df$SUSPECT_SEX == "MALE" | sqf_df$SUSPECT_SEX == "FEMALE"
#     sqf_df <- sqf_df[valid_sex, ]
#     sqf_df <- sqf_df[!is.na(valid_sex), ]
#   }
# }

# rownames(sqf_df) <- seq(length=nrow(sqf_df)) # Reindex the row names

##### CLEANUP DATA #####
library(modeest)

for (feature in features) {
  na_rows <- is.na(sqf_df[, feature])
  if (feature == "FIREARM_FLAG" || feature == "KNIFE_CUTTER_FLAG" || feature == "OTHER_WEAPON_FLAG" ||
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

##### Cast to correct data type #####
for (feature in features) {
  # Should be factor
  if (feature == "FIREARM_FLAG" || feature == "KNIFE_CUTTER_FLAG" || feature == "OTHER_WEAPON_FLAG" ||
      feature == "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG" || feature == "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG" ||
      feature == "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG" || feature == "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG" ||
      feature == "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG" || feature == "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("Y", "N"))
  } else if (feature == "ISSUING_OFFICER_RANK" || feature == "SUSPECTED_CRIME_DESCRIPTION") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  }
}

##### Remove rows missing values #####
# sqf_df <- na.omit(sqf_df)
# rownames(df) <- seq(length=nrow(sqf_df))  # Reindex row numbers

##### Split data ######
# df_rows <- nrow(sqf_df)
# idx <- sample(x=df_rows, size=as.integer(0.3*df_rows))
# test <- sqf_df[idx, ]
# training <- sqf_df[-idx, ]
