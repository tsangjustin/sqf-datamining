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
setwd("/Users/justint/Documents/2018-Fall/CS-513/Project/1_remove_null_outlier/")
# setwd("/MDM/2018 Fall/CS513/sqf-datamining/1_remove_null_outlier/")

file_path <- "./SQF_clean.csv"

df <- read.csv(
  file=file_path,
  header=TRUE,
  sep=",",
  na.strings=c("(null)", "", "(", "#N/A", "<NA>")
)

# features <- c("STOP_WAS_INITIATED", "ISSUING_OFFICER_RANK", "SUPERVISING_OFFICER_RANK", "SUSPECTED_CRIME_DESCRIPTION",
#               "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
#               "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
#               "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
#               "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
#               "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
#               "SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "SUSPECT_HEIGHT", "SUSPECT_WEIGHT",
#               "STOP_LOCATION_PRECINCT")
# features <- c("STOP_WAS_INITIATED", "ISSUING_OFFICER_RANK", "SUPERVISING_OFFICER_RANK", "SUSPECTED_CRIME_DESCRIPTION",
#               "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
#               "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
#               "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
#               "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
#               "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
#               "CATEGORIZED_SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "CATEGORIZED_SUSPECT_HEIGHT", "CATEGORIZED_SUSPECT_WEIGHT",
#               "STOP_LOCATION_PRECINCT")
features <- c(
  "STOP_FRISK_DOM",
  "STOP_FRISK_TIME_MINUTES",
  "MONTH2",
  "DAY2",
  "STOP_WAS_INITIATED",
  "ISSUING_OFFICER_RANK",
  "SUPERVISING_OFFICER_RANK",
  "JURISDICTION_DESCRIPTION",
  "OBSERVED_DURATION_MINUTES",
  "SUSPECTED_CRIME_DESCRIPTION",
  "STOP_DURATION_MINUTES",
  "OFFICER_EXPLAINED_STOP_FLAG",
  "OTHER_PERSON_STOPPED_FLAG",
  "OFFICER_IN_UNIFORM_FLAG",
  "ID_CARD_IDENTIFIES_OFFICER_FLAG",
  "SHIELD_IDENTIFIES_OFFICER_FLAG",
  "VERBAL_IDENTIFIES_OFFICER_FLAG",
  "FRISKED_FLAG",
  "SEARCHED_FLAG",
  "OTHER_CONTRABAND_FLAG",
  "FIREARM_FLAG",
  "KNIFE_CUTTER_FLAG",
  "OTHER_WEAPON_FLAG",
  "WEAPON_FOUND_FLAG",
  "PHYSICAL_FORCE_CEW_FLAG",
  "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG",
  "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
  "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG",
  "PHYSICAL_FORCE_OTHER_FLAG",
  "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
  "PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG",
  "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG",
  "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG",
  "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
  "SUSPECTS_ACTIONS_CASING_FLAG",
  "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG",
  "SUSPECTS_ACTIONS_DECRIPTION_FLAG",
  "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
  "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
  "SUSPECTS_ACTIONS_LOOKOUT_FLAG",
  "SUSPECTS_ACTIONS_OTHER_FLAG",
  "SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG",
  "SEARCH_BASIS_ADMISSION_FLAG",
  "SEARCH_BASIS_CONSENT_FLAG",
  "SEARCH_BASIS_HARD_OBJECT_FLAG",
  # "SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG",
  "SEARCH_BASIS_OTHER_FLAG",
  "SEARCH_BASIS_OUTLINE_FLAG",
  # "DEMEANOR_OF_PERSON_STOPPED",
  "SUSPECT_REPORTED_AGE",
  "SUSPECT_SEX",
  "SUSPECT_RACE_DESCRIPTION",
  "SUSPECT_HEIGHT",
  "SUSPECT_WEIGHT",
  "SUSPECT_BODY_BUILD_TYPE",
  "SUSPECT_EYE_COLOR",
  "SUSPECT_HAIR_COLOR",
  "STOP_LOCATION_PRECINCT"
)
dependent <- c("SUSPECT_ARRESTED_FLAG")
sqf_df <- df[c(features, dependent)]
sqf_df = na.omit(sqf_df) # Remove any rows with missing value

##### Initiate the feature levels #####
ranks <- c("POF", "POM", "DT1", "DT2", "DT3", "DTS", "SSA", "SGT", "SDS", "LSA", "LT", "CPT", "DI", "LCD")
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

##### CLEANUP DATA #####
# for (feature in features) {
#   na_rows <- is.na(sqf_df[, feature])
#   if (feature == "FIREARM_FLAG" || feature == "KNIFE_CUTTER_FLAG" || feature == "OTHER_WEAPON_FLAG" || feature == "WEAPON_FOUND_FLAG" ||
#       feature == "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG" || feature == "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG" ||
#       feature == "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG" || feature == "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG" ||
#       feature == "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG" || feature == "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG") {
#     sqf_df[na_rows, feature] <- "N"
#   }
#   # } else if (feature == "SUSPECT_REPORTED_AGE") {
#   #   mode_age <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
#   #   sqf_df[na_rows, feature] <- mode_age$M
#   # } else if (feature == "SUSPECT_SEX") {
#   #   sqf_df[sqf_df$SUSPECT_SEX == "MALE" | sqf_df$SUSPECT_SEX == "FEMALE", "SUSPECT_SEX"]
#   # }
# }

##### Cast to correct data type #####
for (feature in c(features, dependent)) {
  # Should be factor
  if (feature == "STOP_FRISK_TIME_MINUTES") {
    
  } else if (feature == "DAY2") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = days)
  } else if (feature == "MONTH2") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = months)
  } else if (feature == "ISSUING_OFFICER_RANK" ||
      feature == "SUPERVISING_OFFICER_RANK") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = ranks)
  } else if (feature == "STOP_DURATION_MINUTES") {
    
  } else if (feature == "OFFICER_EXPLAINED_STOP_FLAG" ||
     feature == "OTHER_PERSON_STOPPED_FLAG" ||
     feature == "OFFICER_IN_UNIFORM_FLAG" ||
     feature == "FRISKED_FLAG" ||
     feature == "SEARCHED_FLAG" ||
     feature == "OTHER_CONTRABAND_FLAG" ||
     feature == "FIREARM_FLAG" ||
     feature == "KNIFE_CUTTER_FLAG" ||
     feature == "OTHER_WEAPON_FLAG" ||
     feature == "WEAPON_FOUND_FLAG" ||
     feature == "PHYSICAL_FORCE_CEW_FLAG" ||
     feature == "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG" ||
     feature == "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG" ||
     feature == "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG" ||
     feature == "PHYSICAL_FORCE_OTHER_FLAG" ||
     feature == "PHYSICAL_FORCE_RESTRAINT_USED_FLAG" ||
     feature == "PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG" ||
     feature == "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG" ||
     feature == "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG" ||
     feature == "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG" ||
     feature == "SUSPECTS_ACTIONS_CASING_FLAG" ||
     feature == "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG" ||
     feature == "SUSPECTS_ACTIONS_DECRIPTION_FLAG" ||
     feature == "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG" ||
     feature == "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG" ||
     feature == "SUSPECTS_ACTIONS_LOOKOUT_FLAG" ||
     feature == "SUSPECTS_ACTIONS_OTHER_FLAG" ||
     feature == "SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG" ||
     feature == "SEARCH_BASIS_ADMISSION_FLAG" ||
     feature == "SEARCH_BASIS_CONSENT_FLAG" ||
     feature == "SEARCH_BASIS_HARD_OBJECT_FLAG" ||
     feature == "SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG" ||
     feature == "SEARCH_BASIS_OTHER_FLAG" ||
     feature == "SEARCH_BASIS_OUTLINE_FLAG" ||
     feature == "SUSPECT_ARRESTED_FLAG") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("Y", "N"))
  } else if (feature == "ID_CARD_IDENTIFIES_OFFICER_FLAG") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("I", "N"))
  } else if (feature == "SHIELD_IDENTIFIES_OFFICER_FLAG") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("S", "N"))
  } else if (feature == "VERBAL_IDENTIFIES_OFFICER_FLAG") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("V", "N"))
  } else if (feature == "SUSPECT_SEX") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("MALE", "FEMALE"))
  } else if (feature == "STOP_WAS_INITIATED" ||
      feature == "JURISDICTION_DESCRIPTION" ||
      feature == "SUSPECTED_CRIME_DESCRIPTION" ||
      feature == "SUSPECT_RACE_DESCRIPTION" ||
      feature == "SUSPECT_BODY_BUILD_TYPE" ||
      feature == "SUSPECT_EYE_COLOR" ||
      feature == "SUSPECT_HAIR_COLOR") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  }
}


##### CART #####
library(rpart)
library(rpart.plot) # Enhance tree plot
#library(rattle) # Fancy tree plot
#library(RColorBrewer) # Color needed for rattle

accuracies<-array( dim=c(10,0) )
for (i in 1:10){
    
  ##### Split data ######
  df_rows <- nrow(sqf_df)
  idx <- sample(x=df_rows, size=as.integer(0.25*df_rows))
  test <- sqf_df[idx, ]
  training <- sqf_df[-idx, ]
  
  
  myTree <- rpart(
    SUSPECT_ARRESTED_FLAG ~ ., # Build model where SUSPECT_ARRESTED_FLAG dependent on rest features
    data=training  
  )
  
  ##### Plot Decision Tree #####
  par(mar=c(1,1,1,1))
  png(filename="./CART.png", width=1900, height=1900)
  prp(myTree)
  png(filename="./CART-Fancy.png", width=1900, height=1900)
  fancyRpartPlot(myTree)
  dev.off()
  
  test_arrest <- test$SUSPECT_ARRESTED_FLAG
  predict_arrest <- predict(myTree, test, type="class")
  table_k <- table(test_arrest, predict_arrest)
  accuracies[i] <- sum(diag(table_k)) / sum(table_k)
  print("Table CART D-Tree")
  print(table_k)
  print(paste("Accuracy: ", accuracies[i]))
}

accuracies
