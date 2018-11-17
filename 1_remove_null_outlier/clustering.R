################## HEADER #######################
#  Company    : Stevens 
#  Project    : CS 513 Final Project
#  Purpose    : Perform Naive Bayes to predict arrest likelihood
#  First Name  : Justin
#  Last Name  : Tsang
#  Id			    : 
#  Date       : October 29, 2018
#  Comments   : NULLs and outliers removed

rm(list=ls())
#################################################
###### Load data #####
#setwd("/Users/justint/Documents/2018-Fall/CS-513/Project/1_remove_null_outlier/")
setwd("/MDM/2018 Fall/CS513/sqf-datamining/1_remove_null_outlier/")

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
# features <- c(
#   "STOP_FRISK_TIME_MINUTES",
#   "MONTH2",
#   "DAY2",
#   "STOP_WAS_INITIATED",
#   "ISSUING_OFFICER_RANK",
#   "SUPERVISING_OFFICER_RANK",
#   "JURISDICTION_DESCRIPTION",
#   "OBSERVED_DURATION_MINUTES",
#   "SUSPECTED_CRIME_DESCRIPTION",
#   "STOP_DURATION_MINUTES",
#   "OFFICER_EXPLAINED_STOP_FLAG",
#   "OTHER_PERSON_STOPPED_FLAG",
#   "OFFICER_IN_UNIFORM_FLAG",
#   "ID_CARD_IDENTIFIES_OFFICER_FLAG",
#   "SHIELD_IDENTIFIES_OFFICER_FLAG",
#   "VERBAL_IDENTIFIES_OFFICER_FLAG",
#   "FRISKED_FLAG",
#   "SEARCHED_FLAG",
#   "OTHER_CONTRABAND_FLAG",
#   "FIREARM_FLAG",
#   "KNIFE_CUTTER_FLAG",
#   "OTHER_WEAPON_FLAG",
#   "WEAPON_FOUND_FLAG",
#   "PHYSICAL_FORCE_CEW_FLAG",
#   "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG",
#   "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
#   "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG",
#   "PHYSICAL_FORCE_OTHER_FLAG",
#   "PHYSICAL_FORCE_RESTRAINT_USED_FLAG",
#   "PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG",
#   "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG",
#   "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG",
#   "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
#   "SUSPECTS_ACTIONS_CASING_FLAG",
#   "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG",
#   "SUSPECTS_ACTIONS_DECRIPTION_FLAG",
#   "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
#   "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
#   "SUSPECTS_ACTIONS_LOOKOUT_FLAG",
#   "SUSPECTS_ACTIONS_OTHER_FLAG",
#   "SUSPECTS_ACTIONS_PROXIMITY_TO_SCENE_FLAG",
#   "SEARCH_BASIS_ADMISSION_FLAG",
#   "SEARCH_BASIS_CONSENT_FLAG",
#   "SEARCH_BASIS_HARD_OBJECT_FLAG",
#   "SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG",
#   "SEARCH_BASIS_OTHER_FLAG",
#   "SEARCH_BASIS_OUTLINE_FLAG",
#   # "DEMEANOR_OF_PERSON_STOPPED",
#   "SUSPECT_REPORTED_AGE",
#   "SUSPECT_SEX",
#   "SUSPECT_RACE_DESCRIPTION",
#   "SUSPECT_HEIGHT",
#   "SUSPECT_WEIGHT",
#   "SUSPECT_BODY_BUILD_TYPE",
#   "SUSPECT_EYE_COLOR",
#   "SUSPECT_HAIR_COLOR",
#   "STOP_LOCATION_PRECINCT"
# )
features <- c(
  "SUSPECTED_CRIME_DESCRIPTION",
  "SEARCHED_FLAG",
  "MONTH2",
  "WEAPON_FOUND_FLAG",
  "FIREARM_FLAG",
  "OTHER_CONTRABAND_FLAG",
  "SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG",
  "STOP_LOCATION_PRECINCT",
  "JURISDICTION_DESCRIPTION",
  "STOP_FRISK_TIME_MINUTES",
  "SUSPECT_REPORTED_AGE"
)
dependent <- c("SUSPECT_ARRESTED_FLAG")
sqf_df <- df[c(features, dependent)]
sqf_df = na.omit(sqf_df) # Remove any rows with missing value

##### Level for features #####
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

for (col in colnames(sqf_df)) {
  NA_rows <- is.na(sqf_df[, col])
  print(paste("NA Rows for col ", col))
  print(sqf_df[NA_rows, 1])
}

##### Cast to correct data type #####
mmnorm <- function(x,minx,maxx) {
  z <- (x-minx)/(maxx-minx)
  return(z)
}

for (feature in c(features, dependent)) {
  # Should be factor
  if (feature == "STOP_FRISK_DOM" ||
      feature == "STOP_FRISK_TIME_MINUTES") {
    sqf_df[, feature] = as.numeric(sqf_df[, feature])
    min_feature <- min(sqf_df[, feature])
    max_feature <- max(sqf_df[, feature])
    sqf_df[, feature] <- mmnorm(sqf_df[, feature], min_feature, max_feature)
  } else if (feature == "DAY2") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = days)
  } else if (feature == "MONTH2") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = months)
  } else if (feature == "ISSUING_OFFICER_RANK" ||
     feature == "SUPERVISING_OFFICER_RANK") {
    sqf_df[, feature] <- factor(sqf_df[, feature], ranks)
  } else if (feature == "OBSERVED_DURATION_MINUTES" ||
             feature == "STOP_DURATION_MINUTES") {
    sqf_df[, feature] = as.numeric(sqf_df[, feature])
    min_feature <- min(sqf_df[, feature])
    max_feature <- max(sqf_df[, feature])
    sqf_df[, feature] <- mmnorm(sqf_df[, feature], min_feature, max_feature)
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
  } else if (feature == "SUSPECT_REPORTED_AGE" ||
             feature == "SUSPECT_HEIGHT" ||
             feature == "SUSPECT_WEIGHT" ||
            feature == "STOP_LOCATION_PRECINCT") {
    sqf_df[, feature] = as.numeric(sqf_df[, feature])
    min_feature <- min(sqf_df[, feature])
    max_feature <- max(sqf_df[, feature])
    sqf_df[, feature] <- mmnorm(sqf_df[, feature], min_feature, max_feature)
  }
}

##### Need to make dummy data #####
m_form <- as.formula(paste(" ~ ", paste(c(features), collapse = " + ")))
m <- model.matrix(
  m_form,
  data = sqf_df
)
m <- m[, -c(1)]
m_2 <- as.data.frame(cbind(m, SUSPECT_ARRESTED_FLAG=sqf_df$SUSPECT_ARRESTED_FLAG))
library(plyr)
m_2$SUSPECT_ARRESTED_FLAG <- factor(m_2$SUSPECT_ARRESTED_FLAG)
m_2$SUSPECT_ARRESTED_FLAG <- revalue(m_2$SUSPECT_ARRESTED_FLAG, c("1"="Y", "2"="N"))

# df_dist <- dist(sqf_df[, features])
df_dist <- dist(m)
clust <- hclust(
  df_dist,
  method="average"
)

# Get table of percentage for class and survived
clust <- cutree(clust, 2) # Cut tree into 2 clusters
table_k <- table(Hclust=clust, Actual=sqf_df[, dependent]) # Compare prediction to output
accuracy_k <- sum(diag(table_k)) / sum(table_k)
print("Table H Clustering")
print(table_k)
print(paste("Accuracy: ", accuracy_k))

###### kmeans #####
kmeans_df <- kmeans(
  m,
  centers = 2,
  nstart = 10
) # Reinit centroids 10 times for 2 clusters
k_clust <- kmeans_df$cluster
str(k_clust)
table_k <- table(kmeans=k_clust, actual=sqf_df[, dependent]) # 1 and 2 are arbitary
accuracy_k <- sum(diag(table_k)) / sum(table_k)
print("Table K-Means Clustering")
print(table_k)
print(paste("Accuracy: ", accuracy_k))

