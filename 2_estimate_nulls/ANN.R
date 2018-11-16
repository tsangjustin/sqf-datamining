################## HEADER #######################
#  Company    : Stevens 
#  Project    : CS 513 Final Project
#  Purpose    : Perform ANN to predict arrest likelihood
#  First Name  : Justin
#  Last Name  : Tsang
#  Id			    : 
#  Date       : October 29, 2018
#  Comments   : NULLs and outliers replaced with mode

rm(list=ls())
# dev.off()
#################################################
###### Load data #####
setwd("/Users/justint/Documents/2018-Fall/CS-513/Project/2_estimate_nulls/")
file_path <- "./SQF_clean.csv"

df <- read.csv(
  file=file_path,
  header=TRUE,
  sep=",",
  na.strings=c("(null)", "", "V", "(", "#N/A", "<NA>")
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

##### Level of features #####
ranks <- c("POF", "POM", "DT1", "DT2", "DT3", "DTS", "SSA", "SGT", "SDS", "LSA", "LT", "CPT", "DI", "LCD")
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

##### Normalize #####
mmnorm <- function(x,minx,maxx) {
  z <- (x-minx)/(maxx-minx)
  return(z)
}

# for (feature in features) {
#   if (feature == "SUSPECTED_CRIME_DESCRIPTION" || feature == "SUSPECT_RACE_DESCRIPTION") {
#     sqf_df[, feature] <- gsub('\\s+', '_', sqf_df[, feature])
#     sqf_df[, feature] <- gsub('\\/+', '_', sqf_df[, feature])
#   }
# }

##### Cast to correct data type #####
for (feature in c(features, dependent)) {
  # Should be factor
  if (feature == "STOP_FRISK_DOM" ||
      feature == "STOP_FRISK_TIME_MINUTES") {
    sqf_df[, feature] = as.numeric(sqf_df[, feature])
    min_feature <- min(sqf_df[, feature])
    max_feature <- max(sqf_df[, feature])
    sqf_df[, feature] <- mmnorm(sqf_df[, feature], min_feature, max_feature)
  } else if (feature == "MONTH2") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = months)
  } else if (feature == "DAY2") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = days)
  } else if (feature == "STOP_WAS_INITIATED") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  } else if (feature == "ISSUING_OFFICER_RANK" ||
             feature == "SUPERVISING_OFFICER_RANK") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = ranks)
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
  } else if (feature == "SUSPECTED_CRIME_DESCRIPTION") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  } else if (feature == "SUSPECT_SEX") {
    sqf_df[, feature] <- factor(sqf_df[, feature], levels = c("MALE", "FEMALE"))
  }  else if (feature == "SUSPECT_RACE_DESCRIPTION") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  } else if (feature == "SUSPECT_REPORTED_AGE" ||
             feature == "STOP_LOCATION_PRECINCT") {
    min_feature <- min(sqf_df[, feature])
    max_feature <- max(sqf_df[, feature])
    sqf_df[, feature] <- mmnorm(sqf_df[, feature], min_feature, max_feature)
  }
}

##### Need to make dummy data #####
m_form <- as.formula(paste(" ~ ", paste(c(features, dependent), collapse = " + ")))
m <- model.matrix(
  m_form,
  data = sqf_df
)
m_2 <- m[, -c(1)]
# m_2 <- as.data.frame(cbind(m, SUSPECT_ARRESTED_FLAG=sqf_df$SUSPECT_ARRESTED_FLAG))
# library(plyr)
# m_2$SUSPECT_ARRESTED_FLAG <- factor(m_2$SUSPECT_ARRESTED_FLAG)
# m_2$SUSPECT_ARRESTED_FLAG <- revalue(m_2$SUSPECT_ARRESTED_FLAG, c("1"="Y", "2"="N"))

##### Split data ######
df_rows <- nrow(m_2)
idx <- sample(x=df_rows, size=as.integer(0.25*df_rows))
test <- m_2[idx, ]
training <- m_2[-idx, ]

##### Since single input and output, one input layer node and one utput layer node #####
# install.packages("neuralnet")
library("neuralnet")

len_m <- length(colnames(m_2))
f <- as.formula(
  paste(
    paste(colnames(m_2)[c(len_m)], collapse = " + "),
    " ~",
    paste(colnames(m_2)[-c(len_m)], collapse = " + ")
  )
)

net.sqrt <- neuralnet(
  formula = f,
  data=training,
  hidden=c(2 * length(features)),
  stepmax = 1e6,
  threshold=0.01, # If weight does not change more than threshold consider stable
  linear.output = FALSE
)

plot(net.sqrt)

simplify <- function(x) if (x <= 0.5) "Y" else "N"

test_arrest <- test[, c(len_m)]
simp_test_arrest <- as.factor(sapply(test_arrest, simplify))
predict_arrest <- compute(net.sqrt, test[, -c(len_m)])
predict_arrest <- predict_arrest$net.result
simp_predict_arrest <- as.factor(sapply(predict_arrest, simplify))
table_k <- table(test=simp_test_arrest, predict=simp_predict_arrest)

accuracy_k <- sum(diag(table_k)) / sum(table_k)
print("Table ANN")
print(table_k)
print(paste("Accuracy: ", accuracy_k))
