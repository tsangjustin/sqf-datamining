library(modeest)

file_path <- "/Users/justint/Documents/2018-Fall/CS-513/Project/2_estimate_nulls/SQF-dirty.csv"

df <- read.csv(
  file=file_path,
  header=TRUE,
  sep=",",
  na.strings=c("(null)", "", "(", "#N/A", "<NA>", "(nu"),
  stringsAsFactors = FALSE
)

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
  "SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG",
  "SEARCH_BASIS_OTHER_FLAG",
  "SEARCH_BASIS_OUTLINE_FLAG",
  "DEMEANOR_OF_PERSON_STOPPED",
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

ranks <- c("POF", "POM", "DT1", "DT2", "DT3", "DTS", "SSA", "SGT", "SDS", "LSA", "LT", "CPT", "DI", "LCD")
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

sqf_df <- df[, c(features, dependent)]

# Create the function.
get_mode <- function(data, na.rm) {
  if (na.rm) {
    data <- na.omit(data)
  }
  levels <- unique(data)
  levels[which.max(tabulate(match(data, levels)))]
}

for (feature in features) {
  na_rows <- is.na(sqf_df[, feature])
  if (feature == "STOP_FRISK_DOM" ||
      feature == "STOP_FRISK_TIME_MINUTES") {
    sqf_df[, feature] <- as.numeric(sqf_df[, feature])
    mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    mode_feature <- mlv_feature$M # Get the mode value
    sqf_df[na_rows, feature] <- mode_feature
  } else if (feature == "STOP_WAS_INITIATED" ||
      feature == "JURISDICTION_DESCRIPTION" ||
      feature == "SUSPECTED_CRIME_DESCRIPTION") {
    sqf_df[, feature] <- gsub('\\s+', '_', sqf_df[, feature])
    sqf_df[, feature] <- gsub('\\/+', '_', sqf_df[, feature])
    sqf_df[, feature] <- as.factor(sqf_df[, feature])
    # mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    # mode_feature <- mlv_feature$M # Get the mode value
    mode_feature <- get_mode(sqf_df[, feature], na.rm = TRUE)
    sqf_df[na_rows, feature] <- mode_feature
  } else if (feature == "OFFICER_EXPLAINED_STOP_FLAG" ||
      feature == "OTHER_PERSON_STOPPED_FLAG" ||
      feature == "OFFICER_IN_UNIFORM_FLAG" ||
      feature == "ID_CARD_IDENTIFIES_OFFICER_FLAG" ||
      feature == "SHIELD_IDENTIFIES_OFFICER_FLAG" ||
      feature == "VERBAL_IDENTIFIES_OFFICER_FLAG" ||
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
      feature == "SEARCH_BASIS_OUTLINE_FLAG") {
    sqf_df[na_rows, feature] <- "N"
    sqf_df[, feature] <- as.factor(sqf_df[, feature])
  } else if (feature == "SUSPECT_REPORTED_AGE") {
    # sqf_df[, feature] <- as.numeric(sqf_df[, feature])
    # mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    # mode_feature <- mlv_feature$M # Get the mode value
    mode_feature <- get_mode(sqf_df[, feature], na.rm = TRUE)
    sqf_df[na_rows, feature] <- mode_feature
  } else if (feature == "SUSPECT_SEX") {
    sqf_df[, feature] <- as.factor(sqf_df[, feature])
    # mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    # mode_feature <- mlv_feature$M # Get the mode value
    mode_feature <- get_mode(sqf_df[, feature], na.rm = TRUE)
    sqf_df[na_rows, feature] <- mode_feature
  } else if (feature == "SUSPECT_RACE_DESCRIPTION") {
    sqf_df[, feature] <- gsub('\\s+', '_', sqf_df[, feature])
    sqf_df[, feature] <- gsub('\\/+', '_', sqf_df[, feature])
    sqf_df[, feature] <- as.factor(sqf_df[, feature])
    # mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    # mode_feature <- mlv_feature$M # Get the mode value
    mode_feature <- get_mode(sqf_df[, feature], na.rm = TRUE)
    sqf_df[na_rows, feature] <- mode_feature
  } else if (feature == "SUSPECT_HEIGHT" ||
     feature == "SUSPECT_WEIGHT") {
    sqf_df[, feature] <- as.numeric(sqf_df[, feature])
    # mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    # mode_feature <- mlv_feature$M # Get the mode value
    mode_feature <- get_mode(sqf_df[, feature], na.rm = TRUE)
    sqf_df[na_rows, feature] <- mode_feature
  } else if (feature == "SUSPECT_BODY_BUILD_TYPE" ||
     feature == "SUSPECT_EYE_COLOR" ||
     feature == "SUSPECT_HAIR_COLOR") {
    sqf_df[, feature] <- gsub('\\s+', '_', sqf_df[, feature])
    sqf_df[, feature] <- gsub('\\/+', '_', sqf_df[, feature])
    sqf_df[, feature] <- as.factor(sqf_df[, feature])
    # mlv_feature <- mlv(sqf_df[, feature], method="mfv", na.rm=TRUE) # most frequent value
    # mode_feature <- mlv_feature$M # Get the mode value
    mode_feature <- get_mode(sqf_df[, feature], na.rm = TRUE)
    sqf_df[na_rows, feature] <- mode_feature
  }
}

output_file <- "/Users/justint/Documents/2018-Fall/CS-513/Project/2_estimate_nulls/SQF_clean.csv"
write.csv(sqf_df, file=output_file)
