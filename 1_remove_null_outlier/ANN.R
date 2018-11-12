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

##### Load dataset #####
df <- read.csv(
  file=file_path,
  header=TRUE,
  sep=",",
  na.strings=c("(null)", "", "V", "("),
  stringsAsFactors = FALSE
)

# features <- c("SUSPECTED_CRIME_DESCRIPTION",
              # "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
              # "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
              # "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
              # "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
              # "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
              # "CATEGORIZED_SUSPECT_REPORTED_AGE", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "CATEGORIZED_SUSPECT_HEIGHT", "CATEGORIZED_SUSPECT_WEIGHT",
              # "STOP_LOCATION_PRECINCT")
features <- c("SUSPECTED_CRIME_DESCRIPTION",
              "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "FIREARM_FLAG", "KNIFE_CUTTER_FLAG",
              "OTHER_WEAPON_FLAG", "WEAPON_FOUND_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG",
              "BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", "BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG",
              "SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG",
              "SUSPECTS_ACTIONS_IDENTIFY_CRIME_PATTERN_FLAG",
              "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION",
              "STOP_LOCATION_PRECINCT")

dependent <- c("SUSPECT_ARRESTED_FLAG")

ranks <- c("POF", "POM", "DT1", "DT2", "DT3", "DTS", "SSA", "SGT", "SDS", "LSA", "LT", "CPT", "DI", "LCD")

sqf_df <- df[c(features, dependent)]


for (feature in features) {
  if (feature == "SUSPECTED_CRIME_DESCRIPTION" || feature == "SUSPECT_RACE_DESCRIPTION") {
    sqf_df[, feature] <- gsub('\\s+', '_', sqf_df[, feature])
    sqf_df[, feature] <- gsub('\\/+', '_', sqf_df[, feature])
  }
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
  }  else if (feature == "SUSPECT_RACE_DESCRIPTION") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  }
}

m_form <- as.formula(paste("~ ", paste(c(features, dependent), collapse = " + ")))
m <- model.matrix(
  m_form,
  data = sqf_df
)
m <- m[, -c(1)]

##### Split data ######
df_rows <- nrow(m)
idx <- sample(x=df_rows, size=as.integer(0.25*df_rows))
test <- m[idx, ]
training <- m[-idx, ]

##### Since single input and output, one input layer node and one utput layer node #####
# install.packages("neuralnet")
library("neuralnet")

len_m <- length(colnames(m))
f <- as.formula(
  paste("SUSPECT_ARRESTED_FLAGY + SUSPECT_ARRESTED_FLAGN ~",
        paste(colnames(m)[-c(len_m-1, len_m)], collapse = " + ")
      )
  )

net.sqrt <- neuralnet(
  formula = f,
  data=training,
  hidden=c(2 * len_m),
  stepmax = 1e6,
  threshold=0.01 # If weight does not change more than threshold consider stable
)

plot(net.sqrt)

simplify <- function(x) if (x[1] > x[2]) "Y" else "N"

test_arrest <- test[, c(len_m-1, len_m)]
simp_test_arrest <- as.factor(apply(test_arrest, 1, simplify))
predict_arrest <- compute(net.sqrt, test[, -c(len_m-1, len_m)])
predict_arrest <- predict_arrest$net.result
simp_predict_arrest <- as.factor(apply(predict_arrest, 1, simplify))

table_k <- table(test=simp_test_arrest, predict=simp_predict_arrest)
accuracy_k <- sum(diag(table_k)) / sum(table_k)
print("Table C5.0 D-Tree")
print(table_k)
print(paste("Accuracy: ", accuracy_k))
