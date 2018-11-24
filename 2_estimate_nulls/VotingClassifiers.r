################## HEADER #######################
#  Company    : Stevens 
#  Project    : CS 513 Final Project
#  Purpose    : Perform CART to predict arrest likelihood
#  First Name  : Justin
#  Last Name  : Tsang
#  Id			    : 
#  Date       : October 29, 2018
#  Comments   : NULLs and outliers replaced with mode

rm(list=ls())
#dev.off()
#################################################
###### Load data #####
setwd("/MDM/2018 Fall/CS513/sqf-datamining/2_estimate_nulls/")
#setwd("/Users/justint/Documents/2018-Fall/CS-513/Project/2_estimate_nulls/")
file_path <- "./SQF_clean.csv"

df <- read.csv(
  file=file_path,
  header=TRUE,
  sep=",",
  na.strings=c("(null)", "", "(", "#N/A", "<NA>")
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
  #"SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG",
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
             feature == "SUSPECT_HAIR_COLOR" ||
             feature == "SUSPECT_ARRESTED_FLAG") {
    sqf_df[, feature] <- factor(sqf_df[, feature])
  }
}
features2 <- c(
  "SUSPECTED_CRIME_DESCRIPTION",
  "SEARCHED_FLAG",
  "MONTH2",
  "WEAPON_FOUND_FLAG",
  "FIREARM_FLAG",
  "OTHER_CONTRABAND_FLAG",
  #"SEARCH_BASIS_INCIDENTAL_TO_ARREST_FLAG",
  "STOP_LOCATION_PRECINCT",
  "JURISDICTION_DESCRIPTION",
  "STOP_FRISK_TIME_MINUTES",
  "SUSPECT_REPORTED_AGE"
)
sqf_df_nb <- sqf_df[c(features2, dependent)]
m_form <- as.formula(paste(" ~ ", paste(c(features2), collapse = " + ")))
m <- model.matrix(
  m_form,
  data = sqf_df_nb
)
m <- m[, -c(1)]
m_2 <- as.data.frame(cbind(m, SUSPECT_ARRESTED_FLAG=sqf_df_nb$SUSPECT_ARRESTED_FLAG))
library(plyr)
m_2$SUSPECT_ARRESTED_FLAG <- factor(m_2$SUSPECT_ARRESTED_FLAG)
m_2$SUSPECT_ARRESTED_FLAG <- revalue(m_2$SUSPECT_ARRESTED_FLAG, c("1"="Y", "2"="N"))


library(randomForest)
library("C50")
library(rpart)
library(e1071)
library(class)

#for (i in 1:10){
  
  ##### Split data ######
  df_rows <- nrow(sqf_df)
  idx <- sample(x=df_rows, size=as.integer(0.25*df_rows))
  test <- sqf_df[idx, ]
  training <- sqf_df[-idx, ]
  test_nb<-sqf_df_nb[idx, ]
  training_nb <- sqf_df_nb[-idx, ]
  test_svm<-m_2[idx, ]
  training_svm <- m_2[-idx, ]
  
  test_arrest <- test$SUSPECT_ARRESTED_FLAG
  
  ####RF####
  fit <- randomForest(
    factor(SUSPECT_ARRESTED_FLAG) ~ .,
    data=training,
    importance = TRUE,
    ntree = 1000
  )
  predict_rf <- predict(fit, test)
  predict_rf_prob <- predict(fit, test,type="prob")
  table_fit <- table(actual=test_arrest, predict=predict_rf)
  accuracy_rf <- sum(diag(table_fit)) / sum(table_fit)

  ####C5.0####
  myC50Tree <- C5.0(
    SUSPECT_ARRESTED_FLAG ~ .,
    data=training
  )
  predict_C50 <- predict(myC50Tree, test, type="class")
  predict_C50_prob <- predict(myC50Tree, test, type="prob")
  table_fit <- table(actual=test$SUSPECT_ARRESTED_FLAG, predict=predict_C50)
  accuracy_C50 <- sum(diag(table_fit)) / sum(table_fit)

  ###CART####
  myTree <- rpart(
    SUSPECT_ARRESTED_FLAG ~ ., # Build model where SUSPECT_ARRESTED_FLAG dependent on rest features
    data=sqf_df  
  )
  predict_cart <- predict(myTree, test, type="class")
  predict_cart_prob <- predict(myTree, test,type="prob")
  table_fit <- table(actual=test$SUSPECT_ARRESTED_FLAG, predict=predict_cart)
  accuracy_cart <- sum(diag(table_fit)) / sum(table_fit)

  ####NB####
  nBayes_arrest <- naiveBayes(
    SUSPECT_ARRESTED_FLAG ~ .,
    data=training_nb
  )
  predict_nb <- predict(nBayes_arrest, test_nb, type="class")
  predict_nb_prob <- predict(nBayes_arrest, test_nb, type="raw")
  table_fit <- table(actual=test$SUSPECT_ARRESTED_FLAG, predict=predict_nb)
  accuracy_nb <- sum(diag(table_fit)) / sum(table_fit)

  ####SVM####
  svm.model <- svm(
    factor(SUSPECT_ARRESTED_FLAG) ~ .,
    data=training_svm,
    probability=TRUE
  )
  predict_svm <- predict(svm.model, test_svm)
  predict_svm_prob <- attr(predict(svm.model, test_svm,probability=TRUE),"probabilities")
  table_fit <- table(actual=test$SUSPECT_ARRESTED_FLAG, predict=predict_svm)
  accuracy_svm <- sum(diag(table_fit)) / sum(table_fit)

  p_rf<-as.numeric(as.character(revalue(predict_rf,c("Y"="1","N"="0"))))
  p_C50<-as.numeric(as.character(revalue(predict_C50,c("Y"="1","N"="0"))))
  p_cart<-as.numeric(as.character(revalue(predict_cart,c("Y"="1","N"="0"))))
  p_nb<-as.numeric(as.character(revalue(predict_nb,c("Y"="1","N"="0"))))
  p_svm<-as.numeric(as.character(revalue(predict_svm,c("Y"="1","N"="0"))))
  
  p_singlevote<-(p_rf+p_C50+p_cart+p_nb+p_svm)/5
  p_vote<-revalue(factor(p_singlevote<.5),c("FALSE"="Y","TRUE"="N"))
  table_fit <- table(actual=test$SUSPECT_ARRESTED_FLAG, predict=p_vote)
  accuracy_sv <- sum(diag(table_fit)) / sum(table_fit)

  p_avg<-(predict_rf_prob[,"Y"]+predict_C50_prob[,"Y"]+predict_cart_prob[,"Y"]+predict_nb_prob[,"Y"]+predict_svm_prob[,"Y"])/5
  p_avgvote<-revalue(factor(p_avg<.5),c("FALSE"="Y","TRUE"="N"))
  table_fit <- table(actual=test$SUSPECT_ARRESTED_FLAG, predict=p_avgvote)
  accuracy_av <- sum(diag(table_fit)) / sum(table_fit)
  
  p_weighted<-(.3*predict_rf_prob[,"Y"]+.25*predict_C50_prob[,"Y"]+.25*predict_cart_prob[,"Y"]+.1*predict_nb_prob[,"Y"]+.1*predict_svm_prob[,"Y"])
  p_wavote<-revalue(factor(p_weighted<.5),c("FALSE"="Y","TRUE"="N"))
  table_fit <- table(actual=test$SUSPECT_ARRESTED_FLAG, predict=p_wavote)
  accuracy_wav <- sum(diag(table_fit)) / sum(table_fit)
  
  
  
  print(paste("RF Accuracy: ", accuracy_rf))
  print(paste("C5.0 Accuracy: ", accuracy_C50))
  print(paste("CART Accuracy: ", accuracy_cart))
  print(paste("NB Accuracy: ", accuracy_nb))
  print(paste("SVMAccuracy: ", accuracy_svm))
  print(paste("Single Vote Accuracy: ", accuracy_sv))
  print(paste("Average Vote Accuracy: ", accuracy_av))
  print(paste("Weighted Average Vote Accuracy: ", accuracy_wav))

  
  
  
