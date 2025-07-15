#This code has been made with the goal of creating a list of drugs and their sensitivity and specificity
#This code involves creating a confusion matrix between two columns of descriptive data by transforming the data to be binomial



#reading in files containing prescriptions and diagnosis of a set of patients
#note the exact path to the file must be read for R to find the file and can be found by checking the properties of the file

prescriptions <- read.csv('path to prescriptions.csv')

diagnoses <- read.csv('path to diagnoses.csv')

#creating a common column between the data sets that will allow joining of data later
#this example joins the subject ID with the hospital admission ID

prescriptions$subject_hadm_id <- paste(prescriptions$subject_id, prescriptions$hadm_id, sep = '_')

diagnoses$subject_hadm_id <- paste(diagnoses$subject_id, diagnoses$hadm_id, sep = '_')

#create a data set made up of only patients that have relevant ICD codes for the target condition

condition_subject <- diagnoses %>%
  filter(icd_code %in% c('relevant ICDs'))
         
#create same common column as before within the new data set
         
condition_subject$subject_hadm_id <- paste(condition_subject$subject_id, condition_subject$hadm_id, sep = '_')

#filter data set to avoid duplicates from later joining of other data sets

condition_subject_nodup <- condition_subject[!duplicated(condition_subject$subject_hadm_id), ]

#inner join the new data set with the prescription data
#this is done to create a column with the matching drug for each row

condition_drug_full_nodup <- inner_join(condition_subject_nodup, prescriptions)

#OPTIONAL
#filter out the data set for routes of administration
#requires one of the data sets, most likely prescriptions, to have a route column

condition_drug_full_nodup_filtered <- condition_drug_full_nodup %>% filter(route %in% c('routes of interest'))

#create a frequency table for the drug column

condition_drug_full_nodup_filtered_freq <- condition_drug_full_nodup_filtered %>% group_by(drug) %>% summarise(count = n())

#create a list of drugs from the frequency table

condition_drug_list <- as.list(condition_drug_full_nodup_filtered_freq$drug)

#create a new data set by filtering the prescription data with the drug list
#this gices a data set with only the relevant patients for the list of drugs
#reduces the sample size from the prescription data, reducing computing memory needed

prescriptions_condition_drugs <- prescriptions %>% filter(drug %in% condition_drug_list)

#filter out duplicates

prescriptions_condition_drugs_nodup <- prescriptions_condition_drugs[!duplicated(prescriptions_condition_drugs$subject_hadm_id), ]

#inner join with the diagnoses data to gain ICD column

prescriptions_condition_diagnoses_nodup_icd <- inner_join(prescriptions_condition_drugs_nodup, diagnoses)

#create a new column that will assign rows with a condition ICD a value of 1 and rows without a value of 0

prescriptions_condition_diagnoses_nodup_icd <- prescriptions_condition_diagnoses_nodup_icd %>% mutate(condition_true=ifelse(icd_code %in% condition_icd, 1, 0))

#for loop that will produce a table of the drugs relevant to the target condition and their sensitivity and specificity
#the sensitivity and specificity shows how well the drugs will perform as a predictor/indicator of the condition (or history of)

for (i in 1:length(condition_drug_list)) {
  current_drug <- as.character(condition_drug_list[[i]])
  
  prescriptions_condition_diagnoses_nodup_icd <- prescriptions_condition_diagnoses_nodup_icd %>% mutate(drug_true=ifelse(drug %in% current_drug, 1, 0))
  
  model_drug <- glm(condition_true ~ drug_true, family = binomial(link= "logit"), data = prescriptions_condition_diagnoses_nodup_icd)
  
  predict_drug <- predict(model_drug, newdata = prescriptions_condition_diagnoses_nodup_icd, type = "response")
  
  roc_drug <- roc(prescriptions_condition_diagnoses_nodup_icd$condition_true, predict_drug)
  
  youdbest <- pROC::coords(roc_drug, "b", ret = "threshold", best.method = "youden")
  
  cm_drug <- table(ifelse(predict_drug > youdbest[[1]], 1, 0), prescriptions_condition_diagnoses_nodup_icd$condition_true)
  
  rval.tes <- epiR::epi.tests(cm_drug)
  
  sensitivity <- rval.tes$detail[rval.tes$detail$statistic == "se", "est"]
  
  specificity <- rval.tes$detail[rval.tes$detail$statistic == "sp", "est"]
  
  if(i == 1){
    condition_drug_ss <- data.frame('placeholder', 0.5, 0.5)
    
    colnames(condition_drug_ss) <- c("drug", "Sensitivity", "Specificity")
    
    condition_drug_ss[1,1] <- as.character(current_drug)
    
    condition_drug_ss[1,2] <- sensitivity
    
    condition_drug_ss[1,3] <- specificity
    
  } else if (i > 1){
    condition_drug_ss2 <- data.frame('placeholder', 0.5, 0.5)
    
    colnames(condition_drug_ss2) <- c("drug", "Sensitivity", "Specificity")
    
    condition_drug_ss2[1,1] <- as.character(current_drug)
    
    condition_drug_ss2[1,2] <- sensitivity
    
    condition_drug_ss2[1,3] <- specificity
    
    condition_drug_ss <- rbind(condition_drug_ss, condition_drug_ss2)
  }
  
}

#creates and saves a csv file containing the table created from the for loop

write.csv(condition_drug_ss, "condition sens and spec.csv")