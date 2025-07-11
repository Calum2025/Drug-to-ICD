

prescriptions <- read.csv('prescriptions.csv')

diagnoses <- read.csv('diagnosis.csv')

prescriptions$subject_hadm_id <- paste(prescriptions$subject_id, prescriptions$hadm_id, sep = '_')

diagnoses$subject_hadm_id <- paste(diagnoses$subject_id, diagnoses$hadm_id, sep = '_')

angina_subject <- diagnoses %>%
  filter(icd_code %in% c('4130', '4131', '4139', 'I20', 'I200', 'I201', 'I202', 'I208', 'I209', 'I237', 'I2511', 'I25110', 'I25111', 'I25112', 'I25118', 'I25119', 'I257', 'I2570', 'I25700', 'I25701', 'I25702', 'I25708', 'I25709', 'I2571', 'I25710', 'I25711', 'I25712', 'I25718', 'I25719', 'I2572', 'I25720', 'I25721', 'I25722', 'I25728', 'I25729', 'I2573', 'I25730', 'I25731', 'I25732', 'I25738', 'I25739', 'I2575', 'I25750', 'I25751', 'I25752', 'I25758', 'I25759', 'I2576', 'I25760', 'I25761', 'I25762', 'I25768', 'I25769', 'I2579', 'I25790', 'I25791', 'I25792', 'I25798', 'I25799'))

angina_subject$subject_hadm_id <- paste(angina_subject$subject_id, angina_subject$hadm_id, sep = '_')

angina_subject_nodup <- angina_subject[!duplicated(angina_subject$subject_hadm_id), ]

angina_drug_ful_nodup <- inner_join(angina_subject_nodup, prescriptions)

angina_drug_ful_nodup_filtered <- angina_drug_ful_nodup %>% filter(route %in% c('TD', 'PO', 'PO/NG', 'SL'))

angina_drug_ful_nodup_filtered_freq <- angina_drug_ful_nodup_filtered %>% group_by(drug) %>% summarise(count = n())

angina_drug_list <- as.list(angina_drug_ful_nodup_filtered_freq$drug)

prescriptions_angina_drugs <- prescriptions %>% filter(drug %in% angina_drug_list)

prescriptions_angina_drugs_nodup <- prescriptions_angina_drugs[!duplicated(prescriptions_angina_drugs$subject_hadm_id), ]

p_a_d_nodup_icd <- inner_join(prescriptions_angina_drugs_nodup, diagnoses)

p_a_d_nodup_icd <- p_a_d_nodup_icd %>% mutate(angina_true=ifelse(icd_code %in% angina_icd, 1, 0))

for (i in 1:length(angina_drug_list)) {
  current_drug <- as.character(angina_drug_list[[i]])
  
  p_a_d_nodup_icd <- p_a_d_nodup_icd %>% mutate(drug_true=ifelse(drug %in% current_drug, 1, 0))
  
  model_drug <- glm(angina_true ~ drug_true, family = binomial(link= "logit"), data = p_a_d_nodup_icd)
  
  predict_drug <- predict(model_drug, newdata = p_a_d_nodup_icd, type = "response")
  
  roc_drug <- roc(p_a_d_nodup_icd$angina_true, predict_drug)
  
  youdbest <- pROC::coords(roc_drug, "b", ret = "threshold", best.method = "youden")
  
  cm_drug <- table(ifelse(predict_drug > youdbest[[1]], 1, 0), p_a_d_nodup_icd$angina_true)
  
  rval.tes <- epiR::epi.tests(cm_drug)
  
  sensitivity <- rval.tes$detail[rval.tes$detail$statistic == "se", "est"]
  
  specificity <- rval.tes$detail[rval.tes$detail$statistic == "sp", "est"]
  
  if(i == 1){
    angina_drug_ss <- data.frame('placeholder', 0.5, 0.5)
    
    colnames(angina_drug_ss) <- c("drug", "Sensitivity", "Specificity")
    
    angina_drug_ss[1,1] <- as.character(current_drug)
    
    angina_drug_ss[1,2] <- sensitivity
    
    angina_drug_ss[1,3] <- specificity
    
  } else if (i > 1){
    angina_drug_ss2 <- data.frame('placeholder', 0.5, 0.5)
    
    colnames(angina_drug_ss2) <- c("drug", "Sensitivity", "Specificity")
    
    angina_drug_ss2[1,1] <- as.character(current_drug)
    
    angina_drug_ss2[1,2] <- sensitivity
    
    angina_drug_ss2[1,3] <- specificity
    
    angina_drug_ss <- rbind(angina_drug_ss, angina_drug_ss2)
  }
  
}