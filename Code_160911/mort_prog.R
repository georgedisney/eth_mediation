# Packages needed ----------------------------------------------------------

library (dplyr)
library (haven)
library (sas7bdat)
library (readr)
library (broom)
library (reshape)
library (boot)

#### Load functions and models ------------------------------------------

source ("/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/data_functions.R")
source ("/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_models.R")

#### Read data into R and initial prep ----------------------------------------

read_data <- function (file_title){
  
  root <- "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/"
  file_title <- file_title 
  path <- paste(root, file_title, sep = "")
  mort_data_master <- read_csv(path)

  mort_data <- mort_data_master%>%
                filter (EqIncJen != "NA", 
                        AgeC_yrs < 75 & AgeC_yrs >=25,
                        TotMaori %in% c("1","5"),
                        SmkStat %in% c("1", "2", "3"),
                        !is.na(NZDep10),
                        HQual3 != "9")
  
  mort_data$EqIncJen <- ifelse (mort_data$EqIncJen < 1000, 1000, mort_data$EqIncJen)
  
  mort_data <- mort_data %>%
                mutate (EqIncJen_log = log(EqIncJen))%>%
                filter (EqIncJen_log > 0 & EqIncJen_log != "NaN")%>%
                mutate(EqIncJen_log_cent = EqIncJen_log - mean(EqIncJen_log),
                       NZDep10_cent = NZDep10 - 5,            
                       Age_group = sapply(AgeC_yrs, age_5),
                       Age_group_broad = sapply(AgeC_yrs, age_broad),
                       Retired = sapply(AgeC_yrs, retired),
                       WHO_stand = sapply(AgeC_yrs, WHO_stand_fun),
                       Ret_lfs = ifelse (AgeC_yrs > 64 & LabSt3 >1, 1, 0))%>%
                  droplevels.data.frame(.)
  
  # ENSURING VARIABLES ARE NUMERIC WHERE APPROPRIATE #
  
  mort_data$NZDep10<- as.numeric(mort_data$NZDep10)
  mort_data$NZDep10_cent<- as.numeric(mort_data$NZDep10_cent)
  mort_data$WHO_stand <- as.numeric(mort_data$WHO_stand)
                                                                                               
  # ENSURING VARIABLES ARE CATEGORICAL WHERE APPROPRIATE #
  
  mort_data$Retired <- as.factor(mort_data$Retired)
  mort_data$Sex <- as.factor(mort_data$Sex)
  mort_data$Link <- as.factor(mort_data$Link)
  mort_data$CarAccess <- as.factor(mort_data$CarAccess)
  mort_data$HQual3 <- as.factor(mort_data$HQual3)
  mort_data$TotMaori <- as.factor(mort_data$TotMaori)
  mort_data$LabSt3 <- as.factor(mort_data$LabSt3)
  mort_data$SmkStat <- as.factor(mort_data$SmkStat)
  mort_data$Age_group <- as.factor(mort_data$Age_group)
  mort_data$Age_group_broad <- as.factor(mort_data$Age_group_broad) 

  mort_data$TotMaori <- ifelse (mort_data$TotMaori == "1", 1, ifelse (mort_data$TotMaori == "5", 0, NA))   
  mort_data$Age_group <- relevel(mort_data$Age_group, ref = "6064")  
  
  mort_data$SmkStat <- ifelse (mort_data$SmkStat == "3", 1, ifelse (mort_data$SmkStat == "1", 3, 2))
  mort_data$SmkStat <- as.factor(mort_data$SmkStat)

mort_data  
  
}


#### Splitting cohorts into separate sexes, extra prep and calculating weights -----------------------------------------

sex_prep <- function (cohort, sex){
  #look at code below and assess whether can be moved up to previous function
 sex_mort_data <- filter (cohort,
                        Sex == sex,
                        EqIncJen != "NA",
                        EqIncJen_log > 0 & EqIncJen_log != "NaN")%>%
            droplevels.data.frame(.)

  sex_mort_data <- sex_mort_data %>%
                    mutate(WHO_prop = WHO_stand/0.5413,
                           total_gender = n())%>%
                    group_by(TotMaori)%>%
                    mutate(eth_sum = n(),
                           p_eth = eth_sum/total_gender)
 
 sex_mort_data <- sex_mort_data %>% 
                    group_by(Age_group,TotMaori)%>%
                    mutate(age_eth_sum = n())%>%
                    mutate (p_age_eth = (age_eth_sum/total_gender)/p_eth)

 sex_mort_data <- sex_mort_data %>%
                    mutate(weights = WHO_prop/p_age_eth)
 sex_mort_data

}



#### Cross world function for natural direct and indirect effects ------------------------------------------------------


cw_calcs <- function (cohort_sex, sex_sep_mod, sex_tob_mod, mod_set_name) {
  
  cw_cohort_sex <- cohort_sex
  
  # Natural effects for SEP
  cw_cohort_sex$cross_ref <- cw_cohort_sex$TotMaori 
  cw_cohort_sex$TotMaori <- ifelse(cw_cohort_sex$TotMaori == "0", 1, 1)
  
  cw_pred <- predict.glm(sex_sep_mod, type = "response", newdata = cw_cohort_sex)
  cw_pred <- as.data.frame(cw_pred)
  #sep_pred <- as.data.frame(sex_sep_mod$fitted.values) # new code
  sep_pred <- predict.glm(sex_sep_mod, type = "response", newdata = cohort_sex)
  sep_pred <- as.data.frame(sep_pred)
  
  cw_cohort_sex <- bind_cols(cw_cohort_sex, cw_pred)
  cw_cohort_sex <- bind_cols(cw_cohort_sex, sep_pred)
  
  # Natural effects for SEP + Smoking
  cw_pred_smo <- predict.glm(sex_tob_mod, type = "response", newdata = cw_cohort_sex)
  cw_pred_smo <- as.data.frame(cw_pred_smo)
  #smo_pred <- as.data.frame(sex_tob_mod$fitted.values)
  smo_pred <- predict.glm(sex_tob_mod, type = "response", newdata = cohort_sex)
  smo_pred <- as.data.frame(smo_pred)
  
  
  cw_cohort_sex <- bind_cols(cw_cohort_sex, cw_pred_smo)
  cw_cohort_sex <- bind_cols(cw_cohort_sex, smo_pred)
  
  # Controlled effects ie "never smoker"
  never_cohort_sex <- cohort_sex
  never_cohort_sex$SmkStat <- ifelse(never_cohort_sex$SmkStat == "3"|never_cohort_sex$SmkStat == "2", 1,1)
  never_cohort_sex$SmkStat <- as.factor(never_cohort_sex$SmkStat)
  
  never_pred <- predict.glm(sex_tob_mod, type = "response", newdata = never_cohort_sex)
  never_pred <- as.data.frame(never_pred)
  
  cw_cohort_sex <- bind_cols(cw_cohort_sex, never_pred)
  
  cw_cohort_sex <- cw_cohort_sex %>%
                    mutate (model_set = mod_set_name)
  
  
  # Observed risk calculation
  cw_cohort_sex$mort <- ifelse (cw_cohort_sex$Link == "1", 1, 0)
  
  cw_cohort_sex <- cw_cohort_sex
  
  risks <- cw_cohort_sex %>%
                    mutate (comb_weight = W_AgEthAdj*weights)%>%        
                    group_by(CenYear, Sex, cross_ref, model_set)%>%
                    summarise(obs_risk = weighted.mean(mort, w = comb_weight),
                              pred_sep_risk = weighted.mean (sep_pred, w = weights), #new
                              cw_sep_risk = weighted.mean(cw_pred, w = weights),
                              pred_smo_risk = weighted.mean(smo_pred, w = weights), #new
                              cw_smo_risk = weighted.mean(cw_pred_smo, w = weights),
                              never_risk = weighted.mean (never_pred, w = weights))%>%
                    mutate(Age_group_broad = "2574",
                           Date_run = Sys.Date())
  
  
  
  risks_age <- cw_cohort_sex %>%
                    mutate (comb_weight = W_AgEthAdj*weights)%>%        
                    group_by(CenYear,Sex,cross_ref, model_set, Age_group_broad)%>%
                    summarise(obs_risk = weighted.mean(mort, w = comb_weight),
                              pred_sep_risk = weighted.mean(sep_pred, w = weights),
                              cw_sep_risk = weighted.mean(cw_pred, w = weights),
                              pred_smo_risk = weighted.mean(smo_pred, w = weights),
                              cw_smo_risk = weighted.mean(cw_pred_smo, w = weights),
                              never_risk = weighted.mean (never_pred, w = weights))%>%
                    mutate(Date_run = Sys.Date())
  
  tab_risks <- bind_rows(risks, risks_age)    
  
  tab_risks
  
  
}





############ Code for running complete analysis including the model fit element ####################

run_mediation <- function(cohort, glm_sep_title = NULL, glm_tob_title = NULL, 
                          glm_sep_spec = NULL, glm_tob_spec = NULL, model_name) {


data_name <- read_data (file_title = paste ("mort", cohort, ".csv", sep = ""))

males_data <- sex_prep(data_name, sex = "1")
females_data <- sex_prep(data_name, sex = "2")



if (!is.null(glm_sep_spec)& !is.null(glm_tob_spec)){
  males_glm_sep <- glm(as.formula(glm_sep_spec),
                       family = binomial(link = "logit"), data = males_data, weights = W_AgEthAdj)
  
  females_glm_sep <- glm(as.formula(glm_sep_spec),
                       family = binomial(link = "logit"), data = females_data, weights = W_AgEthAdj)
  
  
  males_glm_tob <- glm(as.formula(glm_tob_spec),
                       family = binomial(link = "logit"), data = males_data, weights = W_AgEthAdj)
  
  females_glm_tob <- glm(as.formula(glm_tob_spec),
                         family = binomial(link = "logit"), data = females_data, weights = W_AgEthAdj)
  
  
  
  males_cw <- cw_calcs (cohort_sex = males_data, 
                        sex_sep_mod = males_glm_sep, 
                        sex_tob_mod =  males_glm_tob, 
                        mod_set_name = model_name)
  
  females_cw <- cw_calcs (cohort_sex = females_data, 
                          sex_sep_mod = females_glm_sep, 
                          sex_tob_mod = females_glm_tob, #replace 06 with 2006 
                          mod_set_name = model_name)

}


if (!is.null(glm_sep_title)& !is.null(glm_tob_title)){

root <- "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/regression_output/"  

males_glm_sep <- load(file = paste0 (root,"males_", glm_sep_title,".R"))
females_glm_sep <- load(file = paste0 (root,"females_", glm_sep_title,".R"))

males_glm_tob <- load(file = paste0 (root,"males_", glm_tob_title,".R"))
females_glm_tob <- load(file = paste0 (root,"females_", glm_tob_title,".R"))

males_cw <- cw_calcs (cohort_sex = males_data, 
                      sex_sep_mod = get(males_glm_sep), 
                      sex_tob_mod =  get(males_glm_tob), 
                      mod_set_name = model_name)

females_cw <- cw_calcs (cohort_sex = females_data, 
                        sex_sep_mod =  get(females_glm_sep), 
                        sex_tob_mod = get(females_glm_tob), #replace 06 with 2006 
                        mod_set_name = model_name)


}

coef_1 <- tidy (males_glm_sep)%>%
            mutate (Model_fam = model_name,
                    Model = "SEP",
                    Sex = 1,
                    CenYear = cohort,
                    Date_run = Sys.Date())
coef_2 <- tidy (males_glm_tob)%>%
            mutate (Model_fam = model_name,
                    Model = "Tob",
                    Sex = 1,
                    CenYear = cohort,
                    Date_run = Sys.Date())

coef_3 <- tidy (females_glm_sep)%>%
            mutate (Model_fam = model_name,
                    Model = "SEP",
                    Sex = 2,
                    CenYear = cohort,
                    Date_run = Sys.Date())

coef_4 <- tidy (females_glm_tob)%>%
            mutate (Model_fam = model_name,
                    Model = "Tob",
                    Sex = 2,
                    CenYear = cohort,
                    Date_run = Sys.Date())

fit_stats1 <- glance (males_glm_sep)%>%
                mutate (Model_fam = model_name,
                        Model = "SEP",
                        Sex = 1,
                        CenYear = cohort,
                        Date_run = Sys.Date())

fit_stats2 <- glance (males_glm_tob)%>%
                mutate (Model_fam = model_name,
                        Model = "Tob",
                        Sex = 1,
                        CenYear = cohort,
                        Date_run = Sys.Date())

fit_stats3 <- glance (females_glm_sep)%>%
                mutate (Model_fam = model_name,
                        Model = "SEP",
                        Sex = 2,
                        CenYear = cohort,
                        Date_run = Sys.Date())

fit_stats4 <- glance (females_glm_tob)%>%
                mutate (Model_fam = model_name,
                        Model = "Tob",
                        Sex = 2,
                        CenYear = cohort,
                        Date_run = Sys.Date())

fit_results <- bind_rows(fit_stats1, fit_stats2, fit_stats3, fit_stats4)
coeff_results <- bind_rows(coef_1, coef_2, coef_3, coef_4)
risk_results <- bind_rows(males_cw, females_cw)
return(list(coeff_results, fit_results, risk_results))

}








