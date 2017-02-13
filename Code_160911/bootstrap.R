library(parallel)
# Code for making cluster - not sure if this is needed
no_cores <- detectCores()

cl <- makeCluster(5)

stopCluster(cl)


# Bootstrap -----------------------------------

boot_cw <- function (sex_cohort, indices, glm_sep_spec, glm_tob_spec, mod_set_name) {
  
  
  sep_glm <- glm(as.formula(glm_sep_spec),
                  family = binomial(link = "logit"), data = sex_cohort [indices, ], weights = W_AgEthAdj)
  
  tob_glm <- glm(as.formula(glm_tob_spec),
                 family = binomial(link = "logit"), data = sex_cohort[indices, ], weights = W_AgEthAdj)
  
  cw_data <- sex_cohort[indices, ]
  cw_data$cross_ref <- cw_data$TotMaori
  cw_data$TotMaori <- ifelse (cw_data$TotMaori == "1",1,1)
  
  smo_data <- sex_cohort[indices, ]
  smo_data$SmkStat <- ifelse(smo_data$SmkStat == "3"|smo_data$SmkStat == "2", 1, 1)
  smo_data$SmkStat <- as.factor(smo_data$SmkStat)
  
  sep_pred <- predict.glm(sep_glm, type = "response")
  smo_pred <- predict.glm(tob_glm, type = "response")
  
  cw_pred <- predict.glm(sep_glm, newdata = cw_data, type = "response")
  cw_pred_smo <- predict.glm(tob_glm, newdata = cw_data, type = "response")
  never_pred <- predict.glm(tob_glm, newdata = smo_data, type = "response")
  
  cw_data$sep_pred <- sep_pred
  cw_data$smo_pred <- smo_pred
  cw_data$cw_pred <- cw_pred
  cw_data$cw_pred_smo <- cw_pred_smo
  cw_data$never_pred <- never_pred
  cw_data$mort <- ifelse(cw_data$Link == "1", 1, 0)  
  
  risk <- cw_data %>%
            mutate(comb_weights = weights*W_AgEthAdj)%>%
            group_by(CenYear, Sex, cross_ref)%>%
            summarise (obs_risk = weighted.mean(mort, w = comb_weights),
                       pred_sep_risk = weighted.mean(sep_pred, w = weights),
                       cw_sep_risk = weighted.mean(cw_pred, w = weights),
                       pred_smo_risk = weighted.mean(smo_pred, w = weights),
                       cw_smo_risk = weighted.mean(cw_pred_smo, w = weights),
                       never_risk = weighted.mean (never_pred, w = weights))%>%
            mutate(Age_group_broad = "2574",
                   model_set = mod_set_name,
                   Date_run = Sys.Date())
  
  
  risk_age <- cw_data %>%
                mutate(comb_weights = weights*W_AgEthAdj)%>%
                group_by(CenYear, Sex, cross_ref, Age_group_broad)%>% #check age group variable
                summarise (obs_risk = weighted.mean(mort, w = comb_weights),
                           pred_sep_risk = weighted.mean(sep_pred, w = weights),
                           cw_sep_risk = weighted.mean(cw_pred, w = weights),
                           pred_smo_risk = weighted.mean(smo_pred, w = weights),
                           cw_smo_risk = weighted.mean(cw_pred_smo, w = weights),
                           never_risk = weighted.mean (never_pred, w = weights))%>%
                mutate(model_set = mod_set_name,
                       Date_run = Sys.Date())%>%            
    bind_rows (.,risk)
  
  results <- as.matrix(risk_age)
  return(results)
  
}











