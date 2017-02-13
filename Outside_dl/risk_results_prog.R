


# Reading point estimates in and setting data up etc ------------------------------------------------------

risk_results_master <- read_csv("G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Data Lab/Results_160911/p_int_risks.csv")
columns <- c("CenYear", "Sex", "cross_ref", "Age_group_broad")
risk_results_master[columns] <- lapply(risk_results_master[columns], as.factor)
risk_results_master$row <- 1:nrow(risk_results_master)




# Function to divide each risk by either 5 or three depending on which cohort it is from -------------------------

risk_cohort_div <- function (x) {
  
  risk_div <- ifelse (risk_results_master[,"CenYear"] == "2006", x/5, x/3) 
  risk_div
}

# Function to multiple risks by one hundred thousand ------------------------------------------------------------------------

per_thou <- function (x){
  
  x*100000
  
}


# Binding the divided risk columns back to the other categorical variables ----------------------------------------

risk_bind_cohort_div <- function(dat){
  y <- c("obs_risk", "pred_sep_risk", "cw_sep_risk", "pred_smo_risk",
         "cw_smo_risk", "never_risk")
  div_cols <- sapply(dat[y], risk_cohort_div)
  div_cols <- as.data.frame(div_cols)
  thou_cols <- sapply(div_cols[y], per_thou)
  thou_cols <- as.data.frame(thou_cols)
  index <- dat %>%
    select(CenYear, Sex, cross_ref, Age_group_broad, row)
  bind_cols(index, thou_cols)
}


# Function to spread the columns into ethnic specific risks (this is better for the calculations below) ----------------

eth_spread <- function (dat){
  
  dat1 <- dat %>%
    gather(key, value, obs_risk:never_risk)%>%
    unite(key2, cross_ref, key, sep = "_")%>%
    spread(key2, value)  
  
  dat2 <- dat1%>%
    select(CenYear:`0_pred_smo_risk`)%>%
    na.omit(.)
  
  dat3 <- dat1 %>%
    select(`1_cw_sep_risk`:`1_pred_smo_risk`)%>%
    na.omit(.)
  
  dat4 <- bind_cols(dat2, dat3)
  dat4
}


# Code to execute functions above --------------------------------------------------------------------------

risk_results <- risk_bind_cohort_div(dat = risk_results_master)
risk_res_spread <- eth_spread(risk_results)  


# Code to calculate stats of interest (NDE, CDE etc) ----------------------------------------------------------------------

risk_stat_table <- function (dat){

risk_stats <- dat %>%
  mutate(TE_abs = `1_obs_risk` - `0_obs_risk`, 
         TE_rel = `1_obs_risk` / `0_obs_risk`,
         
         pred_TE_abs = `1_pred_sep_risk` - `0_pred_sep_risk`,
         pred_TE_rel = `1_pred_sep_risk` / `0_pred_sep_risk`,
         pred_TE_smo_abs = `1_pred_smo_risk` - `0_pred_smo_risk`,
         pred_TE_smo_rel = `1_pred_smo_risk` / `0_pred_smo_risk`,
         
         NIE_sep_abs = `1_obs_risk` - `0_cw_sep_risk`,
         pred_NIE_sep_abs = `1_pred_sep_risk` - `0_cw_sep_risk`,
         NIE_smo_abs = `1_obs_risk` - `0_cw_smo_risk`,  
         pred_NIE_smo_abs = `1_pred_smo_risk` - `0_cw_smo_risk`,
         
         NIE_sep_rel = `1_obs_risk` / `0_cw_sep_risk`,
         pred_NIE_sep_rel = `1_pred_sep_risk` / `0_cw_sep_risk`,
         NIE_smo_rel = `1_obs_risk` / `0_cw_smo_risk`,  
         pred_NIE_smo_rel = `1_pred_smo_risk` / `0_cw_smo_risk`,
         
         NDE_sep_abs = `0_cw_sep_risk` - `0_obs_risk`,
         pred_NDE_sep_abs = `0_cw_sep_risk` - `0_pred_sep_risk`,
         NDE_smo_abs = `0_cw_smo_risk` - `0_obs_risk`,
         pred_NDE_smo_abs = `0_cw_smo_risk` - `0_pred_smo_risk`, 
         
         NDE_sep_rel = `0_cw_sep_risk` / `0_obs_risk`,
         pred_NDE_sep_rel = `0_cw_sep_risk` / `0_pred_sep_risk`,
         NDE_smo_rel = `0_cw_smo_risk` / `0_obs_risk`,
         pred_NDE_smo_rel = `0_cw_smo_risk` / `0_pred_smo_risk`, 
         
         pc_med_sep = NIE_sep_abs/TE_abs,
         pred_pc_med_sep = pred_NIE_sep_abs/pred_TE_abs,
         pc_med_smo = NIE_smo_abs/TE_abs,
         pred_pc_med_smo = pred_NIE_smo_abs/pred_TE_smo_abs,
         
         delta_pc_med = pc_med_sep - pc_med_smo,
         pred_delta_pc_med = pred_pc_med_sep - pred_pc_med_smo,
         
         CDE_abs = `1_never_risk` - `0_never_risk`,
         CDE_rel = `1_never_risk` / `0_never_risk`,
         
         pc_erad = 1 - (CDE_abs/TE_abs),
         pred_pc_erad = 1 - (CDE_abs/pred_TE_abs))%>%
  mutate_each(funs(formatC(round(.,3), format = "g", digits = 3)), -CenYear, -Sex, -Age_group_broad, -row)%>%
    select(-row)      



comb_sex <- risk_stats %>%
  select(CenYear,
         Sex,
          `Age Group` = `Age_group_broad`,
          `Māori Observed Risk` = `1_obs_risk`, 
          `Euro Observed Risk` = `0_obs_risk`, 
          `Māori with Euro SEP` = `0_cw_sep_risk`,
          `Māori with Euro SEP + Tob` = `0_cw_smo_risk`,
          `Māori Never Smoking` = `1_never_risk`,
          `Euro Never Smoking` = `0_never_risk`,
          `Total Effect` = `TE_abs`,
          `NDE (SEP)` = `NDE_sep_abs`,
          `NDE (SEP + Tob)` = `NDE_smo_abs`, 
          `NIE (SEP)` = `NIE_sep_abs`, 
          `NIE (SEP + Tob)` = `NIE_smo_abs`,
          `CDE (Tob free)` = `CDE_abs`, 
          `% Mediated (SEP)` = `pc_med_sep`,
          `% Mediated (SEP + Tob)` = `pc_med_smo`, 
          `Mediation Change (incl Tob)` = `delta_pc_med`, 
          `% Eradicated` = `pc_erad`, 
          `Rel Total Effect` = `TE_rel`,
          `Rel NDE (SEP)` = `NDE_sep_rel`,
          `Rel NDE (SEP + Tob)` = `NDE_smo_rel`,
          `Rel NIE (SEP)` = `NIE_sep_rel`,
          `Rel NIE (SEP + Tob)` = `NIE_smo_rel`,
          `Rel CDE (Tob free)` = `CDE_rel`)


males <- comb_sex %>%
  filter(Sex == "1")

males$row <- 1:nrow(males)

females <- comb_sex %>%
  filter (Sex == "2")

females$row <- 1:nrow(females)


males <- males %>% 
  gather(key, stat, `Māori Observed Risk`:`Rel CDE (Tob free)`) %>%
  spread(CenYear, value = stat, drop = TRUE)


females <- females %>% 
  gather(key, stat, `Māori Observed Risk`:`Rel CDE (Tob free)`) %>%
  spread(CenYear, value = stat, drop = TRUE)

males$key <- factor(males$key, levels = c("Māori Observed Risk", "Euro Observed Risk", "Māori with Euro SEP", "Māori with Euro SEP + Tob",
                                          "Māori Never Smoking", "Euro Never Smoking",
                                          "Total Effect", "NDE (SEP)", "NDE (SEP + Tob)", "NIE (SEP)", "NIE (SEP + Tob)", "CDE (Tob free)",
                                          "% Mediated (SEP)", "% Mediated (SEP + Tob)", "% Mediated (SEP + Tob)", "Mediation Change (incl Tob)",
                                          "% Eradicated",
                                          "Rel Total Effect", "Rel NDE (SEP)", "Rel NDE (SEP + Tob)", "Rel NIE (SEP)", "Rel NIE (SEP + Tob)", "Rel CDE (Tob free)"),
                    ordered = TRUE)

females$key <- factor(females$key, levels = c("Māori Observed Risk", "Euro Observed Risk", "Māori with Euro SEP", "Māori with Euro SEP + Tob",
                                              "Māori Never Smoking", "Euro Never Smoking",
                                              "Total Effect", "NDE (SEP)", "NDE (SEP + Tob)", "NIE (SEP)", "NIE (SEP + Tob)", "CDE (Tob free)",
                                              "% Mediated (SEP)", "% Mediated (SEP + Tob)", "% Mediated (SEP + Tob)", "Mediation Change (incl Tob)",
                                              "% Eradicated",
                                              "Rel Total Effect", "Rel NDE (SEP)", "Rel NDE (SEP + Tob)", "Rel NIE (SEP)", "Rel NIE (SEP + Tob)", "Rel CDE (Tob free)"),
                      ordered = TRUE)

males$`Age Group` <- factor(males$`Age Group`, levels = c("2574", "2544", "4564", "6574"),
                                               labels = c("25-74", "25-44", "45-64", "65-74"), ordered = TRUE)

females$`Age Group` <- factor(females$`Age Group`, levels = c("2574", "2544", "4564", "6574"),
                                                   labels = c("25-74", "25-44", "45-64", "65-74"), ordered = TRUE)


males_1981 <- males %>%
  select (-row, -`1996`, -`2006`)%>%
  filter (`1981` != "NA")

males_1996 <- males %>%
  select (-row, -`1981`, -`2006`)%>%
  filter (`1996` != "NA")


males_2006 <- males %>%
  select (-row, -`1981`, -`1996`)%>%
  filter (`2006` != "NA")


females_1981 <- females %>%
  select (-row, -`1996`, -`2006`)%>%
  filter (`1981` != "NA")

females_1996 <- females %>%
  select (-row, -`1981`, -`2006`)%>%
  filter (`1996` != "NA")


females_2006 <- females %>%
  select (-row, -`1981`, -`1996`)%>%
  filter (`2006` != "NA")


males <- full_join(males_1981, males_1996)%>%
  full_join(.,males_2006)%>%
  arrange (`Age Group`:key)

females <- full_join(females_1981, females_1996)%>%
  full_join(.,females_2006)%>%
  arrange (`Age Group`:key)%>%
  select (-`Age Group`, -key)

results <- bind_cols(males, females)
results

}  


point_est_results <- risk_stat_table(risk_res_spread)  

write_csv(point_est_results, path = "G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Eth_Mediation/point_est_results.csv")


