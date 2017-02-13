library (readr)
library (dplyr)
library (tidyr)
options ("scipen" = 100)
# Reading bootstrap results in and setting that data up for putting into tables,  calculating stats etc. ----------------------

boot_results_master <- read_csv("G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Data Lab/Results_160911/boot_int_results.csv")
columns <- c("CenYear", "Sex", "cross_ref", "Age_group_broad")
boot_results_master[columns] <- lapply(boot_results_master[columns], as.factor)
boot_results_master$row <- 1:nrow(boot_results_master)




# Function to divide each risk by either 5 or three depending on which cohort it is from -------------------------

cohort_div <- function (x) {
  
  risk_div <- ifelse (boot_results_master[,"CenYear"] == "2006", x/5, x/3) 
  risk_div
}

# Function to multiple risks by one hundred thousand ------------------------------------------------------------------------

per_thou <- function (x){
  
  x*100000
  
}



# Binding the divided risk columns back to the other categorical variables ----------------------------------------

bind_cohort_div <- function(dat){
  y <- c("obs_risk", "pred_sep_risk", "cw_sep_risk", "pred_smo_risk",
         "cw_smo_risk", "never_risk")
  div_cols <- sapply(dat[y], cohort_div)
  
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

boot_results <- bind_cohort_div(dat = boot_results_master)
boot_res_spread <- eth_spread(boot_results)  


# Function to calculate average, conf intervals and whether obs or pred ---------------------------------------

stat_calc <- function (dat, stat, stat_lab, limit_lower, limit_upper, limit_lab, obs_or_pred){

boot_stats <- dat %>%
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
                       
                       pc_med_sep = (NIE_sep_abs/TE_abs)*100,
                       pred_pc_med_sep = (pred_NIE_sep_abs/pred_TE_abs)*100,
                       pc_med_smo = (NIE_smo_abs/TE_abs)*100,
                       pred_pc_med_smo = (pred_NIE_smo_abs/pred_TE_smo_abs)*100,
                       
                       delta_pc_med = pc_med_smo - pc_med_sep ,
                       pred_delta_pc_med = pred_pc_med_sep - pred_pc_med_smo,
                       
                       CDE_abs = `1_never_risk` - `0_never_risk`,
                       CDE_rel = `1_never_risk` / `0_never_risk`,
                       
                       pc_red_nev_m = (1 - (`1_never_risk`/`1_obs_risk`))*100,
                       pc_red_nev_e = (1 - (`0_never_risk`/`0_obs_risk`))*100,
                       pred_pc_red_nev_m = (1 - (`1_never_risk`/`1_pred_smo_risk`))*100,
                       pred_pc_red_nev_e = (1 - (`0_never_risk`/`0_pred_smo_risk`))*100,
                       
                       pc_erad = (1 - (CDE_abs/TE_abs))*100,
                       pred_pc_erad = (1 - (CDE_abs/pred_TE_abs))*100)


          boot_stats_average <<- boot_stats %>%
                                  group_by(CenYear, Sex, Age_group_broad)%>%
                                  summarise(`0_obs_risk` = stat(`0_obs_risk`),
                                    `0_pred_sep_risk` = stat(`0_pred_sep_risk`),
                                    `0_cw_sep_risk` = stat(`0_cw_sep_risk`),
                                    `0_pred_smo_risk` = stat(`0_pred_smo_risk`),
                                    `0_cw_smo_risk` = stat(`0_cw_smo_risk`),
                                    `0_never_risk` = stat(`0_never_risk`),
                                    
                                    `1_obs_risk` = stat(`1_obs_risk`),
                                    `1_pred_sep_risk` = stat(`1_pred_sep_risk`),
                                    `1_cw_sep_risk` = stat(`1_cw_sep_risk`),
                                    `1_pred_smo_risk` = stat(`1_pred_smo_risk`),
                                    `1_cw_smo_risk` = stat(`1_cw_smo_risk`),
                                    `1_never_risk` = stat(`1_never_risk`),
                                    
                                    `TE_abs` = stat(`TE_abs`), 
                                    `TE_rel` = stat(`TE_rel`),
                                    
                                    `pred_TE_abs` = stat(`pred_TE_abs`),
                                    `pred_TE_rel` = stat(`pred_TE_rel`),
                                    `pred_TE_smo_abs` = stat(`pred_TE_smo_abs`),
                                    `pred_TE_smo_rel` = stat(`pred_TE_smo_rel`),
                                    
                                    `NIE_sep_abs` = stat(`NIE_sep_abs`),
                                    `pred_NIE_sep_abs` = stat(`pred_NIE_sep_abs`),
                                    `NIE_smo_abs` = stat(`NIE_smo_abs`),  
                                    `pred_NIE_smo_abs` = stat(`pred_NIE_smo_abs`),
                                    
                                    `NIE_sep_rel` = stat(`NIE_sep_rel`),
                                    `pred_NIE_sep_rel` = stat(`pred_NIE_sep_rel`),
                                    `NIE_smo_rel` = stat(`NIE_smo_rel`),  
                                    `pred_NIE_smo_rel` = stat(`pred_NIE_smo_rel`),
                                    
                                    `NDE_sep_abs` = stat(`NDE_sep_abs`),
                                    `pred_NDE_sep_abs` = stat(`pred_NDE_sep_abs`),
                                    `NDE_smo_abs` = stat(`NDE_smo_abs`),
                                    `pred_NDE_smo_abs` = stat(`pred_NDE_smo_abs`), 
                                    
                                    `NDE_sep_rel` = stat(`NDE_sep_rel`),
                                    `pred_NDE_sep_rel` = stat(`pred_NDE_sep_rel`),
                                    `NDE_smo_rel` = stat(`NDE_smo_rel`),
                                    `pred_NDE_smo_rel` = stat(`pred_NDE_smo_rel`), 
                                    
                                    `pc_med_sep` = stat(`pc_med_sep`),
                                    `pred_pc_med_sep` = stat(`pred_pc_med_sep`),
                                    `pc_med_smo` = stat(`pc_med_smo`),
                                    `pred_pc_med_smo` = stat(`pred_pc_med_smo`),
                                    
                                    `delta_pc_med` = stat(`delta_pc_med`),
                                    `pred_delta_pc_med` = stat(`pred_delta_pc_med`),
                                    
                                    `CDE_abs` = stat(`CDE_abs`),
                                    `CDE_rel` = stat(`CDE_rel`),
                                    
                                    `pc_red_nev_m` = stat(`pc_red_nev_m`), 
                                    `pc_red_nev_e` = stat(`pc_red_nev_e`),
                                    `pred_pc_red_nev_m` = stat(`pred_pc_red_nev_m`), 
                                    `pred_pc_red_nev_e` = stat(`pred_pc_red_nev_e`),
                                    
                                    `pc_erad` = stat(`pc_erad`),
                                    `pred_pc_erad` = stat(`pred_pc_erad`)
          
                           
                           )%>%
                  mutate(stat = stat_lab,
                        date_run = Sys.Date()) 
  

boot_stats_uncer <- boot_stats %>%
              group_by(CenYear, Sex, Age_group_broad)%>%
    summarise(`0_obs_risk_low` = quantile(`0_obs_risk`, probs = limit_lower),
              `0_obs_risk_upp` = quantile(`0_obs_risk`, probs = limit_upper),
              `0_pred_sep_risk_low` = quantile(`0_pred_sep_risk`, probs = limit_lower),
              `0_pred_sep_risk_upp` = quantile(`0_pred_sep_risk`, probs = limit_upper),
              `0_cw_sep_risk_low` = quantile(`0_cw_sep_risk`, probs = limit_lower),
              `0_cw_sep_risk_upp` = quantile(`0_cw_sep_risk`, probs = limit_upper),
              `0_pred_smo_risk_low` = quantile(`0_pred_smo_risk`, probs = limit_lower),
              `0_pred_smo_risk_upp` = quantile(`0_pred_smo_risk`, probs = limit_upper),
              `0_cw_smo_risk_low` = quantile(`0_cw_smo_risk`, probs = limit_lower),
              `0_cw_smo_risk_upp` = quantile(`0_cw_smo_risk`, probs = limit_upper),
              `0_never_risk_low` = quantile(`0_never_risk`, probs = limit_lower),
              `0_never_risk_upp` = quantile(`0_never_risk`, probs = limit_upper),
              
              `1_obs_risk_low` = quantile(`1_obs_risk`, probs = limit_lower),
              `1_obs_risk_upp` = quantile(`1_obs_risk`, probs = limit_upper),
              `1_pred_sep_risk_low` = quantile(`1_pred_sep_risk`, probs = limit_lower),
              `1_pred_sep_risk_upp` = quantile(`1_pred_sep_risk`, probs = limit_upper),
              `1_cw_sep_risk_low` = quantile(`1_cw_sep_risk`, probs = limit_lower),
              `1_cw_sep_risk_upp` = quantile(`1_cw_sep_risk`, probs = limit_upper),
              `1_pred_smo_risk_low` = quantile(`1_pred_smo_risk`, probs = limit_lower),
              `1_pred_smo_risk_upp` = quantile(`1_pred_smo_risk`, probs = limit_upper),
              `1_cw_smo_risk_low` = quantile(`1_cw_smo_risk`, probs = limit_lower),
              `1_cw_smo_risk_upp` = quantile(`1_cw_smo_risk`, probs = limit_upper),
              `1_never_risk_low` = quantile(`1_never_risk`, probs = limit_lower),
              `1_never_risk_upp` = quantile(`1_never_risk`, probs = limit_upper),
              
              
              `TE_abs_low` = quantile(`TE_abs`, probs = limit_lower),
              `TE_abs_upp` = quantile(`TE_abs`, probs = limit_upper),
              `TE_rel_low` = quantile(`TE_rel`, probs = limit_lower),
              `TE_rel_upp` = quantile(`TE_rel`, probs = limit_upper),
            
              `pred_TE_abs_low` = quantile(`pred_TE_abs`, probs = limit_lower),
              `pred_TE_abs_upp` = quantile(`pred_TE_abs`, probs = limit_upper),
              `pred_TE_rel_low` = quantile(`pred_TE_rel`, probs = limit_lower),
              `pred_TE_rel_upp` = quantile(`pred_TE_rel`, probs = limit_upper),
              `pred_TE_smo_abs_low` = quantile(`pred_TE_smo_abs`, probs = limit_lower),
              `pred_TE_smo_abs_upp` = quantile(`pred_TE_smo_abs`, probs = limit_upper),
              `pred_TE_smo_rel_low` = quantile(`pred_TE_smo_rel`, probs = limit_lower),
              `pred_TE_smo_rel_upp` = quantile(`pred_TE_smo_rel`, probs = limit_upper),
              
              
              `NIE_sep_abs_low` = quantile(`NIE_sep_abs`, probs = limit_lower),
              `NIE_sep_abs_upp` = quantile(`NIE_sep_abs`, probs = limit_upper),
              `pred_NIE_sep_abs_low` = quantile(`pred_NIE_sep_abs`, probs = limit_lower),
              `pred_NIE_sep_abs_upp` = quantile(`pred_NIE_sep_abs`, probs = limit_upper),
              `NIE_smo_abs_low` = quantile(`NIE_smo_abs`, probs = limit_lower),
              `NIE_smo_abs_upp` = quantile(`NIE_smo_abs`, probs = limit_upper),
              `pred_NIE_smo_abs_low` = quantile(`pred_NIE_smo_abs`, probs = limit_lower),
              `pred_NIE_smo_abs_upp` = quantile(`pred_NIE_smo_abs`, probs = limit_upper),
              
              
              `NIE_sep_rel_low` = quantile(`NIE_sep_rel`, probs = limit_lower),
              `NIE_sep_rel_upp` = quantile(`NIE_sep_rel`, probs = limit_upper),
              `pred_NIE_sep_rel_low` = quantile(`pred_NIE_sep_rel`, probs = limit_lower),
              `pred_NIE_sep_rel_upp` = quantile(`pred_NIE_sep_rel`, probs = limit_upper),
              `NIE_smo_rel_low` = quantile(`NIE_smo_rel`, probs = limit_lower),
              `NIE_smo_rel_upp` = quantile(`NIE_smo_rel`, probs = limit_upper),
              `pred_NIE_smo_rel_low` = quantile(`pred_NIE_smo_rel`, probs = limit_lower),
              `pred_NIE_smo_rel_upp` = quantile(`pred_NIE_smo_rel`, probs = limit_upper),
              
              
              `NDE_sep_abs_low` = quantile(`NDE_sep_abs`, probs = limit_lower),
              `NDE_sep_abs_upp` = quantile(`NDE_sep_abs`, probs = limit_upper),
              `pred_NDE_sep_abs_low` = quantile(`pred_NDE_sep_abs`, probs = limit_lower),
              `pred_NDE_sep_abs_upp` = quantile(`pred_NDE_sep_abs`, probs = limit_upper),
              `NDE_smo_abs_low` = quantile(`NDE_smo_abs`, probs = limit_lower),
              `NDE_smo_abs_upp` = quantile(`NDE_smo_abs`, probs = limit_upper),
              `pred_NDE_smo_abs_low` = quantile(`pred_NDE_smo_abs`, probs = limit_lower), 
              `pred_NDE_smo_abs_upp` = quantile(`pred_NDE_smo_abs`, probs = limit_upper),
              
              
              `NDE_sep_rel_low` = quantile(`NDE_sep_rel`, probs = limit_lower),
              `NDE_sep_rel_upp` = quantile(`NDE_sep_rel`, probs = limit_upper),
              `pred_NDE_sep_rel_low` = quantile(`pred_NDE_sep_rel`, probs = limit_lower),
              `pred_NDE_sep_rel_upp` = quantile(`pred_NDE_sep_rel`, probs = limit_upper),
              `NDE_smo_rel_low` = quantile(`NDE_smo_rel`, probs = limit_lower),
              `NDE_smo_rel_upp` = quantile(`NDE_smo_rel`, probs = limit_upper),
              `pred_NDE_smo_rel_low` = quantile(`pred_NDE_smo_rel`, probs = limit_lower),
              `pred_NDE_smo_rel_upp` = quantile(`pred_NDE_smo_rel`, probs = limit_upper),
              
              
              `pc_med_sep_low` = quantile(`pc_med_sep`, probs = limit_lower),
              `pc_med_sep_upp` = quantile(`pc_med_sep`, probs = limit_upper),
              `pred_pc_med_sep_low` = quantile(`pred_pc_med_sep`, probs = limit_lower),
              `pred_pc_med_sep_upp` = quantile(`pred_pc_med_sep`, probs = limit_upper),
              `pc_med_smo_low` = quantile(`pc_med_smo`, probs = limit_lower),
              `pc_med_smo_upp` = quantile(`pc_med_smo`, probs = limit_upper),
              `pred_pc_med_smo_low` = quantile(`pred_pc_med_smo`, probs = limit_lower),
              `pred_pc_med_smo_upp` = quantile(`pred_pc_med_smo`, probs = limit_upper),
              
              
              `delta_pc_med_low` = quantile(`delta_pc_med`, probs = limit_lower),
              `delta_pc_med_upp` = quantile(`delta_pc_med`, probs = limit_upper),
              `pred_delta_pc_med_low` = quantile(`pred_delta_pc_med`, probs = limit_lower),
              `pred_delta_pc_med_upp` = quantile(`pred_delta_pc_med`, probs = limit_upper),
              
              
              `CDE_abs_low` = quantile(`CDE_abs`, probs = limit_lower),
              `CDE_abs_upp` = quantile(`CDE_abs`, probs = limit_upper),
              `CDE_rel_low` = quantile(`CDE_rel`, probs = limit_lower),
              `CDE_rel_upp` = quantile(`CDE_rel`, probs = limit_upper),
              
              `pc_red_nev_m_low` = quantile(`pc_red_nev_m`, probs = limit_lower),
              `pc_red_nev_m_upp` = quantile(`pc_red_nev_m`, probs = limit_upper),
              `pc_red_nev_e_low` = quantile(`pc_red_nev_e`, probs = limit_lower),
              `pc_red_nev_e_upp` = quantile(`pc_red_nev_e`, probs = limit_upper),
              `pred_pc_red_nev_m_low` = quantile(`pred_pc_red_nev_m`, probs = limit_lower),
              `pred_pc_red_nev_m_upp` = quantile(`pred_pc_red_nev_m`, probs = limit_upper),
              `pred_pc_red_nev_e_low` = quantile(`pred_pc_red_nev_e`, probs = limit_lower),
              `pred_pc_red_nev_e_upp` = quantile(`pred_pc_red_nev_e`, probs = limit_upper),
              
              `pc_erad_low` = quantile(`pc_erad`, probs = limit_lower),
              `pc_erad_upp` = quantile(`pc_erad`, probs = limit_upper),
              `pred_pc_erad_low` = quantile(`pred_pc_erad`, probs = limit_lower),
              `pred_pc_erad_upp` = quantile(`pred_pc_erad`, probs = limit_upper)
    )%>%
    mutate(limit = limit_lab,
           date_run = Sys.Date())%>%
            left_join(.,boot_stats_average)


per_thou_cols <- c("")

boot_stats_uncer <- boot_stats_uncer %>%
                     # mutate_each(funs(format(., scientific = FALSE)), -CenYear, -Sex, -Age_group_broad, -stat, -limit, -date_run )%>%
                      mutate_each(funs(formatC(round(.,3), format = "fg", digits = 3, drop0trailing = TRUE)), 
                                  -CenYear, -Sex, -Age_group_broad, -stat, -limit, -date_run)



######################
 
if (obs_or_pred == "obs"){
 comb_sex <<- boot_stats_uncer %>%
    transmute(`Age Group` = `Age_group_broad`,
              `Māori` = paste0(`1_obs_risk`, " ", "(", `1_obs_risk_low`, ", ", `1_obs_risk_upp`, ")"),
              `Euro` = paste0(`0_obs_risk`, " ", "(", `0_obs_risk_low`, ", ", `0_obs_risk_upp`, ")"),
              `Māori with Euro SEP (CW)` = paste0(`0_cw_sep_risk`, " ", "(", `0_cw_sep_risk_low`, ", ", `0_cw_sep_risk_upp`, ")"),
              `Māori with Euro SEP + Tob (CW)` = paste0(`0_cw_smo_risk`, " ", "(", `0_cw_smo_risk_low`, ", ", `0_cw_smo_risk_upp`, ")"),
              `Māori Never Smoking (counterfactual)` = paste0(`1_never_risk`, " ", "(", `1_never_risk_low`, ", ", `1_never_risk_upp`, ")"),
              `Euro Never Smoking (counterfactual)` = paste0(`0_never_risk`, " ", "(", `0_never_risk_low`, ", ", `0_never_risk_upp`, ")"),
              `Total Effect` = paste0(`TE_abs`, " ", "(", `TE_abs_low`, ", ", `TE_abs_upp`, ")"),
              `NDE (SEP mediation)` = paste0(`NDE_sep_abs`, " ", "(", `NDE_sep_abs_low`, ", ", `NDE_sep_abs_upp`, ")"),
              `NDE (SEP + Tob mediation)` = paste0(`NDE_smo_abs`, " ", "(", `NDE_smo_abs_low`, ", ", `NDE_smo_abs_upp`, ")"),
              `NIE (SEP mediation)` = paste0(`NIE_sep_abs`, " ", "(", `NIE_sep_abs_low`, ", ", `NIE_sep_abs_upp`, ")"),
              `NIE (SEP + Tob mediation)` = paste0(`NIE_smo_abs`, " ", "(", `NIE_smo_abs_low`, ", ", `NIE_smo_abs_upp`, ")"),
              `CDE (Tobacco free counterfactual)` = paste0(`CDE_abs`, " ", "(", `CDE_abs_low`, ", ", `CDE_abs_upp`, ")"),
              `% Mediated (SEP)` = paste0(`pc_med_sep`, " ", "(", `pc_med_sep_low`, ", ", `pc_med_sep_upp`, ")"),
              `% Mediated (SEP + Tob)` = paste0(`pc_med_smo`, " ", "(", `pc_med_smo_low`, ", ", `pc_med_smo_upp`, ")"),
              `Mediation Change (SEP to SEP + Tob)` = paste0(`delta_pc_med`, " ", "(", `delta_pc_med_low`, ", ", `delta_pc_med_upp`, ")"),
              `% Reduction in Māori risk` = paste0(`pc_red_nev_m`, " ", "(", `pc_red_nev_m_low`, ", ", `pc_red_nev_m_upp`, ")"),
              `% Reduction in Euro risk` = paste0(`pc_red_nev_e`, " ", "(", `pc_red_nev_e_low`, ", ", `pc_red_nev_e_upp`, ")"),
              `% Eradicated (Tobacco free counterfactual)` = paste0(`pc_erad`, " ", "(", `pc_erad_low`, ", ", `pc_erad_upp`, ")"),
              `Rel Total Effect` = paste0(`TE_rel`, " ", "(", `TE_rel_low`, ", ", `TE_rel_upp`, ")"),
              `Rel NDE (SEP mediation)` = paste0(`NDE_sep_rel`, " ", "(", `NDE_sep_rel_low`, ", ", `NDE_sep_rel_upp`, ")"),
              `Rel NDE (SEP + Tob mediation)` = paste0(`NDE_smo_rel`, " ", "(", `NDE_smo_rel_low`, ", ", `NDE_smo_rel_upp`, ")"),
              `Rel NIE (SEP mediation)` = paste0(`NIE_sep_rel`, " ", "(", `NIE_sep_rel_low`, ", ", `NIE_sep_rel_upp`, ")"),
              `Rel NIE (SEP + Tob mediation)` = paste0(`NIE_smo_rel`, " ", "(", `NIE_smo_rel_low`, ", ", `NIE_smo_rel_upp`, ")"),
              `Rel CDE (Tobacco free counterfactual)` = paste0(`CDE_rel`, " ", "(", `CDE_rel_low`, ", ", `CDE_rel_upp`, ")")
              )
 
 
 males <- comb_sex %>%
            filter(Sex == "1")
 
 males$row <- 1:nrow(males)
 
 females <- comb_sex %>%
              filter (Sex == "2")
 
 females$row <- 1:nrow(females)
 
 
 males <- males %>% 
            gather(key, stat, `Māori`:`Rel CDE (Tobacco free counterfactual)`) %>%
            spread(CenYear, value = stat, drop = TRUE)
 
 
 females <- females %>% 
   gather(key, stat, `Māori`:`Rel CDE (Tobacco free counterfactual)`) %>%
   spread(CenYear, value = stat, drop = TRUE)
 
 males$key <- factor(males$key, levels = c("Māori", "Euro", "Māori with Euro SEP (CW)", "Māori with Euro SEP + Tob (CW)",
                                           "Māori Never Smoking (counterfactual)", "Euro Never Smoking (counterfactual)",
                                           "Total Effect", "NDE (SEP mediation)", "NDE (SEP + Tob mediation)", 
                                           "NIE (SEP mediation)", "NIE (SEP + Tob mediation)", 
                                           "CDE (Tobacco free counterfactual)",
                                           "% Mediated (SEP)", "% Mediated (SEP + Tob)", "% Mediated (SEP + Tob)", 
                                           "Mediation Change (SEP to SEP + Tob)", "% Reduction in Māori risk",
                                           "% Reduction in Euro risk", 
                                           "% Eradicated (Tobacco free counterfactual)",
                                           "Rel Total Effect", "Rel NDE (SEP mediation)", "Rel NDE (SEP + Tob mediation)", 
                                           "Rel NIE (SEP mediation)", "Rel NIE (SEP + Tob mediation)", "Rel CDE (Tobacco free counterfactual)"),
                     ordered = TRUE)

 females$key <- factor(females$key, levels = c("Māori", "Euro", "Māori with Euro SEP (CW)", "Māori with Euro SEP + Tob (CW)",
                                               "Māori Never Smoking (counterfactual)", "Euro Never Smoking (counterfactual)",
                                               "Total Effect", "NDE (SEP mediation)", "NDE (SEP + Tob mediation)", 
                                               "NIE (SEP mediation)", "NIE (SEP + Tob mediation)", 
                                               "CDE (Tobacco free counterfactual)",
                                               "% Mediated (SEP)", "% Mediated (SEP + Tob)", "% Mediated (SEP + Tob)", 
                                               "Mediation Change (SEP to SEP + Tob)", "% Reduction in Māori risk",
                                               "% Reduction in Euro risk", 
                                               "% Eradicated (Tobacco free counterfactual)",
                                               "Rel Total Effect", "Rel NDE (SEP mediation)", "Rel NDE (SEP + Tob mediation)", 
                                               "Rel NIE (SEP mediation)", "Rel NIE (SEP + Tob mediation)", "Rel CDE (Tobacco free counterfactual)"),
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
results$table <- print(obs_or_pred)
}

if (obs_or_pred == "pred"){
  
  comb_sex <- boot_stats_uncer %>%
    transmute(`Age Group` = `Age_group_broad`,
              `Māori` = paste0(`1_pred_sep_risk`, " ", "(", `1_pred_sep_risk_low`, ", ", `1_pred_sep_risk_upp`, ")"),
              `Euro` = paste0(`0_pred_sep_risk`, " ", "(", `0_pred_sep_risk_low`, ", ", `0_pred_sep_risk_upp`, ")"),
              `Māori with Euro SEP (CW)` = paste0(`0_cw_sep_risk`, " ", "(", `0_cw_sep_risk_low`, ", ", `0_cw_sep_risk_upp`, ")"),
              `Māori with Euro SEP + Tob (CW)` = paste0(`0_cw_smo_risk`, " ", "(", `0_cw_smo_risk_low`, ", ", `0_cw_smo_risk_upp`, ")"),
              `Māori Never Smoking (counterfactual)` = paste0(`1_never_risk`, " ", "(", `1_never_risk_low`, ", ", `1_never_risk_upp`, ")"),
              `Euro Never Smoking (counterfactual)` = paste0(`0_never_risk`, " ", "(", `0_never_risk_low`, ", ", `0_never_risk_upp`, ")"),
              `Total Effect` = paste0(`pred_TE_abs`, " ", "(", `pred_TE_abs_low`, ", ", `pred_TE_abs_upp`, ")"),
              `NDE (SEP mediation)` = paste0(`pred_NDE_sep_abs`, " ", "(", `pred_NDE_sep_abs_low`, ", ", `pred_NDE_sep_abs_upp`, ")"),
              `NDE (SEP + Tob mediation)` = paste0(`pred_NDE_smo_abs`, " ", "(", `pred_NDE_smo_abs_low`, ", ", `pred_NDE_smo_abs_upp`, ")"),
              `NIE (SEP mediation)` = paste0(`pred_NIE_sep_abs`, " ", "(", `pred_NIE_sep_abs_low`, ", ", `pred_NIE_sep_abs_upp`, ")"),
              `NIE (SEP + Tob mediation)` = paste0(`pred_NIE_smo_abs`, " ", "(", `pred_NIE_smo_abs_low`, ", ", `pred_NIE_smo_abs_upp`, ")"),
              `CDE (Tobacco free counterfactual)` = paste0(`CDE_abs`, " ", "(", `CDE_abs_low`, ", ", `CDE_abs_upp`, ")"),
              `% Mediated (SEP)` = paste0(`pred_pc_med_sep`, " ", "(", `pred_pc_med_sep_low`, ", ", `pred_pc_med_sep_upp`, ")"),
              `% Mediated (SEP + Tob)` = paste0(`pred_pc_med_smo`, " ", "(", `pred_pc_med_smo_low`, ", ", `pred_pc_med_smo_upp`, ")"),
              `Mediation Change (SEP to SEP + Tob)` = paste0(`pred_delta_pc_med`, " ", "(", `pred_delta_pc_med_low`, ", ", `pred_delta_pc_med_upp`, ")"),
              `% Reduction in Māori risk` = paste0(`pred_pc_red_nev_m`, " ", "(", `pred_pc_red_nev_m_low`, ", ", `pred_pc_red_nev_m_upp`, ")"),
              `% Reduction in Euro risk` = paste0(`pred_pc_red_nev_e`, " ", "(", `pred_pc_red_nev_e_low`, ", ", `pred_pc_red_nev_e_upp`, ")"),
              `% Eradicated (Tobacco free counterfactual)` = paste0(`pred_pc_erad`, " ", "(", `pred_pc_erad_low`, ", ", `pred_pc_erad_upp`, ")"),
              `Rel Total Effect` = paste0(`pred_TE_rel`, " ", "(", `pred_TE_rel_low`, ", ", `pred_TE_rel_upp`, ")"),
              `Rel NDE (SEP mediation)` = paste0(`pred_NDE_sep_rel`, " ", "(", `pred_NDE_sep_rel_low`, ", ", `pred_NDE_sep_rel_upp`, ")"),
              `Rel NDE (SEP + Tob mediation)` = paste0(`pred_NDE_smo_rel`, " ", "(", `pred_NDE_smo_rel_low`, ", ", `pred_NDE_smo_rel_upp`, ")"),
              `Rel NIE (SEP mediation)` = paste0(`pred_NIE_sep_rel`, " ", "(", `pred_NIE_sep_rel_low`, ", ", `pred_NIE_sep_rel_upp`, ")"), 
              `Rel NIE (SEP + Tob mediation)` = paste0(`pred_NIE_smo_rel`, " ", "(", `pred_NIE_smo_rel_low`, ", ", `pred_NIE_smo_rel_upp`, ")"),
              `Rel CDE (Tobacco free counterfactual)` = paste0(`CDE_rel`, " ", "(", `CDE_rel_low`, ", ", `CDE_rel_upp`, ")")
    )
  
  
  males <- comb_sex %>%
    filter(Sex == "1")
  
  males$row <- 1:nrow(males)
  
  females <- comb_sex %>%
    filter (Sex == "2")
  
  females$row <- 1:nrow(females)
  
  males$`Age Group` <- factor(males$`Age Group`, levels = c("2574", "2544", "4564", "6574"), 
                                                 labels = c("25-74", "25-44", "45-64", "65-74"), ordered = TRUE)
  
  females$`Age Group` <- factor(females$`Age Group`, levels = c("2574", "2544", "4564", "6574"),
                                                     labels = c("25-74", "25-44", "45-64", "65-74"), ordered = TRUE)
  
    
  males <- males %>% 
    gather(key, stat, `Māori`:`Rel CDE (Tobacco free counterfactual)`) %>%
    spread(CenYear, value = stat, drop = TRUE)
  
  
  females <- females %>% 
    gather(key, stat, `Māori`:`Rel CDE (Tobacco free counterfactual)`) %>%
    spread(CenYear, value = stat, drop = TRUE)
  
  males$key <- factor(males$key, levels = c("Māori", "Euro", "Māori with Euro SEP (CW)", "Māori with Euro SEP + Tob (CW)",
                                            "Māori Never Smoking (counterfactual)", "Euro Never Smoking (counterfactual)",
                                            "Total Effect", "NDE (SEP mediation)", "NDE (SEP + Tob mediation)", 
                                            "NIE (SEP mediation)", "NIE (SEP + Tob mediation)", 
                                            "CDE (Tobacco free counterfactual)",
                                            "% Mediated (SEP)", "% Mediated (SEP + Tob)", "% Mediated (SEP + Tob)", 
                                            "Mediation Change (SEP to SEP + Tob)", "% Reduction in Māori risk",
                                            "% Reduction in Euro risk", 
                                            "% Eradicated (Tobacco free counterfactual)",
                                            "Rel Total Effect", "Rel NDE (SEP mediation)", "Rel NDE (SEP + Tob mediation)", 
                                            "Rel NIE (SEP mediation)", "Rel NIE (SEP + Tob mediation)", "Rel CDE (Tobacco free counterfactual)"),
                      ordered = TRUE)
  
  females$key <- factor(females$key, levels = c("Māori", "Euro", "Māori with Euro SEP (CW)", "Māori with Euro SEP + Tob (CW)",
                                                "Māori Never Smoking (counterfactual)", "Euro Never Smoking (counterfactual)",
                                                "Total Effect", "NDE (SEP mediation)", "NDE (SEP + Tob mediation)", 
                                                "NIE (SEP mediation)", "NIE (SEP + Tob mediation)", 
                                                "CDE (Tobacco free counterfactual)",
                                                "% Mediated (SEP)", "% Mediated (SEP + Tob)", "% Mediated (SEP + Tob)", 
                                                "Mediation Change (SEP to SEP + Tob)", "% Reduction in Māori risk",
                                                "% Reduction in Euro risk", 
                                                "% Eradicated (Tobacco free counterfactual)",
                                                "Rel Total Effect", "Rel NDE (SEP mediation)", "Rel NDE (SEP + Tob mediation)", 
                                                "Rel NIE (SEP mediation)", "Rel NIE (SEP + Tob mediation)", "Rel CDE (Tobacco free counterfactual)"),
                        ordered = TRUE)
  
  
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
    arrange (`Age Group`:`key`)
  
  females <- full_join(females_1981, females_1996)%>%
    full_join(.,females_2006)%>%
    arrange (`Age Group`:key)%>%
    select (-`Age Group`, -key)
  
  results <- bind_cols(males, females)
  results$table <- print (obs_or_pred)
}


results 
  

}







obs <- stat_calc (dat = boot_res_spread, stat = median, stat_lab = "median", 
                  limit_lower = 0.025, limit_upper = 0.975, limit_lab = "95", obs_or_pred = "obs")   

pred <- stat_calc (dat = boot_res_spread, stat = median, stat_lab = "median", 
                   limit_lower = 0.025, limit_upper = 0.975, limit_lab = "95", obs_or_pred = "pred")   


sd_obs <- stat_calc (dat = boot_res_spread, stat = sd, stat_lab = "median", 
                  limit_lower = 0.025, limit_upper = 0.975, limit_lab = "95", obs_or_pred = "obs")   



## Standard deviation stuff - temporary ---------------------------------------------------------------------


conf_int <- function (x){
  
  x + (1.96*x)
  
}


test <- sapply(boot_stats_average[c("0_obs_risk", "0_pred_sep_risk", 
                                    "0_cw_sep_risk", "0_pred_smo_risk", "0_cw_smo_risk", "0_never_risk", 
                                    "1_obs_risk", "1_pred_sep_risk", "1_cw_sep_risk", "1_pred_smo_risk", 
                                    "1_cw_smo_risk", "1_never_risk", "TE_abs", "TE_rel", "pred_TE_abs", 
                                    "pred_TE_rel", "pred_TE_smo_abs", "pred_TE_smo_rel", "NIE_sep_abs", 
                                    "pred_NIE_sep_abs", "NIE_smo_abs", "pred_NIE_smo_abs", "NIE_sep_rel", 
                                    "pred_NIE_sep_rel", "NIE_smo_rel", "pred_NIE_smo_rel", "NDE_sep_abs", 
                                    "pred_NDE_sep_abs", "NDE_smo_abs", "pred_NDE_smo_abs", "NDE_sep_rel", 
                                    "pred_NDE_sep_rel", "NDE_smo_rel", "pred_NDE_smo_rel", "pc_med_sep", 
                                    "pred_pc_med_sep", "pc_med_smo", "pred_pc_med_smo", "delta_pc_med", 
                                    "pred_delta_pc_med", "CDE_abs", "CDE_rel", "pc_erad", "pred_pc_erad")], conf_int)


c("0_obs_risk", "0_pred_sep_risk", 
  "0_cw_sep_risk", "0_pred_smo_risk", "0_cw_smo_risk", "0_never_risk", 
  "1_obs_risk", "1_pred_sep_risk", "1_cw_sep_risk", "1_pred_smo_risk", 
  "1_cw_smo_risk", "1_never_risk", "TE_abs", "TE_rel", "pred_TE_abs", 
  "pred_TE_rel", "pred_TE_smo_abs", "pred_TE_smo_rel", "NIE_sep_abs", 
  "pred_NIE_sep_abs", "NIE_smo_abs", "pred_NIE_smo_abs", "NIE_sep_rel", 
  "pred_NIE_sep_rel", "NIE_smo_rel", "pred_NIE_smo_rel", "NDE_sep_abs", 
  "pred_NDE_sep_abs", "NDE_smo_abs", "pred_NDE_smo_abs", "NDE_sep_rel", 
  "pred_NDE_sep_rel", "NDE_smo_rel", "pred_NDE_smo_rel", "pc_med_sep", 
  "pred_pc_med_sep", "pc_med_smo", "pred_pc_med_smo", "delta_pc_med", 
  "pred_delta_pc_med", "CDE_abs", "CDE_rel", "pc_erad", "pred_pc_erad")



write_csv(obs, path = "G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Eth_Mediation/mediation_obs.csv")
write_csv(pred, path = "G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Eth_Mediation/mediation_pred.csv")
#write_csv(boot_stats_uncer, path = "G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Eth_Mediation/boot_stats_uncer.csv")
