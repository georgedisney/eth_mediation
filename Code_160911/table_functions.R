# Loading packages
library (dplyr)
library (haven)
library (sas7bdat)
library (readr)
library (broom)
library (reshape2)
library (boot)

### Programme for assembling coefficients into tidy table -----------------------------------------------

# Initial generic function to put one cohort into a tidy table 

coef_tab <- function (model_family,cohort){

coef <- get(paste0(model_family,"_", cohort))
coef <- coef[[1]]
#creating a look up table for levels of coefficient 
look_up <- coef %>%
            filter (Model == "Tob",
                    Sex == "1")

coef$term <- factor(coef$term, levels = dput(look_up$term), ordered = TRUE)
coef$Sex <- factor(coef$Sex)
coef$CenYear <- factor(coef$CenYear)

coef <- melt(coef)%>%
          filter(variable == "estimate")

coef <- dcast(coef, term ~ variable + CenYear + Sex + Model_fam + Model, value.var = "value")

coef
}

# Function to create combined table of coefficients, for all three cohorts, you can pick which model family ------------------ 

comb_coef_tab <- function (model_fam_results){
  
  
a <- coef_tab (model_fam_results, cohort = 1981)
b <-  coef_tab (model_fam_results, cohort = 1996)
c <-  coef_tab (model_fam_results, cohort = 2006)
  
one <- full_join(a,b, by = c("term"))
two <- full_join(one, c, by = c("term"))
}


### Programme for putting risks into tidy table -----------------------------------------------------------------------------

risk_tab <- function (model_family, cohort){
  
  risk <- get(paste0(model_family,"_", cohort))
  risk <- risk[[3]]
  
risk  
}


comb_risk_tab <- function (model_fam_results){
  
  a <- risk_tab (model_fam_results, cohort = 1981)
  b <-  risk_tab (model_fam_results, cohort = 1996)
  c <-  risk_tab (model_fam_results, cohort = 2006)
  
  #one <- left_join(a,b, by = c("Sex", "cross_ref", "model_set", "Age_group_broad", "Date_run"))
#two <- left_join(one, c,  by = c("Sex", "cross_ref", "model_set", "Age_group_broad", "Date_run"))
dat <- bind_rows(a,b,c)
  
  dat %>%
    select(Date_run, model_set, Sex, cross_ref, Age_group_broad, everything())
}
  

### Programme for comparing AIC Stats across two model types -----------------------------------------------------------------

aic_comp <- function (model_fam_a, model_fam_b, cohort){
  
  aic_a <- get(paste0(model_fam_a,"_", cohort))
  aic_a <- aic_a[[2]]
  
  aic_b <- get(paste0(model_fam_b,"_", cohort))
  aic_b <- aic_b[[2]]
  
  bind_rows (aic_a, aic_b)
}


comb_aic_comp <- function (model_fam_a_results, model_fam_b_results){
  
  
  a <- aic_comp(model_fam_a_results, model_fam_b_results, "1981") 
  b <- aic_comp(model_fam_a_results, model_fam_b_results, "1996")
  c <- aic_comp(model_fam_a_results, model_fam_b_results, "2006")
  bind_rows (a,b,c)
}
