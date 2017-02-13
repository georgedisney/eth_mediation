### Code to run mediation analysis --------------------------------------------------

### Reading in mediation and results functions --------------------------------------

source ("/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mort_prog.R")
source ("/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/table_functions.R")


### Prior interactions results ------------------------------------------------------
# Risks, model fit and coefficients
p_int_1981 <- run_mediation(cohort = 1981, glm_sep_title = NULL, glm_tob_title = NULL,
                            glm_sep_spec = fam_p_int_sep, glm_tob_spec = fam_p_int_tob, model_name = "Prior Interaction")

p_int_1996 <- run_mediation(cohort = 1996, glm_sep_title = NULL, glm_tob_title = NULL,
                            glm_sep_spec = fam_p_int_sep, glm_tob_spec = fam_p_int_tob, model_name = "Prior Interaction")

p_int_2006 <- run_mediation(cohort = 2006, glm_sep_title = NULL, glm_tob_title = NULL,
                            glm_sep_spec = fam_p_int_sep, glm_tob_spec = fam_p_int_tob, model_name = "Prior Interaction")


save(p_int_1981, file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/p_int_1981.R")
save(p_int_1996, file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/p_int_1996.R")
save(p_int_2006, file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/p_int_2006.R")


# Coefficient tidying ----------------------------------------------------------------------

p_int_coef <- comb_coef_tab("p_int")

write_csv(p_int_coef, path = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/p_int_coef.csv")

# Risk tidying ------------------------------------------------------------------------------
p_int_risks <- comb_risk_tab("p_int")

write_csv(p_int_risks, path = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/p_int_risks.csv")

#-----------------------------------------------------------------------------------------------------------------------------------


### Prior interactions, but taking out three way interactions ------------------------------------------- 
sens_int_1981 <- run_mediation(cohort = 1981, glm_sep_title = NULL, glm_tob_title = NULL,
                            glm_sep_spec = fam_sens_int_sep, glm_tob_spec = fam_sens_int_tob, model_name = "Sens Interaction")

sens_int_1996 <- run_mediation(cohort = 1996, glm_sep_title = NULL, glm_tob_title = NULL,
                            glm_sep_spec = fam_sens_int_sep, glm_tob_spec = fam_sens_int_tob, model_name = "Sens Interaction")

sens_int_2006 <- run_mediation(cohort = 2006, glm_sep_title = NULL, glm_tob_title = NULL,
                            glm_sep_spec = fam_sens_int_sep, glm_tob_spec = fam_sens_int_tob, model_name = "Sens Interaction")

save(sens_int_1981, file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/sens_int_1981.R")
save(sens_int_1996, file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/sens_int_1996.R")
save(sens_int_2006, file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/sens_int_2006.R")

# Tidying up coefficients -------------------------------------
sens_int_coef <- comb_coef_tab("sens_int")
write_csv(sens_int_coef, path = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/sens_int_coef.csv")

# Tidying up risks ---------------------------------------------
sens_int_risks <- comb_risk_tab("sens_int")

write_csv(sens_int_risks, path = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/sens_int_risks.csv")

### AIC Comparison table -----------------------

test <- comb_aic_comp("p_int", "sens_int")
test <- test %>%
          filter (Model == "Tob")

write_csv(test, path = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/sens_int_aic.csv")



 
