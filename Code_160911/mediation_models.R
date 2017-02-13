#### Model list -------------------

### TB Prior Model ------------------------------------------------------ 

fam_prior_sep <- "Link ~ Age_group + TotMaori + EqIncJen_log_cent + NZDep10_cent + LabSt3 + HQual3 + Ret_lfs + Age_group*TotMaori"

fam_prior_tob <- "Link ~ Ret_lfs + Age_group*TotMaori + Age_group*EqIncJen_log_cent + Age_group*NZDep10_cent + Age_group*HQual3 + 
                         SmkStat*TotMaori + SmkStat*Age_group +
                         Age_group + TotMaori + EqIncJen_log_cent +  NZDep10_cent + HQual3 + LabSt3"


### TB Prior Model with interactions ------------------------------------

fam_p_int_sep <- "Link ~ Age_group + TotMaori + EqIncJen_log_cent + NZDep10_cent + LabSt3 + HQual3 + Ret_lfs + Age_group*TotMaori +
                         TotMaori*EqIncJen_log_cent + TotMaori*NZDep10_cent + TotMaori*LabSt3 + TotMaori*HQual3 + TotMaori*Ret_lfs"

fam_p_int_tob <- "Link ~ Age_group + TotMaori + EqIncJen_log_cent + NZDep10_cent + LabSt3 + HQual3 + Ret_lfs + Age_group*TotMaori +
                         TotMaori*EqIncJen_log_cent + TotMaori*NZDep10_cent + TotMaori*LabSt3 + TotMaori*HQual3 + TotMaori*Ret_lfs +
                         SmkStat + SmkStat*TotMaori + SmkStat*Age_group + SmkStat*Age_group*TotMaori"


### TB Prior Tobacco Model with interactions, but removing three way interaction ------------------------
fam_sens_int_sep <- "Link ~ Age_group + TotMaori + EqIncJen_log_cent + NZDep10_cent + LabSt3 + HQual3 + Ret_lfs + Age_group*TotMaori +
                         TotMaori*EqIncJen_log_cent + TotMaori*NZDep10_cent + TotMaori*LabSt3 + TotMaori*HQual3 + TotMaori*Ret_lfs"


fam_sens_int_tob <- "Link ~ Age_group + TotMaori + EqIncJen_log_cent + NZDep10_cent + LabSt3 + HQual3 + Ret_lfs + Age_group*TotMaori +
                              TotMaori*EqIncJen_log_cent + TotMaori*NZDep10_cent + TotMaori*LabSt3 + TotMaori*HQual3 + TotMaori*Ret_lfs +
                              SmkStat + SmkStat*TotMaori + SmkStat*Age_group"
