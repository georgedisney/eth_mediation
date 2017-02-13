#### Putting bootstrap results into a tidy table -------------------------------------------
library(tidyr)
library(dplyr)

# Function for putting one set of cohort-sex bootstrap results into a table  ---------------------------------------------------------------------------

boot_results <- function (boot_fam_cohort){

x <- list(1:8, 9:16, 17:24, 25:32, 33:40, 41:48, 49:56, 57:64, 65:72, 73:80, 81:88, 89:96)
y <- list("CenYear", "Sex", "cross_ref", "Age_group_broad", "obs_risk", "pred_sep_risk", "cw_sep_risk", 
          "pred_smo_risk", "cw_smo_risk", "never_risk", "model_set", "Date_run")
z <- list("Boot_ind_a", "CenYear", "Boot_ind", "Sex", "Boot_ind", "cross_ref", "Boot_ind", "Age_group_broad", 
          "Boot_ind","obs_risk","Boot_ind", "pred_sep_risk", "Boot_ind", "cw_sep_risk", 
          "Boot_ind","pred_smo_risk","Boot_ind", "cw_smo_risk","Boot_ind", "never_risk","Boot_ind", "model_set","Boot_ind", "Date_run")

boot_object <- get(boot_fam_cohort)

dat <- as.data.frame(boot_object$t)
dat <-  lapply(x, function(i) return(dat[,i]))
dat <- Map(cbind, dat, a = y)

melt_fun <- function(x){
              dat_temp <- x
              melt(dat_temp, id.vars = "a")
}

dat <- lapply(dat, melt_fun)

select_fun <- function (x){
                dat_temp <- x
                select (dat_temp, variable, value)
}

dat <- lapply(dat, select_fun)
dat <- as.data.frame(bind_cols(dat))
colnames(dat)<- z

dat <- dat[,!duplicated(colnames(dat))]%>%
        select(., -Boot_ind)

dat[6:11] <- as.numeric(sapply (dat[6:11],as.character))
dat
}


# Putting interaction family bootstrap results into a single table 

load(file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/boot_int_females_1981.R")
load(file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/boot_int_females_1996.R")
load(file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/boot_int_females_2006.R")
load(file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/boot_int_males_1981.R")
load(file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/boot_int_males_1996.R")
load(file = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/boot_int_males_2006.R")



boot_int_results <- bind_rows(boot_results("boot_int_females_1981"), boot_results("boot_int_males_1981"),
                              boot_results("boot_int_females_1996"), boot_results("boot_int_males_1996"),
                              boot_results("boot_int_females_2006"), boot_results("boot_int_males_2006"))


write_csv(boot_int_results, 
          path = "/home/STATSNZ/dl_gdisney_int/Network-Shares/Datalab-MA/MAA2006-04 Cancer Trends/NZCMS_CT/TempDatasets/GDforR/mediation_results/boot_int_results.csv")




