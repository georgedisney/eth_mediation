# Plotting mediation results ---------------------------------

library (ggplot2)
library (tidyr)
library (forcats)
library (readr)
library (dplyr)
# Function for creating a midpoint variable ----------------------------------------------------------
mid_point_fun <- function (x){
  if (x == 1981){
    return (x + 1.5)
  } else if (x == 1996){
    return (x + 1.5)
  } else
    return (x + 2.5)
  
}

# Function for creating plot template ----------------------------------------------------------------

theme_NZCMS <- function (base_size = 15, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = "")+
    theme (
      plot.title = element_text(size = base_size*1.2, vjust = 2, face = "bold"),
      panel.border = element_rect(fill=NA, colour="white"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(colour="gray80", size = 0.3),
      panel.margin = unit(1.3, "lines"),
      strip.background = element_rect(fill = "white", colour = "white"),
      strip.text = element_text(size = base_size*1.2, face = "bold"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom"
      
    )
}


# Reading in and preparing data ----------------------------------------------------------------------------------

plot_data <- read_csv(file = "G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Eth_Mediation/boot_stats_uncer.csv")

#plot_data$CenYear <- as.numeric(levels(plot_data$CenYear))[plot_data$CenYear]
plot_data$mid_point <- sapply (plot_data$CenYear, mid_point_fun)
plot_data <- plot_data %>%
              select(-limit, -date_run, -stat)

plot_low <- plot_data %>%
              select(CenYear, Sex, Age_group_broad, mid_point, contains("low")) %>%
              gather(mediation_stat, value_low, -CenYear, -Sex, - Age_group_broad, -mid_point)

plot_med <- plot_data %>%
              select(everything(), -contains("low")) %>%
              select(everything(), -contains("upp")) %>%
              gather(mediation_stat, value_med, -CenYear, -Sex, - Age_group_broad, -mid_point)

plot_upp <- plot_data %>%
              select(CenYear, Sex, Age_group_broad, mid_point, contains("upp")) %>%
              gather(mediation_stat, value_upp, -CenYear, -Sex, - Age_group_broad, -mid_point)



plot_data <- full_join(plot_low, plot_med)%>%
                full_join(.,plot_upp)

plot_data$mediation_stat <- as.factor(plot_data$mediation_stat)


plot_data <- plot_data %>%
          mutate(mediation_stat = fct_recode(mediation_stat,
                                             "0_cw_sep_risk" = "0_cw_sep_risk_low", 
                                             "0_cw_sep_risk" ="0_cw_sep_risk_upp", 
                                             "0_cw_smo_risk" = "0_cw_smo_risk_low", 
                                             "0_cw_smo_risk" = "0_cw_smo_risk_upp", 
                                             "0_never_risk" = "0_never_risk_low", 
                                             "0_never_risk" = "0_never_risk_upp", 
                                             "0_obs_risk" = "0_obs_risk_low", 
                                             "0_obs_risk" = "0_obs_risk_upp", 
                                             "0_pred_sep_risk" = "0_pred_sep_risk_low",
                                             "0_pred_sep_risk" = "0_pred_sep_risk_upp", 
                                             "0_pred_smo_risk" = "0_pred_smo_risk_low", 
                                             "0_pred_smo_risk" = "0_pred_smo_risk_upp", 
                                             "1_cw_sep_risk" = "1_cw_sep_risk_low", 
                                             "1_cw_sep_risk" = "1_cw_sep_risk_upp", 
                                             "1_cw_smo_risk" = "1_cw_smo_risk_low", 
                                             "1_cw_smo_risk" = "1_cw_smo_risk_upp", 
                                             "1_never_risk" = "1_never_risk_low",
                                             "1_never_risk" = "1_never_risk_upp", 
                                             "1_obs_risk" = "1_obs_risk_low", 
                                             "1_obs_risk" = "1_obs_risk_upp", 
                                             "1_pred_sep_risk" = "1_pred_sep_risk_low", 
                                             "1_pred_sep_risk" = "1_pred_sep_risk_upp", 
                                             "1_pred_smo_risk" = "1_pred_smo_risk_low",
                                             "1_pred_smo_risk" = "1_pred_smo_risk_upp", 
                                             "CDE_abs" = "CDE_abs_low",
                                             "CDE_abs" = "CDE_abs_upp", 
                                             "CDE_rel" = "CDE_rel_low", 
                                             "CDE_rel" = "CDE_rel_upp", 
                                             "delta_pc_med" = "delta_pc_med_low", 
                                             "delta_pc_med" = "delta_pc_med_upp", 
                                             "NDE_sep_abs" = "NDE_sep_abs_low", 
                                             "NDE_sep_abs" = "NDE_sep_abs_upp", 
                                             "NDE_sep_rel" = "NDE_sep_rel_low", 
                                             "NDE_sep_rel" = "NDE_sep_rel_upp", 
                                             "NDE_smo_abs" = "NDE_smo_abs_low", 
                                             "NDE_smo_abs" = "NDE_smo_abs_upp", 
                                             "NDE_smo_rel" = "NDE_smo_rel_low",
                                             "NDE_smo_rel" = "NDE_smo_rel_upp", 
                                             "NIE_sep_abs" = "NIE_sep_abs_low", 
                                             "NIE_sep_abs" = "NIE_sep_abs_upp", 
                                             "NIE_sep_rel" =  "NIE_sep_rel_low",
                                             "NIE_sep_rel" = "NIE_sep_rel_upp",
                                             "NIE_smo_abs" = "NIE_smo_abs_low",
                                             "NIE_smo_abs" = "NIE_smo_abs_upp", 
                                             "NIE_smo_rel" = "NIE_smo_rel_low", 
                                             "NIE_smo_rel" = "NIE_smo_rel_upp", 
                                             "pc_erad" = "pc_erad_low", 
                                             "pc_erad" = "pc_erad_upp", 
                                             "pc_med_sep" = "pc_med_sep_low",
                                             "pc_med_sep" = "pc_med_sep_upp", 
                                             "pc_med_smo" = "pc_med_smo_low",
                                             "pc_med_smo" = "pc_med_smo_upp", 
                                             "pred_delta_pc_med" = "pred_delta_pc_med_low", 
                                             "pred_delta_pc_med" = "pred_delta_pc_med_upp", 
                                             "pred_NDE_sep_abs" = "pred_NDE_sep_abs_low",
                                             "pred_NDE_sep_abs" = "pred_NDE_sep_abs_upp", 
                                             "pred_NDE_sep_rel" = "pred_NDE_sep_rel_low", 
                                             "pred_NDE_sep_rel" = "pred_NDE_sep_rel_upp", 
                                             "pred_NDE_smo_abs" = "pred_NDE_smo_abs_low",
                                             "pred_NDE_smo_abs" = "pred_NDE_smo_abs_upp", 
                                             "pred_NDE_smo_rel" = "pred_NDE_smo_rel_low", 
                                             "pred_NDE_smo_rel" = "pred_NDE_smo_rel_upp", 
                                             "pred_NIE_sep_abs" = "pred_NIE_sep_abs_low", 
                                             "pred_NIE_sep_abs" = "pred_NIE_sep_abs_upp", 
                                             "pred_NIE_sep_rel" = "pred_NIE_sep_rel_low", 
                                             "pred_NIE_sep_rel" = "pred_NIE_sep_rel_upp", 
                                             "pred_NIE_smo_abs" = "pred_NIE_smo_abs_low",
                                             "pred_NIE_smo_abs" = "pred_NIE_smo_abs_upp", 
                                             "pred_NIE_smo_rel" = "pred_NIE_smo_rel_low", 
                                             "pred_NIE_smo_rel" = "pred_NIE_smo_rel_upp", 
                                             "pred_pc_erad" = "pred_pc_erad_low", 
                                             "pred_pc_erad" = "pred_pc_erad_upp", 
                                             "pred_pc_med_sep" = "pred_pc_med_sep_low", 
                                             "pred_pc_med_sep" = "pred_pc_med_sep_upp", 
                                             "pred_pc_med_smo" = "pred_pc_med_smo_low", 
                                             "pred_pc_med_smo" = "pred_pc_med_smo_upp", 
                                             "pred_TE_abs" = "pred_TE_abs_low", 
                                             "pred_TE_abs" = "pred_TE_abs_upp", 
                                             "pred_TE_rel" = "pred_TE_rel_low", 
                                             "pred_TE_rel" = "pred_TE_rel_upp",
                                             "pred_TE_smo_abs" = "pred_TE_smo_abs_low",
                                             "pred_TE_smo_abs" = "pred_TE_smo_abs_upp", 
                                             "pred_TE_smo_rel" = "pred_TE_smo_rel_low", 
                                             "pred_TE_smo_rel" = "pred_TE_smo_rel_upp", 
                                             "TE_abs" = "TE_abs_low", 
                                             "TE_abs" = "TE_abs_upp", 
                                             "TE_rel" = "TE_rel_low", 
                                             "TE_rel" = "TE_rel_upp"))%>%
  droplevels.data.frame(.)
                 
                 
                 



plot_low <- plot_data %>%
          select(-value_med, - value_upp)%>%
          filter(value_low != "NA")

plot_med <- plot_data %>%
              select(-value_low, - value_upp)%>%
               filter(value_med != "NA")  

plot_upp <- plot_data %>%
              select(-value_low, - value_med)%>%
              filter(value_upp != "NA")

plot_data <- inner_join(plot_med, plot_low, by = c("CenYear", "Sex", "mediation_stat", "Age_group_broad", "mid_point")) %>%
          inner_join(., plot_upp, by = c("CenYear", "Sex", "mediation_stat", "Age_group_broad", "mid_point"))


plot_data$Age_group_broad <- factor(plot_data$Age_group_broad, levels = c("2574", "2544", "4564", "6574"), 
                                    labels = c("25-74", "25-44", "45-64", "65-74"), ordered = TRUE)

plot_data$Sex <- factor(plot_data$Sex, labels = c("Males", "Females"))




ineq_dat <- plot_data %>%
              filter(mediation_stat %in% c("0_pred_sep_risk", "1_pred_sep_risk")) %>%
              droplevels.data.frame(.)



sep_med_dat <- plot_data %>%
                filter(mediation_stat %in% c("0_pred_sep_risk", "1_pred_sep_risk", "0_cw_sep_risk")) %>%
                droplevels.data.frame(.)


tob_med_dat <- plot_data %>%
                filter(mediation_stat %in% c("0_pred_smo_risk", "1_pred_smo_risk", "0_cw_sep_risk", "0_cw_smo_risk"),
                       Age_group_broad == "25-74")



erad_smo_dat <- plot_data %>%
                  filter(mediation_stat %in% c("0_pred_smo_risk", "1_pred_smo_risk",  "1_never_risk", "0_never_risk" )) %>%
                  droplevels.data.frame(.)





ineq <- ggplot(ineq_dat, aes(x = mid_point, y = value_med))+
  geom_line(aes(group = mediation_stat, colour = mediation_stat))+
  geom_point(aes(shape = mediation_stat), size = 2.5)+                
  geom_ribbon(aes(ymin = value_low, ymax = value_upp, fill = mediation_stat, group = mediation_stat), 
              alpha = 0.1)+
  scale_y_continuous("Standardised Risk (per 100,000)", expand = c(0.04,0))+
  scale_x_continuous("Mid Date", expand = c(0.07,0))+
  geom_hline(yintercept = 0, size = 0.4, color = "black")+    
  geom_vline(xintercept = 1980, size = 0.4, color = "black")+
  scale_colour_manual(values = c("#00BFC4", "#F8766D"))+
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
  theme_NZCMS()+
  facet_grid(~Age_group_broad ~Sex, scales = "free") 






ggplot(eth_trend_males1, aes(x=MidDate, y=StdRate))+
  geom_line(aes(group=VExposVal, colour = VExposVal), size=0.3)+
  geom_point(aes(shape=VExposVal), size = 2.5)+
  aes(xmin=1980)+
  scale_y_continuous("Standardised Rate (per 100,000)", expand = c(0.04,0))+
  scale_x_continuous("Mid Date", expand = c(0.07,0))+
  geom_ribbon(aes(ymin = SRCI95Low, ymax = SRCI95Upp, fill = VExposVal, group = VExposVal), 
              alpha = 0.1)+
  expand_limits(y = 0)+
  geom_blank()+
  theme_NZCMS(base_size = 15)+
  ggtitle("Standardised Rate, 1-74 yrs \n Males")+
  theme(plot.title = element_text(size = 16, vjust = 2))+
  theme(panel.border = element_rect(fill=NA, colour="white"))+
  theme(panel.background = element_rect(fill = "white"))+
  theme(panel.grid.major = element_line(colour="gray80", size = 0.3))+
  theme(panel.margin = unit(1.3, "lines"))+
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(size = 15, face = "bold"))+
  # theme(axis.ticks = element_blank())+
  theme(legend.position = "bottom")+
  geom_hline(yintercept = 0, size = 0.4, color = "black")+    
  geom_vline(xintercept = 1980, size = 0.4, color = "black")+
  #scale_color_manual(values = c("#F8766D", "#00BFC4", "#00BA38"))+
  scale_colour_manual(values = c("#F8766D", "#00BFC4", "#00BA38"),name = "", 
                      labels = expression(paste("NZ M",bar(a),"ori", sep = ""),
                                          "Pacific", European/Other)) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#00BA38"), name = "", labels = expression(paste("NZ M",bar(a),"ori", sep = ""),
                                                                                                "Pacific", European/Other))+
  scale_shape_discrete(name = "", labels = expression(paste("NZ M",bar(a),"ori", sep = ""),
                                                      "Pacific", European/Other))+
  facet_wrap(~EventCode, ncol = 1, scales = "free")






















sep_med <- ggplot(sep_med_dat, aes(x = mid_point, y = value_med))+
                  geom_line(aes(group = mediation_stat, colour = mediation_stat))+
                  geom_point(aes(shape = mediation_stat), size = 2.5)+                
                  geom_ribbon(aes(ymin = value_low, ymax = value_upp, fill = mediation_stat, group = mediation_stat), 
                                  alpha = 0.1)+
                  scale_y_continuous("Standardised Risk (per 100,000)", expand = c(0.04,0))+
                  scale_x_continuous("Mid Date", expand = c(0.07,0))+
                  geom_hline(yintercept = 0, size = 0.4, color = "black")+    
                  geom_vline(xintercept = 1980, size = 0.4, color = "black")+
                  theme_NZCMS()+
                  facet_grid(~Age_group_broad ~Sex, scales = "free") 

tob_med <- ggplot(tob_med_dat, aes(x = mid_point, y = value_med))+
            geom_line(aes(group = mediation_stat, colour = mediation_stat))+
            geom_point(aes(shape = mediation_stat), size = 2.5)+                
            geom_ribbon(aes(ymin = value_low, ymax = value_upp, fill = mediation_stat, group = mediation_stat), 
                       alpha = 0.1)+
            scale_y_continuous("Standardised Risk (per 100,000)", expand = c(0.04,0))+
            scale_x_continuous("Mid Date", expand = c(0.09,0))+
            geom_hline(yintercept = 0, size = 0.4, color = "black")+    
            geom_vline(xintercept = 1980, size = 0.4, color = "black")+
            theme_NZCMS()+
            facet_grid(~Sex, scales = "free") 
  


ggsave (plot = ineq, 
        path = "G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Eth_Mediation/Plots/",
        filename = "ineq.png",
        type = "cairo", scale = 3, height = 12, width = 5.6, units = "cm", dpi = 300)


ggsave (plot = sep_med, 
        path = "G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Eth_Mediation/Plots/",
        filename = "sep_med.png",
        type = "cairo", scale = 3, height = 12, width = 5.6, units = "cm", dpi = 300)

ggsave (plot = tob_med, 
        path = "G:/Publications/Papers/411_NZCMS CT Eth-Mortality mediation by SEP tobacco/411_R/Eth_Mediation/Plots/",
        filename = "tob_med.png",
        type = "cairo", scale = 3, height = 5, width = 7, units = "cm", dpi = 300)


