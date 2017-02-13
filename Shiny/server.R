library(dplyr)
library(ggplot2)
library(Cairo)
library(grid)
library(scales)
library(directlabels)
library (RColorBrewer)
#library (plotly)
options(shiny.usecairo=T)


med_data_master <- readRDS("data/med_data_master.RDS")

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


shinyServer(function (input, output){
  
  age_select <- reactive ({input$age_select})
  stat_select <- reactive ({input$stat_select})

  med_data <- reactive ({ med_data_master %>%
                          filter (Age_group_broad %in% age_select(),
                                  mediation_stat %in% stat_select ()
      )
  })
  
  
  
  
  
  
  #   max_axis <- reactive({input$axis}) 
  
  
  output$med_plot <- renderPlot({
    
    
    
    p_med <- ggplot(med_data(), aes(x = mid_point, y = value_med))+
      geom_line(aes(group = mediation_stat, colour = mediation_stat))+
      geom_point(aes(shape = mediation_stat), size = 2.5)+                
      geom_ribbon(aes(ymin = value_low, ymax = value_upp, fill = mediation_stat, group = mediation_stat), 
                  alpha = 0.1)+
      scale_y_continuous("Standardised Risk (per 100,000)", expand = c(0.04,0))+
      scale_x_continuous("Mid Date", expand = c(0.07,0))+
      geom_hline(yintercept = 0, size = 0.4, color = "black")+    
      geom_vline(xintercept = 1980, size = 0.4, color = "black")+
      #scale_colour_manual(values = c("#00BFC4", "#F8766D"))+
      #scale_fill_manual(values = c("#00BFC4", "#F8766D"))+
      theme_NZCMS()+
      facet_grid(~Age_group_broad ~Sex, scales = "free") 
    
    
    
    #invisible(print
    #         (
    p_med
    #        ))
  },height = reactive ({height = input$plot_size}), width = 1100)   
  
})  


