library(dplyr)
library(ggplot2)
library(Cairo)
library(grid)
library(scales)
library(directlabels)
library (RColorBrewer)
options(shiny.usecairo=T)


med_data_master <- readRDS("data/med_data_master.RDS")



shinyUI(fluidPage(
  
  titlePanel(h3("Ethnicity Mediation Analysis, Exploratory Tool")),
  
  
  sidebarLayout(
    sidebarPanel(
      
      
      p("Select mediation stat and age groups of interest by either clicking or typing query. 
        Clear previous selections using backspace or unchecking."),
      
      
      selectInput( 
        "age_select",
        label=h5(strong("Select Age Group")),
        choices = levels(med_data_master$Age_group_broad),
        selected = "25-74",
        multiple = T),
      
      
      selectInput( 
        "stat_select",
        label=h5(strong("Select Mediation Stat")),
        choices = levels(med_data_master$mediation_stat),
        selected = c("0_pred_sep_risk", "1_pred_sep_risk"),
        multiple = T),
      
    
      
      

    
      
      sliderInput("plot_size",
                  label = h5(strong("Adjust Size of Plot")),
                  min = 450, max = 1500, value = 650)
      
      
      ),
    mainPanel(
      plotOutput("med_plot", height = "100%", width = "100%"),
      class = 'leftAlign',
      position = "bottom")
  )
))  