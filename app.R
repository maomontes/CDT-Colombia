
library(tidyverse)
library(shiny)
library(shinydashboard)

df_shiny <- readRDS("data/df_dias_cdt_RDS")

ui <- dashboardPage(
    
    dashboardHeader(title = "Tasa Captacion CDT 2020"),
    
    dashboardSidebar(),
    
    dashboardBody()
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
