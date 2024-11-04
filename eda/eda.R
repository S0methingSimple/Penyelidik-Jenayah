pacman::p_load(shiny, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes)


eda_ui <- tabPanel("Exploratory Data Analysis",
  sidebarLayout(
    sidebarPanel(
      h3("Side Bar is here")
    ),
    mainPanel(
      h3("Main Panel")
    )
  )
)

eda_server <- function(input, output) {
  
}