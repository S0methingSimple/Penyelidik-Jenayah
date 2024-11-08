pacman::p_load(shiny, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes)


spa_ui <- tabPanel("Spatial Autocorrelation",
  sidebarLayout(
    sidebarPanel(
      h3("Side Bar is here")
    ),
    mainPanel(
      h3("Main Panel")
    )
  )
)

spa_server <- function(input, output) {
  
}