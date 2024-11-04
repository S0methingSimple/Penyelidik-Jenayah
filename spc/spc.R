pacman::p_load(shiny, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes)


spc_ui <- tabPanel("Spatial Clustering",
  sidebarLayout(
    sidebarPanel(
      h3("Side Bar is here")
    ),
    mainPanel(
      h3("Main Panel")
    )
  )
)

spc_server <- function(input, output) {
  
}