pacman::p_load(shiny, sf, tmap, bslib, tidyverse, sfdep, shinydashboard, shinythemes)


# Load analytics module
source("./eda/eda.R")
source("./spa/spa.R")
source("./spc/spc.R")

#========================#
######## Shiny UI ########
#========================# 

ui <- navbarPage(
  title = "Penyelidik Jenayah",
  fluid = TRUE,
  theme=shinytheme("darkly"),
  id = "navbarID",
  tabPanel("Home",
     sidebarLayout(
       sidebarPanel(
         h3("Side Bar is here")
       ),
       mainPanel(
         h3("Main Panel")
       )
     )
  ),
  eda_ui,
  spa_ui,
  spc_ui
)

#========================#
###### Shiny Server ######
#========================# 

server <- function(input, output){
  eda_server(input, output)
  spa_server(input, output)
  spc_server(input, output)
}

shinyApp (ui=ui, server=server)