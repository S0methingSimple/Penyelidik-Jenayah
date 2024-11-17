pacman::p_load(shiny, bslib, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes, 
               tmaptools, ggplot2, gridExtra, ClustGeo, 
               ggpubr, cluster, factoextra, NbClust,
               heatmaply, corrplot, psych, GGally)

# Load analytics module
source("./eda/eda.R")
source("./spa/spa.R")
source("./spc/spc.R")

#========================#
######## Shiny UI ########
#========================# 

ui <- navbarPage(
  title = "PENYELIDIK JENAYAH 👮🏽👮🏽‍♂👮🏽‍♀️️️",
  fluid = TRUE,
  theme=shinytheme("darkly"),
  id = "navbarID",
  tabPanel("Overview",
     h1("Crime Hotspots: A Malaysian Perspective"),
     img(src = "banner.png", style="width:100%;"),
     h3("Introduction"),
     div("This research delves into the spatial dynamics of crime in Malaysia, utilizing advanced spatial analysis techniques to uncover patterns and trends. By analyzing a comprehensive dataset of crime incidents, we aim to identify hotspots, coldspots, and spatial clusters of criminal activity. Through this exploration, we seek to provide valuable insights into the factors influencing crime and to inform evidence-based crime prevention strategies. Previous studies have demonstrated the efficacy of spatial analysis in understanding crime patterns. Research has shown that property crimes tend to cluster in urban areas, while violent crimes may exhibit different spatial patterns. Spatial autocorrelation and clustering techniques have been employed to identify spatial relationships between crime and socio-economic factors, providing valuable insights for targeted interventions. Additionally, exploratory spatial data analysis (ESDA) has been used to uncover spatial autocorrelation and clustering in crime data."),
     h3("Significance of Spatial Analysis"),
     div("Spatial analysis offers a powerful lens to examine crime patterns and their underlying causes. By visualizing crime data on maps, we can identify spatial clusters, hotspots, and coldspots. This information can help law enforcement agencies allocate resources more effectively, prioritize areas for increased surveillance, and develop targeted crime prevention strategies. Moreover, by analyzing the spatial relationships between crime and other factors, such as socioeconomic conditions and infrastructure, we can gain a deeper understanding of the factors driving crime and develop evidence-based policies."),
     h3("Breakdown of Analysis"),
     tags$ul("Exploratory Data Analysis (EDA): Visualize the spatial and temporal trends of crime in Malaysia."),
     tags$ul("Spatial Autocorrelation (SPA): Explore the spatial dependency of crime incidents across different regions."),
     tags$ul("Spatial Clustering (SPC): Identify clusters of similar crime patterns and understand their spatial distribution."),
     img(src = "smu-logo.jpeg"),
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