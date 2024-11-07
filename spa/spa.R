pacman::p_load(shiny, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes, bslib,
               sf, st, tidyverse, raster, tmap, tmaptools, ggplot2, spatstat, sfdep, spdep)

spa_crime_df_mys_grp <- read_rds("data/spa/rds/crime_df_mys_grp.rds")
spa_total_crime_pc <- spa_crime_df_mys_grp$total_crimes_pc
spa_geometry <- spa_crime_df_mys_grp$geometry
spa_HCSA <- read_rds("data/spa/rds/HCSA.rds")


############################ Shiny UI ############################ 

spa_ui <- tabPanel("Spatial Autocorrelation",
  navset_card_pill(
    nav_panel("Local Moran's I",
     sidebarLayout(
       sidebarPanel(
         radioButtons(inputId = "Contiguity1",
                      label = "Contiguity Method",
                      choices = c("Queen" = TRUE, 
                                  "Rook" = FALSE),
                      selected = TRUE),
         selectInput("MoranWeights", "Spatial Weights Style",
                     choices = c("W: Row standardised" = "W",
                                 "B: Binary" = "B",
                                 "C: Globally standardised" = "C",
                                 "U: C / no of neighbours" = "U",
                                 "minmax" = "minmax",
                                 "S: Variance" = "S"),
                     selected = "W"),
         sliderInput(inputId = "MoranSims", 
                     label = "Number of Simulations:", 
                     min = 99, max = 499,
                     value = 99, step = 100),
         selectInput("localmoranstats", "Select Local Moran's Stat:",
                     choices = c("local moran's" = "ii",
                                 "expectation" = "eii",
                                 "variance" = "var_ii",
                                 "std deviation" = "z_ii",
                                 "P-value" = "p_ii_sim"),
                     selected = "ii"),
         sliderInput(inputId = "MoranStatSig", 
                     label = "Statistical Significance:", 
                     min = 0.05, max = 1,
                     value = 0.05, step = 0.05),
         actionButton("MoranUpdate", "Update Plot")
       ),
       mainPanel(
         tmapOutput("LocalMoranMap")
       )
     )
   ),
     nav_panel("Hot Cold Spot Analysis",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("LocalGi", "Gi",
                               choices = c("Gi*" = "gi_star"),
                               selected = "gi_star"),
                   sliderInput(inputId = "GiStarSig", 
                               label = "Statistical Significance:", 
                               min = 0.05, max = 1,
                               value = 0.05, step = 0.05),
                   sliderInput(inputId = "HCSASims", 
                               label = "Number of Simulations:", 
                               min = 99, max = 499,
                               value = 99, step = 100),
                   actionButton("GiStarUpdate", "Update Plot")
                 ),
                 mainPanel(
                   tmapOutput("GiStarmap")
                 )
               )
     )
  )
)

###### Shiny Server ######

spa_server <- function(input, output){
  localMIResults <- eventReactive(input$MoranUpdate,{
    tmap_mode("view")
    
    mys_nb_q <- st_contiguity(spa_crime_df_mys_grp, queen = input$Contiguity1)
    mys_nb_q[[17]] <- as.integer(c(18))
    mys_nb_q[[18]] <- as.integer(sort(unique(c(mys_nb_q[[18]], 17))))
    mys_wm_rs <- st_weights(mys_nb_q, style = input$MoranWeights)
    
    wm_q <- spa_crime_df_mys_grp %>%
      ungroup() %>%
      mutate(nb = mys_nb_q,
             wt = mys_wm_rs,
             .before = 1) 
    
    lisa <- wm_q %>%
      mutate(local_moran = local_moran(
        spa_total_crime_pc, nb, wt,
        nsim = as.numeric(input$MoranSims)),
        .before = 1) %>%
      unnest(local_moran)
    
    return(lisa)
  })
  
  # Render output maps
  output$LocalMoranMap <- renderTmap({
    lisa <- localMIResults()
    
    lisa_sig <- lisa  %>%
      filter(p_ii_sim < input$MoranStatSig)
    
    localMI_map <- tm_shape(lisa_sig) +
      tm_fill(col = input$localmoranstats, 
              palette = c("#b7dce9","#e1ecbb","#f5f3a6",
                          "#f8d887","#ec9a64","#d21b1c"),
              title = input$localmoranstats,
              midpoint = NA,
              legend.hist = TRUE, 
              legend.is.portrait = TRUE,
              legend.hist.z = 0.1) +
      tm_borders(col = "black", alpha = 0.6)
    
    localMI_map
  })
  
  output$LISA <- renderTmap({
    lisa <- localMIResults()
    
    lisa_sig <- lisa  %>%
      filter(p_ii_sim < input$MoranStatSig)
    
    study_area_lisa <- tm_shape(lisa)+
      tm_polygons() +
      tm_borders(col = "black", alpha = 0.6)+
    tm_shape(lisa_sig)+
    tm_fill(input$LisaClass, 
            palette = c("#b7dce9","#ec9a64","#e1ecbb", "#d21b1c"),
            title = "LISA class",
            midpoint = NA,
            legend.hist = TRUE, 
            legend.is.portrait = TRUE,
            legend.hist.z = 0.1) +
    tm_borders(col = "black", alpha = 0.6)
  
    study_area_lisa
  })
  
  GiStarResults <- eventReactive(input$GiStarUpdate,{
    tmap_mode("view")
    
    return(spa_HCSA)
  })
  
  # Render output maps
  output$GiStarmap <- renderTmap({
    HCSA <- GiStarResults()
    
    
    tm_shape(HCSA) +
      tm_polygons() +
    HCSA_sig_map <- tm_shape(HCSA_sig) +
      tm_fill(col = "gi_star", 
              palette = c("#b7dce9","#e1ecbb","#f5f3a6",
                          "#f8d887","#ec9a64","#d21b1c"),
              title = "HCSA Gi*",
              midpoint = NA,
              legend.hist = TRUE, 
              legend.is.portrait = TRUE,
              legend.hist.z = 0.1) +
      tm_borders(col = "black", alpha = 0.6)
    
    HCSA_sig_map
  })
  
  
}

