pacman::p_load(shiny, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes, bslib,
               sf, st, tidyverse, raster, tmap, tmaptools, ggplot2, spatstat, sfdep, spdep)

spa_crime_df_mys_grp <- read_rds("data/spa/rds/crime_df_mys_grp.rds")


############################ Shiny UI ############################ 

spa_ui <- tabPanel("Spatial Autocorrelation",
  navset_card_pill(
    nav_panel("Global Measures",
     sidebarLayout(
       sidebarPanel(
         h4("Choose Permutation Test"),
         radioButtons(inputId = "HistogramTest",
                      label = "Permutation Test Method:",
                      choices = c("Global Moran's I" = TRUE, 
                                  "Global Geary C's" = FALSE),
                      selected = TRUE),
         hr(),
         h4("Customise Weight Matrix"),
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
         actionButton("GHistWmUpdate", "Update Histogram")
       ),
       mainPanel(
         verbatimTextOutput("MCText"),
         plotOutput("HistogramTestPlot",
                    width = "95%",
                    height = 580)
       )
     )
   ),
   nav_panel("Local Measures",
             sidebarLayout(
               sidebarPanel(
                 h4("Customize Indicator"),
                 radioButtons(inputId = "LISAIndicator",
                              label = "Select LISA Indicator:",
                              choices = c("Local Moran's I" = TRUE,
                                          "LISA classification" = FALSE),
                              selected = TRUE),
                 conditionalPanel(
                   condition = "input.LISAIndicator == 'TRUE'",
                   selectInput("localmoranstats", "Select Local Moran's Stat:",
                               choices = c("local moran's" = "ii",
                                           "expectation" = "eii",
                                           "variance" = "var_ii",
                                           "std deviation" = "z_ii",
                                           "P-value" = "p_ii_sim"),
                               selected = "ii")
                 ),
                 conditionalPanel(
                   condition = "input.LISAIndicator == 'FALSE'",
                   selectInput("LisaClass", "Select Lisa Classification",
                               choices = c("mean" = "mean",
                                           "median" = "median",
                                           "pysal" = "pysal"),
                               selected = "mean")
                 ),
                 hr(),
                 h4("Customise Weight Matrix"),
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
                 sliderInput(inputId = "MoranStatSig", 
                             label = "Statistical Significance:", 
                             min = 0.05, max = 1,
                             value = 0.05, step = 0.05),
                 selectInput(inputId = "colour",
                             label = "Colour scheme:",
                             choices = list("Red-Blue" = "-RdBu", 
                                            "Yellow-Orange-Red" = "YlOrRd",
                                            "Yellow-Orange-Brown" = "YlOrBr",
                                            "Yellow-Green" = "YlGn",
                                            "Orange-Red" = "OrRd"),
                             selected = "-RdBu"),
                 actionButton("MoranUpdate", "Update Plot")
               ),
               mainPanel(
                 tmapOutput("LocalMoranMap",
                            width = "95%",
                            height = 580)
               )
             )
   ),
   nav_panel("Hot Cold Spot Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Choose Hot/Cold Spots"),
                 checkboxInput("hotspots", "View Hotspots", value = TRUE),
                 checkboxInput("coldspots", "View Coldspots", value = TRUE),
                 conditionalPanel(
                   condition = "input.hotspots != input.coldspots",
                   sliderInput(inputId = "topHotColdspots", 
                             label = "Number of spots to preview:", 
                             min = 1, max = 5,
                             value = 3, step = 1)
                 ),
                 hr(),
                 h4("Customise Weight Matrix"),
                 sliderInput(inputId = "MoranSims", 
                             label = "Number of Simulations:", 
                             min = 99, max = 499,
                             value = 99, step = 100),
                 sliderInput(inputId = "GiStarSig", 
                             label = "Statistical Significance:", 
                             min = 0.05, max = 1,
                             value = 0.05, step = 0.05),
                 actionButton("GiStarUpdate", "Update Plot")
               ),
               mainPanel(
                 tmapOutput("GiStarmap",
                            width = "95%",
                            height = 580)
               )
             )
   )
  )
)

###### Shiny Server ######

spa_server <- function(input, output){
  ############## Global SPA ###################
  output$MCText <- renderPrint({ 
    set.seed(123)
    
    mys_nb_q <- st_contiguity(spa_crime_df_mys_grp, queen = input$Contiguity1)
    mys_nb_q[[17]] <- as.integer(c(18))
    mys_nb_q[[18]] <- as.integer(sort(unique(c(mys_nb_q[[18]], 17))))
    mys_wm_rs <- st_weights(mys_nb_q, style = input$MoranWeights)
    
    wm_q <- spa_crime_df_mys_grp %>%
      ungroup() %>%
      mutate(nb = mys_nb_q,
             wt = mys_wm_rs,
             .before = 1) 
    
    if (!!input$HistogramTest == TRUE){
      global_moran_test <- global_moran_test(wm_q$total_crimes_pc,
                                             wm_q$nb,
                                             wm_q$wt,
                                             alternative = "greater")
      global_moran_test
    } else if (!!input$HistogramTest == FALSE) {
      global_c_test <- global_c_test(wm_q$total_crimes_pc,
                                    wm_q$nb,
                                    wm_q$wt,
                                    alternative = "greater")
      global_c_test
    }
    })
  
  output$HistogramTestPlot <- renderPlot({
    set.seed(123)
    
    mys_nb_q <- st_contiguity(spa_crime_df_mys_grp, queen = input$Contiguity1)
    mys_nb_q[[17]] <- as.integer(c(18))
    mys_nb_q[[18]] <- as.integer(sort(unique(c(mys_nb_q[[18]], 17))))
    mys_wm_rs <- st_weights(mys_nb_q, style = input$MoranWeights)
    
    wm_q <- spa_crime_df_mys_grp %>%
      ungroup() %>%
      mutate(nb = mys_nb_q,
             wt = mys_wm_rs,
             .before = 1) 
    
    if (!!input$HistogramTest == TRUE) {
      gmoranMC <- global_moran_perm(wm_q$total_crimes_pc,
                                    wm_q$nb,
                                    wm_q$wt,
                                    nsim = as.numeric(input$MoranSims))
      histogram <- {
        hist(gmoranMC$res, main="Histogram of Global Moran's I Monte-Carlo Simulation Results", xlab="Monte-Carlo Results", ylab="Frequency")
        abline(v=gmoranMC$statistic, col="red") 
      }
    } else if (!!input$HistogramTest == FALSE) {
      bperm <- global_c_perm(wm_q$total_crimes_pc,
                             wm_q$nb,
                             wm_q$wt,
                             nsim = as.numeric(input$MoranSims))
      histogram <- {
        hist(bperm$res, main="Histogram of Global Geary's C Monte-Carlo Simulation Results", xlab="Monte-Carlo Results", ylab="Frequency")
        abline(v=1, col="red") 
        }
    }
    
    histogram
  })
  
  ####### local SPA #######
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
        total_crimes_pc, nb, wt,
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
    
    if (!!input$LISAIndicator == TRUE)
      local_map <- tm_shape(lisa)+
      tm_polygons() +
      tm_shape(lisa_sig) +
        tm_fill(col = input$localmoranstats, 
                palette = input$colour,
                title = input$localmoranstats,
                midpoint = NA,
                legend.hist = TRUE, 
                legend.is.portrait = TRUE,
                legend.hist.z = 0.1) +
        tm_borders(col = "black", alpha = 0.6)
    else if (!!input$LISAIndicator == FALSE) {
      local_map <- tm_shape(lisa)+
        tm_polygons() +
        tm_shape(lisa_sig)+
        tm_fill(input$LisaClass, 
                palette = input$colour,
                title = input$LisaClass,
                midpoint = NA,
                legend.hist = TRUE, 
                legend.is.portrait = TRUE,
                legend.hist.z = 0.1) +
        tm_borders(col = "black", alpha = 0.6)
    }
    
    local_map
  })
  
  ############# HCSA ##################
  
  GiStarResults <- eventReactive(input$GiStarUpdate,{
    tmap_mode("view")
    
    wm_idw <- spa_crime_df_mys_grp %>%
      ungroup() %>%
      mutate(nb = include_self(st_contiguity(geometry)),
             wt = st_inverse_distance(nb, geometry,
                                      scale = 1,
                                      alpha = 1),
             .before = 1)
    
    HCSA <- wm_idw %>% 
      mutate(local_Gi_star = local_gstar_perm(
        total_crimes_pc, nb, wt, nsim = as.numeric(input$MoranSims)),
        .before = 1) %>%
      unnest(local_Gi_star)
    
    return(HCSA)
  })
  
  # Render output maps
  output$GiStarmap <- renderTmap({
    HCSA <- GiStarResults()
    
    HCSA_sig <- HCSA %>%
      filter(p_sim < input$GiStarSig)
    
    if (input$hotspots == input$coldspots)
      HCSA_sig_map <- tm_shape(HCSA) +
        tm_polygons() +
        tm_shape(HCSA_sig) +
        tm_fill(col = "gi_star", 
                palette = "-RdBu",
                title = "HCSA Hot and Coldspots",
                midpoint = NA,
                legend.hist = TRUE, 
                legend.is.portrait = TRUE,
                legend.hist.z = 0.1) +
        tm_borders(col = "black", alpha = 0.6)
    else if (!!input$hotspots == TRUE) {
      hotspots <- (head((HCSA_sig[HCSA_sig$gi_star > 0,]), input$topHotColdspots)$district)
      
      HCSA_hotspots <- HCSA_sig %>% 
        filter(district %in% hotspots)
      
      HCSA_sig_map <- tm_shape(HCSA) +
        tm_polygons() +
        tm_shape(HCSA_hotspots) +
        tm_fill(col = "gi_star", 
                palette = "Reds",
                title = "HCSA Hotspots",
                midpoint = NA,
                legend.hist = TRUE, 
                legend.is.portrait = TRUE,
                legend.hist.z = 0.1) +
        tm_borders(col = "black", alpha = 0.6) }
    else if (!!input$coldspots == TRUE) {
      coldspots <-  (head((HCSA_sig[HCSA_sig$gi_star < 0,]), input$topHotColdspots)$district)
      
      HCSA_coldspots <- HCSA_sig %>% 
        filter(district %in% coldspots)
      
      HCSA_sig_map <- tm_shape(HCSA) +
        tm_polygons() +
        tm_shape(HCSA_coldspots) +
        tm_fill(col = "gi_star", 
                palette = "-Blues",
                title = "HCSA Coldspots",
                midpoint = NA,
                legend.hist = TRUE, 
                legend.is.portrait = TRUE,
                legend.hist.z = 0.1) +
        tm_borders(col = "black", alpha = 0.6)
      }
    
    HCSA_sig_map
  })
}

