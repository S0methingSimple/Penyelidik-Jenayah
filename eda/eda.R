pacman::p_load(shiny, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes, 
               sf, st, tidyverse, raster, tmap, tmaptools, ggplot2, plotly,
               spatstat, sfdep, ClustGeo, ggpubr, cluster, factoextra, 
               NbClust, heatmaply, corrplot, psych, tidyverse, GGally)

eda_sf <- read_rds("data/eda/eda.rds")
eda_sf.all <- eda_sf %>% filter(year == 0)
states <- unique(eda_sf$state)
types <- unique(eda_sf$type)
category <- unique(eda_sf$category)
state_lvl <- eda_sf %>% 
  group_by(state, year, category, type) %>%
  summarise(
    m_crime_state = mean(crimes, na.rm = TRUE),
    s_crime_state = sum(crimes, na.rm = TRUE),
    .groups = "drop"  # This explicitly drops the grouping
  )

state_lvl <- st_drop_geometry(state_lvl)

eda_sf <- eda_sf %>%
  left_join(state_lvl, by = c("state", "year", "category", "type")) %>%
  mutate(
    crime_ratio_to_mean = crimes/m_crime_state,
    crime_ratio_to_sum = crimes/s_crime_state
  )

eda_ui <- tabPanel("Exploratory Data Analysis",
                   h1("Visualising Distributions of Crime"), 
                   navset_card_pill( 
                     # First Panel - Choropleth Analysis
                     nav_panel(
                       "Choropleth Analysis",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             "eda_sel_year",
                             "Year",
                             choices = list(
                               "All" = 0,
                               "2019" = 2019,
                               "2020" = 2020,
                               "2021" = 2021,
                               "2022" = 2022
                             ),
                             selected = 0
                           ),
                           radioButtons(
                             "crime_measure",
                             "Select Crime Measure",
                             choices = list(
                               "Overall Crime Count" = "crimes",
                               "Crime per Capita" = "crimes_pc"
                             )
                           ),
                           selectizeInput(
                             "state_select",
                             "Select States",
                             choices = states, # Will be updated in server
                             selected = NULL,
                             multiple = TRUE,
                             options = list(placeholder = 'Select states or leave empty for all')
                           ),
                           
                           selectInput(
                             "choro_category_select",
                             "Select Crime Category",
                             choices = c("All", "assault", "property"),
                             selected = "All"
                           ),
                           uiOutput("choro_type_select"),
                           actionButton("choro_btn", "Update")
                         ),
                         mainPanel(
                           tmapOutput("eda_choro_plot",
                                      width = "95%",
                                      height = 580)
                         )
                       )
                     ),
                     # Second Panel - 
                     nav_panel(
                       "Crime Trends Analysis Dashboard",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             "trend_sel_year",
                             "Year",
                             choices = list(
                               "All" = 0,
                               "2019" = 2019,
                               "2020" = 2020,
                               "2021" = 2021,
                               "2022" = 2022
                             ),
                             selected = 0
                           ),
                           
                           # Metric selection
                           radioButtons("trend_metric", "Select Metric",
                                        choices = c("Total Crimes" = "crimes",
                                                    "Crimes per Capita" = "crimes_pc"),
                                        selected = "crimes"),
                           
                           # State selection
                           selectizeInput("trend_state_select", "Select States",
                                          choices = states,
                                          multiple = TRUE,
                                          selected = NULL,
                                          options = list(placeholder = 'Select states or leave empty for all')),
                           
                           # Category selection
                           selectInput("trend_category_select", "Select Crime Category",
                                       choices = c("All", "assault", "property"),
                                       selected = "All"),
                           
                           # Type selection
                           uiOutput("trend_type_select"),
                         ),
                         mainPanel(
                           fluidRow(
                             column(12,
                                    plotlyOutput("trend_plot", height = "400px")
                             )
                           ),
                         ),
                       ),
                     ),
                     nav_panel(
                       "State Comparison",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             "comp_year",
                             "Year",
                             choices = list(
                               "All" = 0,
                               "2019" = 2019,
                               "2020" = 2020,
                               "2021" = 2021,
                               "2022" = 2022
                             ),
                             selected = 0
                           ),
                           radioButtons(
                             "comp_measure",
                             "Select Comparison Measure",
                             choices = list(
                               "Sum of Crime Count" = "crime_ratio_to_sum",
                               "Mean Crime Count" = "crime_ratio_to_mean"
                             )
                           ),
                           selectInput(
                             "state_select",
                             "Select State",
                             choices = states,
                             selected = "JOHOR"
                           ),
                           selectInput("comp_type_select", "Select Crime Type",
                                       choices = types,
                                       selected = NULL,
                                       multiple = TRUE
                           ),
                           actionButton("comp_btn", "Update")
                         ),
                         mainPanel(
                           plotlyOutput("state_comparison",
                                        width = "95%",
                                        height = 580)
                         )
                       )
                     )
                   )
)

eda_server <- function(input, output) {
  # Update crime type choices based on selected category
  output$choro_type_select <- renderUI({
    if (input$choro_category_select != "All") {
      types <- unique(eda_sf$type[eda_sf$category == input$choro_category_select])
    } else {
      types <- unique(eda_sf$type)
    }
    
    selectizeInput(
        "choro_type_select",
        "Select Crime Type",
        choices = types,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = 'Select types or leave empty for all')
      )
  })
  # Update crime type choices based on selected category
  output$trend_type_select <- renderUI({
    if (input$trend_category_select != "All") {
      types <- unique(eda_sf$type[eda_sf$category == input$trend_category_select])
    } else {
      types <- unique(eda_sf$type)
    }
    
    selectizeInput("trend_type_select",
                   "Select Crime Type",
                   choices = types,
                   multiple = TRUE,
                   selected = NULL,
                   options = list(placeholder = 'Select types or leave empty for all'))
      
  })
  
  # Choropleth Plot
  choro_result <- eventReactive(input$choro_btn, {
    if (is.null(input$state_select)) {
      filtered_data <- eda_sf %>%
        filter(year %in% ifelse(input$eda_sel_year == 0, unique(year), input$eda_sel_year)) %>%
        filter(category %in% ifelse(input$choro_category_select == "All", unique(category), input$choro_category_select)) %>%
        filter(type %in% if (is.null(input$choro_type_select)) {types} else {input$choro_type_select})
    } else {
      filtered_data <- eda_sf %>%
        filter(year %in% ifelse(input$eda_sel_year == 0, unique(year), input$eda_sel_year)) %>%
        filter(state %in% if (is.null(input$state_select)) {states} else {input$state_select}) %>%
        filter(category %in% ifelse(input$choro_category_select == "All", unique(category), input$choro_category_select)) %>%
        filter(type %in% if (is.null(input$choro_type_select)) {types} else {input$choro_type_select})
    }
    
    return(list(
      data = filtered_data,
      measure = input$crime_measure
    ))
  })
  output$eda_choro_plot <- renderTmap({
    filtered_data <- choro_result()$data
    crime_measure <- choro_result()$measure
    
    # Filter data based on selections
    tm_shape(filtered_data) +
      tm_polygons(crime_measure,
                  palette = "Blues",
                  title = crime_measure,
                  tooltip = c("state", crime_measure)) +
      tm_layout(main.title = "Crime Distribution",
                legend.position = c("right", "bottom"))
    
  })
  
  output$trend_plot <- renderPlotly({
    base_data <- eda_sf %>%
      st_drop_geometry() %>%
      dplyr::select(state, year, category, type, crimes, crimes_pc)
    
    if (is.null(input$trend_state_select)) {
      filtered_data <- base_data %>%
        filter(year %in% ifelse(input$trend_sel_year == 0, unique(year), input$trend_sel_year)) %>%
        filter(category %in% ifelse(input$trend_category_select == "All", unique(category), input$trend_category_select)) %>%
        filter(type %in% if (is.null(input$trend_type_select)) {types} else {input$trend_type_select})
    } else {
      filtered_data <- base_data %>%
        filter(year %in% ifelse(input$trend_sel_year == 0, unique(year), input$trend_sel_year)) %>%
        filter(state %in% input$trend_state_select) %>%
        filter(category %in% ifelse(input$trend_category_select == "All", unique(category), input$trend_category_select)) %>%
        filter(type %in% if (is.null(input$trend_type_select)) {types} else {input$trend_type_select})
    }
    
    p <- ggplot(filtered_data, 
                aes(x = year, 
                    y = !!sym(input$trend_metric), 
                    color = state, 
                    group = state)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = paste("Crime Trends:", 
                         ifelse(input$trend_category_select == "All", 
                                "All Categories", 
                                input$trend_category_select)),
           x = "Year",
           y = ifelse(input$trend_metric == "crimes", 
                      "Total Crimes", 
                      "Crimes per Capita"),
           color = "State") +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  # Render choropleth map
  comp_result <- eventReactive(input$comp_btn, {
    filtered_data <- eda_sf %>%
      filter(year %in% ifelse(input$comp_year == 0, unique(year), input$comp_year)) %>%
      filter(state %in% input$state_select) %>%
      filter(type %in% if (is.null(input$comp_type_select)) {types} else {input$comp_type_select})
    
    p <- ggplot(filtered_data, aes(fill = get(input$comp_measure), geometry = geometry)) +
      geom_sf() +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      labs(title = "Crime Distribution",
           fill = input$comp_measure) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5))
    
    
    return(list(
      p = p,
      comp_measure = input$comp_measure
    ))       
  })
  
  output$state_comparison <- renderPlotly({
    p <- comp_result()$p
    comp_measure <- comp_result()$comp_measure
    ggplotly(p, tooltip = c("state", comp_measure))
  })
  
}
