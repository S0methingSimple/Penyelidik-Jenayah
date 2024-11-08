pacman::p_load(shiny, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes, 
               sf, st, tidyverse, raster, tmap, tmaptools, ggplot2, 
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
            "Crime Measure",
            choices = list(
              "Overall Crime Count" = "crimes",
              "Crime per Capita" = "crimes_pc"
            )
          ),
          selectInput(
            "state_select",
            "Select State",
            choices = states, # Will be updated in server
            selected = NULL
          ),
          selectInput(
            "choro_category_select",
            "Crime Category",
            choices = c("All", "assault", "property"),
            selected = "All"
          ),
          selectInput(
            "choro_type_select",
            "Crime Type",
            choices = NULL,
            multiple = TRUE,
            selected = NULL
          ),
        ),
        mainPanel(
          plotOutput("eda_choro_plot",
                     width = "95%",
                     height = 580)
        )
      )
    ),
    # Second Panel - Histogram Analysis
    nav_panel(
      "Histogram Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "hist_year",
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
            "hist_measure",
            "Crime Measure",
            choices = list(
              "Overall Crime Count" = "crimes",
              "Crime per Capita" = "crimes_pc"
            )
          ), 
          selectInput(
            "state_select",
            "Select State",
            choices = states, # Will be updated in server
            multiple = TRUE
          ),
          selectInput(
            "hist_category_select",
            "Crime Category",
            choices = c("All", "assault", "property"),
            selected = "All"
          ),
          selectInput(
            "hist_type_select",
            "Crime Type",
            choices = NULL,
            multiple = TRUE
          ),
        ),
        mainPanel(
          plotOutput("histogram_plot",
                     width = "95%",
                     height = 580)
        )
      )
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
            "Comparison Measure",
            choices = list(
              "Sum of Crime Count" = "crime_ratio_to_sum",
              "Mean Crime Count" = "crime_ratio_to_mean"
            )
          ),
          selectInput(
            "state_select",
            "Select State",
            choices = states,
            multiple = TRUE,
            selected = "JOHOR"
          ),
          selectInput("comp_type_select", "Select Crime Type:",
                      choices = types,
                      selected = NULL,
                      multiple = TRUE
                      ),
        ),
        mainPanel(
          plotOutput("state_comparison",
                     width = "95%",
                     height = 580)
        )
      )
    )
  )
)

eda_server <- function(input, output, session) {
  # Update crime type choices based on selected category
  observe({
    if (input$choro_category_select != "All") {
      types <- unique(eda_sf$type[eda_sf$category == input$choro_category_select])
    } else {
      types <- unique(eda_sf$type)
    }
    updateSelectInput(session, "choro_type_select",
                      choices = types)
  })
  # Update crime type choices based on selected category
  observe({
    if (input$hist_category_select != "All") {
      types <- unique(eda_sf$type[eda_sf$category == input$hist_category_select])
    } else {
      types <- unique(eda_sf$type)
    }
    updateSelectInput(session, "hist_type_select",
                      choices = types)
  })

  # Choropleth Plot
  output$eda_choro_plot <- renderPlot({
    # Filter data based on selections
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
    
    ggplot(filtered_data, aes(fill = get(input$crime_measure), geometry = geometry)) +
      geom_sf() +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      labs(title = "Crime Distribution",
           fill = input$crime_measure) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Comparison Plot and Table
  output$histogram_plot <- renderPlot({
      # Filter data based on selections
      filtered_data <- eda_sf %>%
        filter(year %in% ifelse(input$hist_year == 0, unique(year), input$hist_year)) %>%
        filter(category %in% ifelse(input$hist_category_select == "All", unique(category), input$hist_category_select))
    
    p <- ggplot(filtered_data, aes(x = get(input$hist_measure))) +
      geom_histogram(fill = "lightblue", color = "black") +
      labs(title = "Crime Distribution",
           x = input$hist_measure)
    p
  })
  
  # Render choropleth map
  output$state_comparison <- renderPlot({
    filtered_data <- eda_sf %>%
      filter(year %in% ifelse(input$comp_year == 0, unique(year), input$comp_year)) %>%
      filter(state %in% input$state_select) %>%
      filter(type %in% if (is.null(input$comp_type_select)) {types} else {input$comp_type_select})
    
    ggplot(filtered_data, aes(fill = get(input$comp_measure), geometry = geometry)) +
      geom_sf() +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      labs(title = "Crime Distribution",
           fill = input$comp_measure) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
}
