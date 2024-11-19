pacman::p_load(shiny, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes, 
               sf, st, tidyverse, raster, tmap, tmaptools, ggplot2, plotly,
               spatstat, sfdep, ClustGeo, ggpubr, cluster, factoextra, 
               NbClust, heatmaply, corrplot, psych, tidyverse, GGally)

eda_sf <- read_rds("data/eda/eda.rds")
comp_eda_sf <- read_rds("data/eda/comp_eda.rds")
piv_eda_sf <- read_rds("data/eda/piv_eda.rds")
piv_pc_eda_sf <- read_rds("data/eda/piv_pc_eda.rds")
states <- unique(piv_eda_sf$state)
types <- c("causing_injury", "murder", "rape", "robbery", "break_in", "theft_other", "vehicle_theft" )
category <- unique(eda_sf$category)

eda_ui <- tabPanel("Exploratory Data Analysis",
                   h1("Visualising Distributions of Crime"), 
                   helpText("This dashboard allows you to explore crime patterns across different states, years, and crime types through three different analytical views."),
                   navset_card_pill( 
                     # First Panel - Choropleth Analysis
                     nav_panel(
                       "Choropleth Analysis",
                       helpText("View crime distribution across states using color-coded maps. Darker colors indicate higher crime rates."),
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
                           selectizeInput(
                             "eda_state_select",
                             "States",
                             choices = states, # Will be updated in server
                             selected = NULL,
                             multiple = TRUE,
                             options = list(placeholder = 'Select states or leave empty for all')
                           ),
                           
                           selectInput(
                             "choro_category_select",
                             "Crime Category",
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
                       helpText("Analyze how crime patterns change over time with interactive trend lines."),
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
                           radioButtons("trend_metric", "Metric",
                                        choices = c("Total Crimes" = "crimes",
                                                    "Crimes per Capita" = "crimes_pc"),
                                        selected = "crimes"),
                           
                           # State selection
                           selectizeInput("trend_state_select", "States",
                                          choices = states,
                                          multiple = TRUE,
                                          selected = NULL,
                                          options = list(placeholder = 'Select states or leave empty for all')),
                           
                           # Category selection
                           selectInput("trend_category_select", "Crime Category",
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
                       helpText("Compare crime statistics of one state against others using relative measures."),
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
                             "comp_state_select",
                             "State",
                             choices = states,
                             selected = "JOHOR"
                           ),
                           selectInput("comp_type_select", "Crime Type",
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
        "Crime Type",
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
                   "Crime Type",
                   choices = types,
                   multiple = TRUE,
                   selected = NULL,
                   options = list(placeholder = 'Select types or leave empty for all'))
      
  })
  
  # Choropleth Plot
  choro_result <- eventReactive(input$choro_btn, {
    measure = input$crime_measure
    sel_sf <- if (measure == "crimes") {piv_eda_sf} else {piv_pc_eda_sf}
    sel_types <- if (is.null(input$choro_type_select)) {types} else {input$choro_type_select}
    if (is.null(input$eda_state_select)) {
      filtered_data <- sel_sf %>%
        mutate(crimes = rowSums(st_drop_geometry(sel_sf)[, sel_types], na.rm = TRUE)) %>%
        dplyr::select(-all_of(types)) %>%
        filter(year %in% ifelse(input$eda_sel_year == 0, unique(year), input$eda_sel_year))
    } else {
      filtered_data <- sel_sf %>%
        mutate(crimes = rowSums(st_drop_geometry(sel_sf)[, sel_types], na.rm = TRUE)) %>%
        dplyr::select(-all_of(types)) %>%
        filter(year %in% ifelse(input$eda_sel_year == 0, unique(year), input$eda_sel_year)) %>%
        filter(state %in% if (is.null(input$eda_state_select)) {states} else {input$eda_state_select})
    }
    
    return(list(
      data = filtered_data,
      measure = measure
    ))
  })
  output$eda_choro_plot <- renderTmap({
    #tmap_mode("plot")
    filtered_data <- choro_result()$data
    crime_measure <- choro_result()$measure
    
    # Filter data based on selections
    tm_shape(filtered_data) +
      tm_polygons("crimes",
                  palette = "Blues",
                  title = crime_measure) +
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
    
    # Calculate summary statistics for each state-year combination
    summarized_data <- filtered_data %>%
      group_by(state, year) %>%
      summarise(
        total_crimes = sum(!!sym(input$trend_metric), na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Create the base plot with improved aesthetics
    p <- ggplot(summarized_data, 
                aes(x = year, 
                    y = total_crimes, 
                    color = state, 
                    group = state,
                    text = paste("State:", state,
                                 "<br>Year:", year,
                                 "<br>Value:", round(total_crimes, 2)))) +
      geom_line(size = 1.2) +
      geom_point(size = 4, shape = 21, fill = "white") +
      scale_x_continuous(breaks = unique(summarized_data$year)) +
      scale_y_continuous(labels = scales::comma_format()) +
      scale_color_viridis_d(option = "turbo") +  # Better color palette
      labs(title = paste("Crime Trends:", 
                         ifelse(input$trend_category_select == "All", 
                                "All Categories", 
                                input$trend_category_select)),
           subtitle = paste("Showing", ifelse(input$trend_metric == "crimes", 
                                              "Total Crimes", 
                                              "Crimes per Capita")),
           x = "Year",
           y = ifelse(input$trend_metric == "crimes", 
                      "Total Crimes", 
                      "Crimes per Capita"),
           color = "State") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      )
    
    # Convert to plotly with improved tooltip and layout
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          xanchor = "left",
          x = 1.02,
          y = 0.9
        ),
        margin = list(r = 100),  # Add margin for legend
        hovermode = "x unified"
      ) %>%
      config(displayModeBar = FALSE)  # Remove the plotly toolbar
  })
  
  comp_result1 <- eventReactive(input$comp_btn, {
    filtered_data <- comp_eda_sf %>%
      filter(year %in% ifelse(input$comp_year == 0, unique(year), input$comp_year)) %>%
      filter(state %in% input$state_select) %>%
      filter(type %in% if (is.null(input$comp_type_select)) {types} else {input$comp_type_select})
    
    p <- ggplot(filtered_data) +
      geom_sf(aes(fill = !!sym(input$comp_measure))) +  # Use !!sym() to properly reference the column
      scale_fill_distiller(palette = "Blues", direction = 1) +
      labs(title = "Crime Distribution",
           fill = ifelse(input$comp_measure == "crime_ratio_to_sum", 
                         "Ratio to State Sum", 
                         "Ratio to State Mean")) +
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
  
  output$state_comparison1 <- renderPlotly({
    result <- comp_result()
    p <- result$p
    comp_measure <- result$comp_measure
    
    ggplotly(p, tooltip = c("state", comp_measure)) %>%
      style(hoveron = "fills")  # Ensure hover works on filled regions
  })
  
  comp_result <- eventReactive(input$comp_btn, {
    selected_types <- if (is.null(input$comp_type_select)) {
      types # Use all types if none selected
    } else {
      input$comp_type_select
    }
    
    filtered_data <- if (input$comp_year != 0) {
      piv_eda_sf %>% filter(year == input$comp_year)
    } else {
      piv_eda_sf
    }
    
    filtered_data <- filtered_data %>%
    filter(state %in% input$comp_state_select)
    
    if (input$comp_measure == "crime_ratio_to_sum") {
      processed_data <- filtered_data %>%
        mutate(national_total = rowSums(across(all_of(selected_types)), na.rm = TRUE)) %>%
        group_by(year) %>%
        mutate(national_total = sum(national_total, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(state, district, year) %>%
        mutate(
          district_sum = rowSums(across(all_of(selected_types)), na.rm = TRUE),
          crime_ratio = district_sum / national_total
        ) %>%
        ungroup()
    } else {
      processed_data <- filtered_data %>%
        mutate(national_mean = rowSums(across(all_of(selected_types) / length(selected_types)), na.rm = TRUE)) %>%
        group_by(year) %>%
        mutate(national_total = sum(national_total, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(state, district, year) %>%
        mutate(
          district_mean = rowSums(across(all_of(selected_types) / length(selected_types)), na.rm = TRUE),
          crime_ratio = district_mean / national_mean
        ) %>%
        ungroup()
    }
    
    plot_data <- processed_data %>%
      dplyr::select(state, district, year, geometry, crime_ratio)
    
    p <- ggplot() +
      geom_sf(data = plot_data, 
              aes(geometry = geometry,
                  fill = crime_ratio)) +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      labs(title = paste("Crime Distribution -", 
                         paste(selected_types, collapse = ", ")),
           fill = ifelse(input$comp_measure == "crime_ratio_to_sum", 
                         "Ratio to State Sum", 
                         "Ratio to State Mean")) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5))
    
    return(list(
      p = p,
      data = plot_data,
      comp_measure = input$comp_measure
    ))
  })
  
  output$state_comparison <- renderPlotly({
    result <- comp_result()
    
    p <- result$p +
      aes(text = paste("District:", district,
                       "<br>Crime Ratio:", round(crime_ratio, 6)))
    
    plot <- ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12)
        )
      )
    
    if (result$comp_measure == "crime_ratio_to_sum") {
      plot <- plot %>%
        layout(
          title = list(
            text = "Crime Distribution (Ratio to State Sum)",
            x = 0.5
          )
        )
    } else {
      plot <- plot %>%
        layout(
          title = list(
            text = "Crime Distribution (Ratio to State Mean)",
            x = 0.5
          )
        )
    }
    
    plot %>%
      layout(
        margin = list(l = 50, r = 50, t = 50, b = 50),
        font = list(family = "Arial"),
        showlegend = TRUE,
        legend = list(
          title = list(
            text = if(result$comp_measure == "crime_ratio_to_sum") 
              "Ratio to State Sum" 
            else 
              "Ratio to State Mean"
          ),
          x = 1,
          y = 0.5
        )
      ) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = list(
          "zoomIn2d", "zoomOut2d", "autoScale2d",
          "hoverClosestCartesian", "hoverCompareCartesian"
        ),
        displaylogo = FALSE
      )
  })
}
