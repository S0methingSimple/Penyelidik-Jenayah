pacman::p_load(shiny, bslib, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes,
               tmap, tmaptools, ggplot2, gridExtra, ClustGeo, 
               ggpubr, cluster, factoextra, NbClust,
               heatmaply, corrplot, psych, GGally)

spc_sf <- read_rds("data/spc/spc_sf.rds")
u_states <- unique(spc_sf$state)
cc_mtd <- list(
  c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"),
  c("silhouette", "wss", "gap_stat")
)

proc_df <- function(yr, states, norm, sf) {
  
  if (!is.null(states)) {
    spc_sf.fil <- spc_sf %>%
      filter(year == yr,
             state %in% states)
  } else {
    spc_sf.fil <- spc_sf %>%
      filter(year == yr)
  }
  
  spc_sf.fil <- spc_sf.fil %>%
    mutate(across(5:16, 
                  ~ if (norm == 2) {
                    (.x - min(.x)) / (max(.x) - min(.x))
                  } else if (norm == 3) {
                    scale(.x)
                  } else {
                    .
                  })) 
    
  if (!sf) {
    rns <- spc_sf.fil$district
    spc_sf.fil <- spc_sf.fil %>%
      select(-district, -state, -year) %>%
      st_drop_geometry()
    
    rownames(spc_sf.fil) <- rns
  }
  
  return(spc_sf.fil)
  
}

source("spc/panels.R")
spc_ui <- tabPanel("Spatial Clustering",
                   h1("Spatial Crime Clustering"), 
                   navset_card_pill( 
                     ve_panel, 
                     cc_panel, 
                     nav_panel("C", "Page C content")
                   )
)

spc_server <- function(input, output) {
  ve_func(input, output)
  cc_func(input, output)
  
}

ve_func <- function(input, output) {
  output$spc_explorePlot <- renderPlot({
    clust_vars <- proc_df(as.integer(input$spc_sel_year), input$spc_sel_states, input$spc_rad_std, FALSE)
    
    hist_list <- map(colnames(clust_vars), function(col) {
      if (input$spc_rad_type == 2) {
        ggplot(data = clust_vars, aes(x = .data[[col]])) +
          geom_density(color = "black", fill = "lightblue") +
          ggtitle(str_to_title(str_replace_all(col, "_", " ")))
      } else if (input$spc_rad_type == 3) {
        ggplot(data = clust_vars, aes(x = .data[[col]])) +
          geom_boxplot(color = "black", fill = "lightblue") +
          ggtitle(str_to_title(str_replace_all(col, "_", " ")))
      } else {
        ggplot(data = clust_vars, aes(x = .data[[col]])) +
          geom_histogram(color = "black", fill = "lightblue", bins = 20) +
          ggtitle(str_to_title(str_replace_all(col, "_", " ")))
      }
    })
    grid.arrange(grobs = hist_list, ncol = 4, nrow = 3)
  })
}

cc_func <- function(input, output) {
  
  output$spc_cc_sel_mtd <- renderUI({
    sel_style <- as.integer(input$spc_cc_rad_style) 
    selectInput(
      "spc_cc_sel_mtd",
      "Method",
      choices = cc_mtd[[sel_style]],
      selected = 1
    )
  })
  
  nb_clust_result <- eventReactive(input$spc_cc_btn, {
    
    set.seed(12345)
    clust_vars <- proc_df(as.integer(input$spc_cc_sel_year), NULL, 2, FALSE)
    sel_style <- as.integer(input$spc_cc_rad_style) 
    sel_mtd <- input$spc_cc_sel_mtd
    
    if (sel_style == 1) {
      sel_mtd <- sel_mtd %||% "ward.D"
      res <- NbClust(clust_vars, distance = "euclidean", min.nc = 5, max.nc = 10, method = sel_mtd)
      res$console_output <- paste(capture.output(NbClust(clust_vars, distance = "euclidean", min.nc = 5, max.nc = 10, method = sel_mtd))[1:24], collapse = "<br>")
    } else {
      sel_mtd <- sel_mtd %||% "silhouette"
      res <- fviz_nbclust(clust_vars, FUNcluster = hcut, method = sel_mtd, k.max = 10)
    }
    
    return(res)
  })
  
  output$spc_summary <- renderUI({
    sel_style <- as.integer(input$spc_cc_rad_style) 
    if (sel_style == 1) {
      HTML(nb_clust_result()$console_output)
    }
  })
  
  output$spc_configOut <- renderPlot({
    
    nbc <- nb_clust_result()
    sel_style <- as.integer(input$spc_cc_rad_style) 
    if (sel_style == 1) {
      row_data <- as.data.frame(nbc$Best.nc)["Number_clusters", ]
      row_data_long <- pivot_longer(row_data, cols = colnames(row_data)) %>%
        filter(value != 0)
      
      ggplot(row_data_long, aes(x = name, y = value, fill = value)) +
        geom_bar(stat = "identity") +
        labs(title = "Best Number of Clusters by Index",
             x = "Index",
             y = "Number of Clusters") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    } else {
      nbc
    }
    
    
  })
  
}



