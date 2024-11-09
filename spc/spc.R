pacman::p_load(shiny, bslib, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes,
               tmap, tmaptools, ggplot2, gridExtra, ClustGeo, 
               ggpubr, cluster, factoextra, NbClust,
               heatmaply, corrplot, psych, GGally, spdep)

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
      st_drop_geometry() %>%
      rownames_to_column(var = "row_name") %>%
      mutate(row_name = rns) %>%
      column_to_rownames(var = "row_name")
  }
  
  return(spc_sf.fil)
  
}

source("spc/panels.R")
spc_ui <- tabPanel("Spatial Clustering",
                   h1("Spatial Crime Clustering"), 
                   navset_card_pill( 
                     ve_panel, 
                     cc_panel, 
                     cr_panel
                   )
)

spc_server <- function(input, output) {
  ve_func(input, output)
  cc_func(input, output)
  cr_func(input, output)
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
  
  output$spc_cc_sel_minc <- renderUI({
    sel_style <- as.integer(input$spc_cc_rad_style) 
    
    if (sel_style == 1) {
      sliderInput("spc_cc_sel_minc", "Min Cluster", min = 1, max = 6, value = 5)
    }
  })
  
  nb_clust_result <- eventReactive(input$spc_cc_btn, {
    
    set.seed(12345)
    clust_vars <- proc_df(as.integer(input$spc_cc_sel_year), NULL, 2, FALSE)
    sel_style <- as.integer(input$spc_cc_rad_style) 
    sel_mtd <- input$spc_cc_sel_mtd
    
    if (sel_style == 1) {
      sel_mtd <- sel_mtd %||% "ward.D"
      res <- NbClust(clust_vars, distance = "euclidean", min.nc = 5, max.nc = 10, method = sel_mtd)
      res$console_output <- paste(capture.output(NbClust(clust_vars, distance = "euclidean", min.nc = input$spc_cc_sel_minc, max.nc = input$spc_cc_sel_maxc, method = sel_mtd))[11:24], collapse = "<br>")
      res$sel_style = sel_style
    } else {
      sel_mtd <- sel_mtd %||% "silhouette"
      res <- list(
        plot = fviz_nbclust(clust_vars, FUNcluster = hcut, method = sel_mtd, k.max = input$spc_cc_sel_maxc),
        sel_style = sel_style
      )
    }
    
    return(res)
  })
  
  output$spc_summary <- renderUI({
    nbc <- nb_clust_result()
    if (nbc$sel_style == 1) {
      HTML(nbc$console_output)
    }
  })
  
  output$spc_configOut <- renderPlot({
    
    nbc <- nb_clust_result()
    if (nbc$sel_style == 1) {
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
      nbc$plot
    }
    
  })
  
}

cr_func <- function(input, output) {
  
  hc_result <- eventReactive(input$spc_cr_hc_btn, get_clust(input, output, 1))
  output$spc_cr_hc <- renderTmap(tmap_plot(hc_result()$cluster))
  output$spc_cr_hc_dd <- renderPlot(dendo_plot(hc_result()$dendo))
  
  hg_result <- eventReactive(input$spc_cr_hg_btn, get_clust(input, output, 2))
  output$spc_cr_hg <- renderTmap(tmap_plot(hg_result()$cluster))
  output$spc_cr_hg_dd <- renderPlot(dendo_plot(hg_result()$dendo))
  
  sk_result <- eventReactive(input$spc_cr_sk_btn, get_clust(input, output, 3))
  output$spc_cr_sk <- renderTmap(tmap_plot(sk_result()$cluster))
  
}

get_clust <- function(input, output, type) {
  clust_vars <- proc_df(as.integer(input$spc_cr_sel_year), NULL, 2, FALSE)
  clust_sf <- proc_df(as.integer(input$spc_cr_sel_year), NULL, 2, TRUE)
  n_clust <- input$spc_cr_sel_nc
  
  if (type <= 2) {
    proxmat <- dist(clust_vars, method = 'euclidean')
    if (type == 1) {
      clust = hclust(proxmat, method = input$spc_cr_hc_sel_mtd)
    } else { # Geo
      distmat <- as.dist(st_distance(clust_sf, clust_sf))
      clust <- hclustgeo(proxmat, distmat, alpha = input$spc_cr_hg_sel_alp)
    }
    
    groups <- as.factor(cutree(clust, k=n_clust))
  } else {
    nb <- map_nb(poly2nb(clust_sf))
    clust_vars.w <- nb2listw(nb, nbcosts(nb, clust_vars), style=input$spc_cr_sk_sel_sty)
    clust_vars.mst <- mstree(clust_vars.w)
    clust <- spdep::skater(edges = clust_vars.mst[,1:2], data = clust_vars, method = "euclidean", ncuts = (n_clust - 1))
    
    groups <- as.factor(clust$groups)
  }
  
  cluster <- cbind(clust_sf, groups) %>% rename(`CLUSTER`=`groups`)
  return(list(
    cluster = cluster,
    dendo = list(
      clust = clust, 
      n_clust = n_clust
    )
  ))
  
}

tmap_plot <- function(cluster, view = TRUE, title = "Clustering Result") {
  map <- tm_shape(cluster) +
    tm_fill("CLUSTER", 
            palette = "Set3") +
    tm_layout(main.title = title,
              main.title.position = "center",
              main.title.size = 1.2,
              legend.height = 0.45, 
              legend.width = 0.35,
              frame = TRUE) +
    tm_borders(alpha = 0.5) +
    tm_grid(alpha =0.2)
  
  if (view) {
    map + tm_view(set.zoom.limits = c(6, 7))
  }
  
  return(map)
  
}

dendo_plot <- function(dendo) {
  plot(dendo$clust, cex = 0.6)
  rect.hclust(clust, 
              k = dendo$n_clust, 
              border = 2:5)
}

map_nb <- function(nb) {
  nb[[17]] <- as.integer(c(13, 66, 71))
  nb[[13]] <- c(nb[[13]], as.integer(17))
  nb[[66]] <- c(nb[[66]], as.integer(17))
  nb[[71]] <- c(nb[[71]], as.integer(17))
  nb[[67]] <- c(nb[[67]], as.integer(68))
  nb[[68]] <- c(nb[[68]], as.integer(67))
  nb[[71]] <- c(nb[[71]], as.integer(70))
  nb[[70]] <- c(nb[[70]], as.integer(71))
  nb
}

