pacman::p_load(shiny, bslib, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes,
               tmap, tmaptools, ggplot2, gridExtra, ClustGeo, 
               ggpubr, cluster, factoextra, NbClust,
               heatmaply, corrplot, psych, GGally, spdep)

spc_sf <- read_rds("data/spc/spc_sf.rds")
u_states <- unique(spc_sf$state)
cc_mtd <- list(
  c("gap_stat", "silhouette", "wss"),
  c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans")
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
    
    if (input$spc_rad_type != 4) {
      grid.arrange(grobs = hist_list, ncol = 4, nrow = 3)
    } else {
      corrplot.mixed(cor(clust_vars),
                     lower = "ellipse", 
                     upper = "number",
                     tl.pos = "lt",
                     diag = "l",
                     tl.col = "black",
                     tl.srt = 45,  
                     tl.cex = 0.5)
    }
    
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
      sel_mtd <- sel_mtd %||% "gap_stat"
      res <- list(
        plot = fviz_nbclust(clust_vars, FUNcluster = hcut, method = sel_mtd, k.max = input$spc_cc_sel_rng[2]),
        sel_style = sel_style
      )
    } else {
      sel_mtd <- sel_mtd %||% "ward.D"
      min_rng <- input$spc_cc_sel_rng[1]
      max_rng <- input$spc_cc_sel_rng[2]
      #dev.off()
      res <- NbClust(clust_vars, distance = "euclidean", min.nc = 5, max.nc = 10, method = sel_mtd)
      res$console_output <- paste(sprintf("<h2>Cluster Indicies Vote (K:%d-%d)</h2>", min_rng, max_rng),
                                  paste(capture.output(
                                    NbClust(
                                      clust_vars, 
                                      distance = "euclidean", 
                                      min.nc = min_rng, 
                                      max.nc = max_rng, 
                                      method = sel_mtd), 
                                    file = NULL, 
                                    type = "output")[11:24], 
                                    collapse = "<br>")
                                  )
      res$sel_style = sel_style
    }
    
    return(res)
  })
  
  output$spc_summary <- renderUI({
    nbc <- nb_clust_result()
    if (nbc$sel_style == 2) {
      HTML(nbc$console_output)
    }
  })
  
  output$spc_configOut <- renderPlot({
    
    nbc <- nb_clust_result()
    if (nbc$sel_style == 1) {
      nbc$plot
    } else {
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
    }
    
  })
  
}

cr_func <- function(input, output) {
  
  hm_result <- eventReactive(input$spc_cr_hm_btn, get_clust(input, output, 1))
  output$spc_cr_hm <- renderPlotly(heatmaply_plot(hm_result()$heatmap))
  
  hc_result <- eventReactive(input$spc_cr_hc_btn, get_clust(input, output, 1))
  output$spc_cr_hc <- renderTmap(tmap_plot(hc_result()$cluster, hc_result()$palette))
  output$spc_cr_hc_dd <- renderPlot(subchart_plot(hc_result()$sub_plot))
  
  hg_result <- eventReactive(input$spc_cr_hg_btn, get_clust(input, output, 2))
  output$spc_cr_hg <- renderTmap(tmap_plot(hg_result()$cluster, hg_result()$palette))
  output$spc_cr_hg_dd <- renderPlot(subchart_plot(hg_result()$sub_plot))
  
  sk_result <- eventReactive(input$spc_cr_sk_btn, get_clust(input, output, 3))
  output$spc_cr_sk <- renderTmap(tmap_plot(sk_result()$cluster, sk_result()$palette))
  output$spc_cr_sk_dd <- renderPlot(subchart_plot(sk_result()$sub_plot))
  
  crc_result <- eventReactive(input$spc_crc_btn, get_clust(input, output, 4))
  output$spc_crc <- renderPlot(compare_plot(crc_result()$cluster, crc_result()$palette))
  
}

get_clust <- function(input, output, type) {
  clust_vars <- proc_df(as.integer(input$spc_cr_sel_year), NULL, 2, FALSE)
  clust_sf <- proc_df(as.integer(input$spc_cr_sel_year), NULL, 2, TRUE)
  n_clust <- input$spc_cr_sel_nc
  
  # Return vars
  clust.sf <- NULL
  heatmap <- NULL
  groups <- NULL
  groups_list <- list()
  sel_style <- 1
  comp <- (type == 4) 
  
  # Return vars
  hc_mtd <- if (comp) input$spc_crc_hc_sel_mtd else input$spc_cr_hc_sel_mtd
  hc_alp <- if (comp) input$spc_crc_hg_sel_alp else input$spc_cr_hg_sel_alp
  sk_sty <- if (comp) input$spc_crc_sk_sel_sty else input$spc_cr_sk_sel_sty
  
  if (type == 1 || comp) {
    proxmat <- dist(clust_vars, method = 'euclidean')
    clust = hclust(proxmat, method = hc_mtd)
    sel_style <- input$spc_cr_hc_rad_sty
    heatmap <- list(
      clust_vars = clust_vars,
      n_clust = n_clust,
      sel_mtd = input$spc_cr_hm_sel_mtd
    )
    groups <- as.factor(cutree(clust, k=n_clust))
    if (comp) {
      clust.sf$hc <- cbind(clust_sf, groups) %>% rename(`CLUSTER`=`groups`)
    }
  } 
  
  if (type == 2 || comp) {
    proxmat <- dist(clust_vars, method = 'euclidean')
    distmat <- as.dist(st_distance(clust_sf, clust_sf))
    clust <- hclustgeo(proxmat, distmat, alpha = hc_alp)
    sel_style <- input$spc_cr_hg_rad_sty
    groups <- as.factor(cutree(clust, k=n_clust))
    if (comp) {
      clust.sf$hg <- cbind(clust_sf, groups) %>% rename(`CLUSTER`=`groups`)
    }
  }
  
  if  (type == 3 || comp) { 
    set.seed(12345)
    suppressWarnings(nb <- map_nb(poly2nb(clust_sf)))
    clust_vars.w <- nb2listw(nb, nbcosts(nb, clust_vars), style = sk_sty)
    clust_vars.mst <- mstree(clust_vars.w)
    clust <- spdep::skater(edges = clust_vars.mst[,1:2], data = clust_vars, method = "euclidean", ncuts = (n_clust - 1))
    sel_style <- input$spc_cr_sk_rad_sty
    groups <- as.factor(clust$groups)
    if (comp) {
      clust.sf$sk <- cbind(clust_sf, groups) %>% rename(`CLUSTER`=`groups`)
    }
  }
    
  if (!comp) {
    clust.sf <- cbind(clust_sf, groups) %>% rename(`CLUSTER`=`groups`)
  }
  
  return(list(
    cluster = clust.sf,
    palette = input$spc_cr_sel_pal,
    heatmap = heatmap,
    sub_plot = list(
      sel_style = sel_style,
      clust = clust, 
      clust.sf = clust.sf,
      n_clust = n_clust
    )
  ))
  
}

compare_plot <- function(clusters, palette) {
  hc_plot <- tmap_plot(clusters$hc, palette, view = FALSE, title = "Hierarchical Cluster")
  hg_plot <- tmap_plot(clusters$hg, palette, view = FALSE, title = "Hierarchical (GEO) Cluster")
  sk_plot <- tmap_plot(clusters$sk, palette, view = FALSE, title = "Skater Cluster")
  tmap_arrange(hc_plot, hg_plot, sk_plot, ncol = 3)
}

tmap_plot <- function(cluster, palette, view = TRUE, title = "Clustering Result") {
  map <- tm_shape(cluster) +
    tm_fill("CLUSTER", 
            palette = palette) +
    tm_layout(main.title = title,
              main.title.position = "center",
              main.title.size = 1,
              legend.height = 0.45, 
              legend.width = 0.35,
              frame = TRUE) +
    tm_borders(alpha = 0.5) +
    tm_grid(alpha =0.2)
  
  if (view) {
    map + tm_view(set.zoom.limits = c(6, 7))
  } else {
    map + tm_compass(type="8star", size = 2, position = c("right", "top"))
  }
  
  return(map)
  
}

subchart_plot <- function(sub_plot) {
  sel_style = sub_plot$sel_style
  if (sel_style == 1) {
    ggparcoord(data = sub_plot$clust.sf,
               columns = c(4:10), 
               scale = "globalminmax",
               alphaLines = 0.2,
               boxplot = TRUE, 
               title = "Parallel Coordinates of Crime type by Cluster") +
      facet_grid(~ CLUSTER) + 
      theme(axis.text.x = element_text(angle = 90))
    
  } else if (sel_style < 4) {
    
    cluster_prof <- sub_plot$clust.sf %>% 
      st_drop_geometry() %>%
      group_by(CLUSTER) %>%
      summarise(mean_causing_injury = mean(causing_injury),
                mean_murder = mean(murder),
                mean_rape = mean(rape),
                mean_robbery = mean(robbery),
                mean_break_in = mean(break_in),
                mean_theft_other = mean(theft_other),
                mean_vehicle_theft = mean(vehicle_theft))
    
    if (sel_style == 2) {
      ggplot(cluster_prof %>% pivot_longer(-CLUSTER, names_to = "metric", values_to = "value"), 
             aes(x = CLUSTER, y = value, fill = CLUSTER)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~ metric, scales = "free_y", ncol = 4) +  
        ggtitle("Crime type Cluster Profile") +
        theme_minimal()
    } else {
      ggplot(cluster_prof %>% pivot_longer(cols = starts_with("mean_"), names_to = "variable", values_to = "value"), 
             aes(x = variable, y = CLUSTER, fill = value)) +
        geom_tile() +
        geom_text(aes(label = value), color = "black", size = 1) +
        scale_fill_gradient(low = "yellow", high = "red") +
        labs(title = "Crime type Cluster Heatmap",
             x = "Crime Indicators",
             y = "Clusters") +
        theme_minimal()
    }
    
  } else {
    plot(sub_plot$clust, cex = 0.6)
    rect.hclust(sub_plot$clust, 
                k = sub_plot$n_clust, 
                border = 2:5)
  }
}

heatmaply_plot <- function(heatmap) {
  heatmaply(normalize(data.matrix(heatmap$clust_vars)),
            Colv=NA,
            dist_method = "euclidean",
            hclust_method = heatmap$sel_mtd,
            seriate = "OLO",
            colors = OrRd,
            k_row = heatmap$n_clust,
            margins = c(NA,50,50,NA),
            fontsize_row = 4,
            fontsize_col = 5,
            main="Crime Indicators Heatmap",
            xlab = "Crime and Demographic Indicators",
            ylab = "Districts"
  )
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

