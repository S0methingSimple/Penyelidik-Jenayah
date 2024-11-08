pacman::p_load(shiny, bslib, sf, tmap, tidyverse, sfdep, shinydashboard, shinythemes,
               tmap, tmaptools, ggplot2, gridExtra, ClustGeo, 
               ggpubr, cluster, factoextra, NbClust,
               heatmaply, corrplot, psych, GGally)

spc_sf <- read_rds("data/spc/spc_sf.rds")
spc_sf.all <- spc_sf %>% filter(year == 0)
clust_rownames <- spc_sf.all$state_district

clust_vars <- spc_sf.all %>% 
  select(-state_district, -state, -district, -year) %>% 
  st_drop_geometry()

clust_vars.norm <- clust_vars %>% normalize()

rownames(clust_vars) <- clust_rownames
rownames(clust_vars.norm) <- clust_rownames

spc_ui <- tabPanel("Spatial Clustering",
                   h1("Spatial Crime Clustering"), 
                   navset_card_pill( 
                     nav_panel("Exploratory Data Analysis", 
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(
                                     "spc_sel_year",
                                     "Year",
                                     choices = list("All" = 0, "2019" = 2019, "2020" = 2020, "2021" = 2021, "2022" = 2022),
                                     selected = 1
                                   ),
                                   radioButtons( 
                                     "spc_rad_type", 
                                     "Plot Type", 
                                     choices = list( 
                                       "Histogram" = 1, 
                                       "Density" = 2, 
                                       "Boxplot" = 3 
                                     ) 
                                   )
                                 ),
                                 mainPanel(
                                   plotOutput("spc_explorePlot",
                                              width = "95%", 
                                              height = 580)
                                   
                                 )
                               )
                     ), 
                     nav_panel("B", "Page B content"), 
                     nav_panel("C", "Page C content")
                   )
)

spc_server <- function(input, output) {
  output$spc_explorePlot <- renderPlot({
    
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
