ve_panel <- nav_panel("Variable Exploration", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "spc_sel_year",
                            "Year",
                            choices = list("All" = 0, "2019" = 2019, "2020" = 2020, "2021" = 2021, "2022" = 2022),
                            selected = 1
                          ),
                          selectInput(
                            "spc_sel_states",
                            "State",
                            choices = u_states,
                            selected = NULL,
                            selectize = TRUE,
                            multiple = TRUE 
                          ),
                          radioButtons( 
                            "spc_rad_type", 
                            "Plot Type", 
                            choices = list( 
                              "Histogram" = 1, 
                              "Density" = 2, 
                              "Boxplot" = 3 
                            )
                          ),
                          radioButtons( 
                            "spc_rad_std", 
                            "Data Standardization", 
                            choices = list( 
                              "None" = 1, 
                              "Min-Max" = 2, 
                              "Z-Score" = 3 
                            )
                          )
                        ),
                        mainPanel(
                          plotOutput("spc_explorePlot",
                                     width = "95%", 
                                     height = 700)
                        )
                      )
)

cc_panel <- nav_panel("Cluster Configuration", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "spc_cc_sel_year",
                            "Year",
                            choices = list("All" = 0, "2019" = 2019, "2020" = 2020, "2021" = 2021, "2022" = 2022),
                            selected = 1
                          ),
                          radioButtons( 
                            "spc_cc_rad_style", 
                            "Style", 
                            choices = list("NB Clust" = 1, "Fviz" = 2),
                            selected = 1
                          ),
                          uiOutput("spc_cc_sel_mtd"),
                          sliderInput("spc_cc_sel_rng", "Cluster Range", min = 3, max = 15, value = c(5, 12)), 
                          actionButton("spc_cc_btn", "Update"),
                          helpText(
                            "Create demographic maps with information from the 2010 US Census."
                          )
                        ),
                        mainPanel(
                          htmlOutput("spc_summary"),
                          hr(),
                          plotOutput("spc_configOut")
                        )
                      )
)

cr_panel <- nav_panel("Cluster Result", 
    wellPanel(
      fluidRow(
        column(width = 4, selectInput("spc_cr_sel_year", "Year", choices = list("All" = 0, "2019" = 2019, "2020" = 2020, "2021" = 2021, "2022" = 2022), selected = 1)),
        column(width = 8, sliderInput("spc_cr_sel_nc", "No. Cluster", min = 3, max = 15, value = 7))
      ),
      hr()
    ),
    navset_tab( 
      nav_panel("Heatmap", 
                sidebarLayout(
                  sidebarPanel(
                    selectInput("spc_cr_hm_sel_mtd", "Method", choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), selected = 1),
                    actionButton("spc_cr_hm_btn", "Update")
                  ),
                  mainPanel(
                    plotlyOutput("spc_cr_hm")
                  )
                )
      ),
      nav_panel("Hierarchical", 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("spc_cr_hc_sel_mtd", "Method", choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), selected = 1),
                     actionButton("spc_cr_hc_btn", "Update")
                   ),
                   mainPanel(
                     tmapOutput("spc_cr_hc"),
                     plotOutput("spc_cr_hc_dd")
                   )
                 )
      ), 
      nav_panel("Hierarchical (Geo)", 
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("spc_cr_hg_sel_alp", "Choice Alpha", min = 0.0, max = 1.0, value = 0.5),
                    actionButton("spc_cr_hg_btn", "Update")
                  ),
                  mainPanel(
                    tmapOutput("spc_cr_hg"),
                    plotOutput("spc_cr_hg_dd")
                  )
                )
      ), 
      nav_panel("Skater", 
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      "spc_cr_sk_sel_sty",
                      "Style",
                      choices = c("W", "B", "C", "U", "S"),
                      selected = 1
                    ),
                    actionButton("spc_cr_sk_btn", "Update")
                  ),
                  mainPanel(
                    tmapOutput("spc_cr_sk")
                  )
                )
      ), 
      nav_panel("Comparison", "Page C content") 
  
  )
)
  
  