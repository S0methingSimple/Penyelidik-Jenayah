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
                          uiOutput("spc_cc_sel_minc"),
                          sliderInput("spc_cc_sel_maxc", "Max Cluster", min = 8, max = 15, value = 10), 
                          actionButton("spc_cc_btn", "Update"),
                          helpText(
                            "Create demographic maps with information from the 2010 US Census."
                          )
                        ),
                        mainPanel(
                          plotOutput("spc_configOut"),
                          htmlOutput("spc_summary")
                        )
                      )
)

cr_panel <- nav_panel("Cluster Result", 
    wellPanel(
      fluidRow(column(width = 12, h5("Cluster Filter"))),
      hr(),
      fluidRow(
        column(width = 4, selectInput("spc_cr_sel_year", "Year", choices = list("All" = 0, "2019" = 2019, "2020" = 2020, "2021" = 2021, "2022" = 2022), selected = 1)),
        column(width = 4, sliderInput("spc_cr_sel_nc", "No. Cluster", min = 3, max = 15, value = 7))
      )
    ),
    navset_tab( 
      nav_panel("Hierarchical", 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "spc_cr_hc_sel_mtd",
                       "Method",
                       choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                       selected = 1
                     ),
                     actionButton("spc_cr_hc_btn", "Update")
                   ),
                   mainPanel(
                     tmapOutput("spc_cr_hc")
                   )
                 )
      ), 
      nav_panel("Hierarchical (Geo)", 
                sidebarLayout(
                  sidebarPanel(
                    actionButton("spc_cr_hg_btn", "Update")
                  ),
                  mainPanel(
                    tmapOutput("spc_cr_hg")
                  )
                )
      ), 
      nav_panel("Skater", 
                sidebarLayout(
                  sidebarPanel(
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
  
  