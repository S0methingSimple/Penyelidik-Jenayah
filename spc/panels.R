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
                          actionButton("spc_cc_btn", "Update"),
                          helpText(
                            "Create demographic maps with information from the 2010 US Census."
                          )
                        ),
                        mainPanel(
                          htmlOutput("spc_summary"),
                          plotOutput("spc_configOut")
                        )
                      )
)

cr_panel <- nav_panel("Cluster Result", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "spc_cr_sel_year",
                            "Year",
                            choices = list("All" = 0, "2019" = 2019, "2020" = 2020, "2021" = 2021, "2022" = 2022),
                            selected = 1
                          ),
                          radioButtons( 
                            "spc_cr_rad_style", 
                            "Style", 
                            choices = list("NB Clust" = 1, "Fviz" = 2),
                            selected = 1
                          ),
                          uiOutput("spc_cr_sel_mtd"),
                          actionButton("spc_cr_btn", "Update")
                        ),
                        mainPanel(
                          
                        )
                      )
)