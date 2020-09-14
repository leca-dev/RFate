
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-cogs'></i> Run simulation</span>")
         , value = "panel3"
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = panel.style,
             withMathJax(),
             
             br(),
             fluidRow(
               column(5
                      , div(id = "help3_1"
                            , directoryInput(inputId = "run.folder.simul"
                                             , label = param.style("Select the simulation folder :")
                                             , value = '~')
                      )
               )
               , column(5
                        , div(id = "help3_2"
                              , shinyjs::disabled(
                                selectInput(inputId = "run.simulParam"
                                            , label = param.style("Select the simulation parameters file :")
                                            , choices = NULL
                                            , selected = NULL
                                            , multiple = F
                                            , width = "100%")
                              )
                        )
               )
               , column(2
                        , br()
                        , actionButton(inputId = "run"
                                       , label = "Run"
                                       , icon = icon("play-circle")
                                       , width = "100%"
                                       , style = button.style.help
                        ) %>% helper(type = "inline"
                                     , title = "Run a FATE simulation"
                                     , size = "l"
                                     , colour = "#e0dbd9"
                                     , content = help.full(param.name.vec = c("<hr/>"
                                                                              , "name.simulation"
                                                                              , "file.simulParam"
                                                                              , "name.FATE_executable")
                                                           , param.desc.vec = c("<hr/>"
                                                                                , "a <span style='font-family:Monospace;'>string</span> that corresponds to
                                                                                the main directory or simulation name of the <span style='font-family:Monospace;'>FATE</span> simulation"
                                                                                , "a <span style='font-family:Monospace;'>string</span> that corresponds to
                                                                                the name of a parameter file that will be contained into the <span style='font-family:Monospace;'>PARAM_SIMUL</span>
                                                                                folder of the <span style='font-family:Monospace;'>FATE</span> simulation"
                                                                                , "a <span style='font-family:Monospace;'>string</span> that corresponds to
                                                                                the file name of the <span style='font-family:Monospace;'>FATE</span> executable"
                                                           )
                                                           )
                                     )
                                     )
                                     ) ## END fluidRow
                        ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             wellPanel(id = "main.panel"
                       , style = border.style
                       , fluidRow(
                         column(6
                                , wellPanel(id = "main.panel1",
                                            br(),
                                            HTML("<strong>ERRORS</strong>")
                                )
                         )
                         , column(6
                                  , wellPanel(id = "main.panel2",
                                              br(),
                                              HTML("<strong>OUTPUTS</strong>")
                                  )
                         )
                       )
             ) ## END wellPanel
           ) ## END mainPanel
                        ) ## END sidebarLayout
             ) ## tabPanel
