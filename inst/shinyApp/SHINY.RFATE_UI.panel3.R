
tabPanel(title =  HTML("<span class='panel_title'><i class='fa-solid fa-gears'></i> Run simulation</span>")
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
                        , actionButton(inputId = "HELP.panel3"
                                       , label = "Need some help"
                                       , icon = icon("circle-question")
                                       , width = "100%"
                                       , style = button.style.help)
               )
             ) ## END fluidRow
             , fluidRow(
               column(5
                      , div(id = "help3_3"
                            , numericInput(inputId = "run.opt.no_CPU"
                                           , label = param.style("Number of CPU")
                                           , value = 1
                                           , min = 1
                                           , width = "100%")
                      )
               )
               , column(5
                        , div(id = "help3_4"
                              , sliderInput(inputId = "run.opt.verbose_level"
                                            , label = param.style("Verbose level")
                                            , min = 0
                                            , max = 4
                                            , value = 1
                                            , step = 1
                                            , round = TRUE
                                            , width = "100%")
                        )
               )
               , column(2
                        , br()
                        , actionButton(inputId = "run"
                                       , label = "Run"
                                       , icon = icon("circle-play")
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
                         div(id = "help3_5"
                             , column(6, wellPanel(id = "main.panel1", br(), HTML("<strong>ERRORS :</strong> see in R console")))
                             , column(6, wellPanel(id = "main.panel2", br(), HTML("<strong>OUTPUTS :</strong> see in R console")))
                         )
                       ) ## END fluidRow
             ) ## END wellPanel
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
