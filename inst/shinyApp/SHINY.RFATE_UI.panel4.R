
tabPanel(title = HTML("<span class='panel_title'><i class='fa fa-chart-bar'></i> Simulation outputs & graphics</span>")
         , value = "panel4"
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = panel.style,
             withMathJax(),
             
             br(),
             fluidRow(
               column(5
                      , div(id = "help4_1"
                            , directoryInput(inputId = "graph.folder.simul"
                                             , label = param.style("Select the simulation folder :")
                                             , value = '~')
                      )
               )
               , column(5
                        , div(id = "help4_2"
                              , shinyjs::disabled(
                                selectInput(inputId = "graph.simulParam"
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
                        , actionButton(inputId = "HELP.panel4"
                                       , label = "Need some help"
                                       , icon = icon("question-circle")
                                       , width = "100%"
                                       , style = button.style.help))
             ) ## END fluidRow
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12,
             wellPanel(id = "main.panel"
                       , style = border.style
                       , tabsetPanel(
                         source("SHINY.RFATE_UI.panel4.tab1.R", local = TRUE)$value
                         , source("SHINY.RFATE_UI.panel4.tab2.R", local = TRUE)$value
                         , source("SHINY.RFATE_UI.panel4.tab3.R", local = TRUE)$value
                       ) ## END tabsetPanel
             ) ## END wellPanel
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
