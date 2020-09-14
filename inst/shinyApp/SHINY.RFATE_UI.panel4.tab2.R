
tabPanel(title = HTML("<span class='tabPanel_title'>Through time</span>")
         , value = "panel.through_time"
         , fluidRow(
           column(2
                  , br()
                  , HTML(param.style("no.years"))
                  , numericInput(inputId = "graph.no.years"
                                 , label = NULL
                                 , value = 10
                                 , min = 1
                                 , width = "100%")
           )
           , column(2
                    , br()
                    , HTML(param.style("opt.no_CPU"))
                    , numericInput(inputId = "graph.opt.no_CPU"
                                   , label = NULL
                                   , value = 1
                                   , min = 1
                                   , width = "100%")
           )
           , column(4
                    , br()
                    , HTML(param.style("opt.ras_habitat"))
                    , fileInput(inputId = "graph.opt.ras_habitat"
                                , label = NULL
                                , multiple = FALSE
                                , width = "100%")
           )
           , column(4
                    , br()
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "create.temporalEvolution"
                                   , label = "Run temporal evolution"
                                   , icon = icon("play")
                                   , width = "100%"
                                   , style = button.style.action
                      ) %>% helper(type = "inline"
                                   , title = "Calculate tables of temporal evolution of pixels resources"
                                   , size = "l"
                                   , content = help.HTML(paste0(path.reference, "POST_FATE.temporalEvolution.html"))
                      )
                    )
           )
         )
         , radioGroupButtons(inputId = "show.through_time"
                             , label = ""
                             , choices = c("Total abundance & coverage"
                                           , "Pixels abundance & resources"
                                           , "Community composition stability")
                             , selected = 0
                             , justified = TRUE
                             , status = "panelgraph"
                             , checkIcon = list(yes = icon("ok", lib = "glyphicon")
                                                , no = icon("remove", lib = "glyphicon"))
         )
         , fluidRow(
           br()
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionCoverage"
               , column(8
                        , plotlyOutput(outputId = "plot.evolutionCoverage1", width = "100%", height = "600px")
                        , plotlyOutput(outputId = "plot.evolutionCoverage2", width = "100%", height = "600px")
               )
               , column(4
                        , checkboxInput(inputId = "graph.opt.fixedScale"
                                        , label = param.style("opt.fixedScale")
                                        , value = TRUE
                                        , width = "100%")
                        , br()
                        , actionButton(inputId = "create.evolutionCoverage"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = button.style.action
                          ) %>% helper(type = "inline"
                                       , title = "Plot evolution coverage"
                                       , size = "l"
                                       , content = help.HTML(paste0(path.reference, "POST_FATE.graphic_evolutionCoverage.html"))
                          )
               )
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionPixels"
               , column(8
                        , plotlyOutput(outputId = "plot.evolutionPixels", width = "100%", height = "600px")
               )
               , column(4
                        , checkboxInput(inputId = "graph.opt.fixedScale"
                                        , label = param.style("opt.fixedScale")
                                        , value = TRUE
                                        , width = "100%")
                        , checkboxInput(inputId = "graph.opt.cells_ID"
                                        , label = param.style("opt.cells_ID")
                                        , value = FALSE
                                        , width = "100%")
                        , uiOutput(outputId = "UI.opt.cells_ID")
                        , br()
                        , actionButton(inputId = "create.evolutionPixels"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = button.style.action
                        ) %>% helper(type = "inline"
                                     , title = "Plot evolution pixels"
                                     , size = "l"
                                     , content = help.HTML(paste0(path.reference, "POST_FATE.graphic_evolutionPixels.html"))
                        )
               )
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.evolutionStability"
               , column(8
                        , plotlyOutput(outputId = "plot.evolutionStability", width = "100%", height = "600px")
               )
               , column(4
                        , numericInput(inputId = "graph.mw.size"
                                        , label = param.style("movingWindow_size")
                                        , value = 3
                                        , width = "100%")
                        , numericInput(inputId = "graph.mw.step"
                                        , label = param.style("movingWindow_step")
                                        , value = 1
                                        , width = "100%")
                        , br()
                        , actionButton(inputId = "create.evolutionStability"
                                       , label = "Run plot"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = button.style.action
                        ) %>% helper(type = "inline"
                                     , title = "Plot evolution stability"
                                     , size = "l"
                                     , content = help.HTML(paste0(path.reference, "POST_FATE.graphic_evolutionStability.html"))
                        )
               )
             ))
         )
) ## END tabPanel (Global parameters)
