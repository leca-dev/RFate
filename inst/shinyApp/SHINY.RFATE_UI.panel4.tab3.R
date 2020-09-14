
tabPanel(title = HTML("<span class='tabPanel_title'>Specific year</span>")
         , value = "panel.specific_year"
         , fluidRow(
           column(4
                  , br()
                  , HTML(param.style("year(s)"))
                  , shinyjs::disabled(
                    selectInput(inputId = "graph.year"
                                , label = NULL
                                , choices = NULL
                                , selected = NULL
                                , multiple = TRUE
                                , width = "100%")
                  )
           )
           , column(4
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
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "create.relativeAbund"
                                   , label = "Run relative abund"
                                   , icon = icon("play")
                                   , width = "100%"
                                   , style = button.style.action
                      ) %>% helper(type = "inline"
                                   , title = "Create maps of relative abundance"
                                   , size = "l"
                                   , content = help.HTML(paste0(path.reference, "POST_FATE.relativeAbund.html"))
                      )
                    )
           )
         )
         , fluidRow(
           column(4
                  , br()
                  , HTML(param.style("method"))
                  , shinyjs::disabled(
                    radioButtons(inputId = "graph.binMethod"
                                 , label = NULL
                                 , choices = c("(1) fixed threshold", "(2) optimizing TSS")
                                 , selected = "(1) fixed threshold"
                                 , inline = TRUE
                                 , width = "100%")
                  )
           )
           , column(4
                    , br()
                    , HTML(param.style("threshold / cutoff"))
                    , uiOutput(outputId = "UI.graph.binMethod.opt")
           )
           , column(4
                    , br()
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "create.binaryMaps"
                                   , label = "Run binary maps"
                                   , icon = icon("play")
                                   , width = "100%"
                                   , style = button.style.action
                      ) %>% helper(type = "inline"
                                   , title = "Create binary maps (transform relative abundance into 0/1)"
                                   , size = "l"
                                   , content = help.HTML(paste0(path.reference, "POST_FATE.binaryMaps.html"))
                      )
                    )
           )
         )
         , radioGroupButtons(inputId = "show.spatial_maps"
                             , label = ""
                             , choices = c("Validation statistics"
                                           , "Maps : PFG vs HS"
                                           , "Maps : PFG cover, richness, CWM")
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
               id = "panel.validationStat"
               , column(8
                        , plotlyOutput(outputId = "plot.validationStat", width = "100%", height = "600px")
               )
               , column(4
                        , fluidRow(
                          column(10
                                 , br()
                                 , br()
                                 , uiOutput(outputId = "UI.graph.mat.PFG.obs")
                          )
                          , column(2
                                   , br()
                                   , br()
                                   , actionButton(inputId = "graph.mat.PFG.obs.delete"
                                                  , label = ""
                                                  , icon = icon("broom")
                                                  , width = "100%"
                                                  , style = button.style.action
                                   )
                          )
                        )
                        , HTML(param.style("opt.ras_habitat"))
                        , fileInput(inputId = "graph.opt.ras_habitat"
                                    , label = NULL
                                    , multiple = FALSE
                                    , width = "100%")
                        , br()
                        , actionButton(inputId = "create.validationStat"
                                       , label = "Run Validation statistics"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = button.style.action
                        ) %>% helper(type = "inline"
                                     , title = "Plot validation statistics and transform maps of abundances into 0/1"
                                     , size = "l"
                                     , content = help.HTML(paste0(path.reference, "POST_FATE.graphic_validationStatistics.html"))
                        )
               )
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.PFGvsHS"
               , column(8
                        , plotlyOutput(outputId = "plot.PFGvsHS", width = "100%", height = "600px")
               )
               , column(4
                        , HTML(param.style("opt.stratum"))
                        , selectInput(inputId = "graph.opt.stratum"
                                      , label = NULL
                                      , choices = "all"
                                      , selected = "all"
                                      , multiple = FALSE
                                      , width = "100%")
                        , br()
                        , actionButton(inputId = "create.PFGvsHS"
                                       , label = "Run PFG vs HS"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = button.style.action
                        ) %>% helper(type = "inline"
                                     , title = "Plot maps of 0/1 predicted by FATE vs Habitat suitability"
                                     , size = "l"
                                     , content = help.HTML(paste0(path.reference, "POST_FATE.graphic_mapPFGvsHS.html"))
                        )
               )
             ))
           , shinyjs::hidden(
             fluidRow(
               id = "panel.PFGmap"
               , column(8
                        , plotlyOutput(outputId = "plot.PFGmap", width = "100%", height = "600px")
               )
               , column(4
                        , fluidRow(
                          column(8
                                 , HTML(param.style("opt.stratum min - max"))
                                 , sliderInput(inputId = "graph.opt.stratum_minmax"
                                               , label = NULL
                                               , min = 1
                                               , max = 10
                                               , value = c(1, 10)
                                               , step = 1
                                               , width = "100%")
                          )
                          , column(4
                                   , HTML(param.style("opt.doBinary"))
                                   , checkboxInput(inputId = "graph.opt.doBinary"
                                                   , label = NULL
                                                   , value = FALSE
                                                   , width = "100%")
                          )
                        )
                        , br()
                        , actionButton(inputId = "create.PFGmap"
                                       , label = "Run PFG map"
                                       , icon = icon("play")
                                       , width = "100%"
                                       , style = button.style.action
                        ) %>% helper(type = "inline"
                                     , title = "Plot map of PFG outputs"
                                     , size = "l"
                                     , content = help.HTML(paste0(path.reference, "POST_FATE.graphic_mapPFG.html"))
                        )
               )
             ))
         ) ## END fluidRow
) ## END tabPanel (Global parameters)
