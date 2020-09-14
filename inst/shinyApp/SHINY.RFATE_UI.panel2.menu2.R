
tabPanel(title =  HTML("<span class='panel_title'><i class='fa fa-clone'></i> Create multiple set</span>")
         , value = "panel3"
         
         , fluidRow(
           div(id = "help2_2_1",
               radioGroupButtons(inputId = "set.strategy"
                                 , label = NULL
                                 , choices = c("From 1 folder, 1 simulation file"
                                               , "From 1 folder, 2 simulation files"
                                               , "From 2 folders, 2 simulation files")
                                 , selected = 0
                                 , justified = TRUE
                                 , status = "panelgraph"
                                 , checkIcon = list(yes = icon("ok", lib = "glyphicon")
                                                    , no = icon("remove", lib = "glyphicon"))
               )
           )
         )
         
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 12,
             style = panel.style,
             withMathJax(),
             
             br(),
             fluidRow(
               column(5
                      , div(id = "help2_2_2"
                            , directoryInput(inputId = "set.folder1"
                                             , label = param.style("Select the simulation folder :")
                                             , value = '~')
                            , shinyjs::disabled(
                              selectInput(inputId = "set.folder1.simulParam1"
                                          , label = param.style("Select the simulation parameters file(s) :")
                                          , choices = NULL
                                          , selected = NULL
                                          , multiple = F
                                          , width = "100%")
                            ))
                      , shinyjs::hidden(
                        selectInput(inputId = "set.folder1.simulParam2"
                                    , label = NULL
                                    , choices = NULL
                                    , selected = NULL
                                    , multiple = F
                                    , width = "100%")
                      )
               )
               , column(5, uiOutput(outputId = "UI.set.folders.strat3")
               )
               , column(2
                        , br()
                        , actionButton(inputId = "HELP.panel2.menu2"
                                       , label = "Need some help"
                                       , icon = icon("question-circle")
                                       , width = "100%"
                                       , style = button.style.help))
             ) ## END fluidRow
           ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 12
             , shinyjs::hidden(
               
               wellPanel(id = "main.panel"
                         , style = border.style
                         , fluidRow(
                           column(10
                                  , div(id = "help2_2_3"
                                        , wellPanel(id = "main.panel1",
                                                    br(),
                                                    fluidRow(
                                                      column(4
                                                             , checkboxGroupInput(inputId = "set.choices.1"
                                                                                  , label = HTML("<i class='fa fa-heart'></i> global parameters")
                                                                                  , choices = c("max_abund_low"
                                                                                                , "max_abund_medium"
                                                                                                , "max_abund_high")
                                                                                  , selected = NULL
                                                                                  , width = "100%"
                                                             ) %>% helper(type = "inline"
                                                                          , title = "FATE modules : CORE - Required and impacted parameters"
                                                                          , size = "l"
                                                                          , content = help.HTML(html.file = paste0(path.articles, "fate_tutorial_3_MODULES.html")
                                                                                                , target.anchor = 'class="section level1'
                                                                                                , target.class = "core-module-succession"
                                                                                                , web.address = paste0(path.articles, "fate_tutorial_3_MODULES.html#core-module-succession"))
                                                             )
                                                      )
                                                      , column(3
                                                               , div(id = "help2_2_4"
                                                                     , shinyjs::hidden(
                                                                       sliderInput(inputId = "set.slider.1"
                                                                                   , label = HTML("% of variation<br/><br/>")
                                                                                   , min = 0
                                                                                   , max = 100
                                                                                   , value = 50
                                                                                   , step = 5
                                                                                   , round = TRUE
                                                                                   , width = "100%"
                                                                       )
                                                                     )
                                                               )
                                                      )
                                                      , column(1, br())
                                                      , column(4
                                                               , checkboxGroupInput(inputId = "set.choices.4"
                                                                                    , label = HTML("<i class='fa fa-globe'></i> habitat suitability")
                                                                                    , choices = "habsuit_mode"
                                                                                    , selected = NULL
                                                                                    , width = "100%"
                                                               )
                                                      )
                                                    ) %>% helper(type = "inline"
                                                                 , title = "FATE modules : HABITAT SUITABILITY - Required and impacted parameters"
                                                                 , size = "l"
                                                                 , content = help.HTML(html.file = paste0(path.articles, "fate_tutorial_3_MODULES.html")
                                                                                       , target.anchor = 'class="section level1'
                                                                                       , target.class = "habitat-suitability-module"
                                                                                       , web.address = paste0(path.articles, "fate_tutorial_3_MODULES.html#habitat-suitability-module"))
                                                    )
                                                    , fluidRow(
                                                      br()
                                                      , column(4
                                                               , checkboxGroupInput(inputId = "set.choices.2"
                                                                                    , label = HTML("<i class='fa fa-heart'></i> seeding")
                                                                                    , choices = c("seeding_duration"
                                                                                                  , "seeding_timestep"
                                                                                                  , "seeding_input")
                                                                                    , selected = NULL
                                                                                    , width = "100%"
                                                               )
                                                      )
                                                      , column(3
                                                               , shinyjs::hidden(
                                                                 sliderInput(inputId = "set.slider.2"
                                                                             , label = HTML("<br/>")
                                                                             , min = 0
                                                                             , max = 100
                                                                             , value = 50
                                                                             , step = 5
                                                                             , round = TRUE
                                                                             , width = "100%"
                                                                 )
                                                               )
                                                      )
                                                      , column(1, br())
                                                      , column(4
                                                               , checkboxGroupInput(inputId = "set.choices.5"
                                                                                    , label = HTML("<i class='fa fa-seedling'></i> dispersal")
                                                                                    , choices = c("dispersal_mode")
                                                                                    , selected = NULL
                                                                                    , width = "100%"
                                                               )
                                                      )
                                                    ) %>% helper(type = "inline"
                                                                 , title = "FATE modules : DISPERSAL - Required and impacted parameters"
                                                                 , size = "l"
                                                                 , content = help.HTML(html.file = paste0(path.articles, "fate_tutorial_3_MODULES.html")
                                                                                       , target.anchor = 'class="section level1'
                                                                                       , target.class = "dispersal-module"
                                                                                       , web.address = paste0(path.articles, "fate_tutorial_3_MODULES.html#dispersal-module"))
                                                    )
                                                    , fluidRow(
                                                      br()
                                                      , column(4
                                                               , checkboxGroupInput(inputId = "set.choices.3"
                                                                                    , label = HTML("<i class='fa fa-sun'></i> global parameters")
                                                                                    , choices = c("light_thresh_medium"
                                                                                                  , "light_thresh_low")
                                                                                    , selected = NULL
                                                                                    , width = "100%"
                                                               )
                                                      )
                                                      , column(3
                                                               , shinyjs::hidden(
                                                                 sliderInput(inputId = "set.slider.3"
                                                                             , label = HTML("<br/>")
                                                                             , min = 0
                                                                             , max = 100
                                                                             , value = 50
                                                                             , step = 5
                                                                             , round = TRUE
                                                                             , width = "100%"
                                                                 )
                                                               )
                                                      )
                                                      , column(1, br())
                                                      , column(4
                                                               , checkboxGroupInput(inputId = "set.choices.6"
                                                                                    , label = HTML("<i class='fa fa-sun'></i> height strata")
                                                                                    , choices = c("strata_limits")
                                                                                    , selected = NULL
                                                                                    , width = "100%"
                                                               )
                                                      )
                                                    ) %>% helper(type = "inline"
                                                                 , title = "FATE modules : LIGHT - Required and impacted parameters"
                                                                 , size = "l"
                                                                 , content = help.HTML(html.file = paste0(path.articles, "fate_tutorial_3_MODULES.html")
                                                                                       , target.anchor = 'class="section level1'
                                                                                       , target.class = "light-module-competition"
                                                                                       , web.address = paste0(path.articles, "fate_tutorial_3_MODULES.html#light-module-competition"))
                                                    ) ## END fluidRow
                                        ) ## END main.panel1
                                  )
                           )
                           , column(2
                                    , wellPanel(id = "main.panel2",
                                                br(),
                                                div(id = "help2_2_5",
                                                    numericInput(inputId = "set.num_simul"
                                                                 , label = "Maximum number of simulation files"
                                                                 , value = 3000
                                                                 , min = 1000
                                                                 , max = 10000
                                                                 , step = 500
                                                                 , width = "100%"
                                                    )
                                                )
                                                , br()
                                                , br()
                                                , shinyjs::disabled(
                                                  actionButton(inputId = "create.multiple_set"
                                                               , label = HTML("Create <br/>multiple set")
                                                               , icon = icon("play")
                                                               , width = "100%"
                                                               , style = button.style.action
                                                  ) %>% helper(type = "inline"
                                                               , title = "Multiple sets of parameters"
                                                               , size = "l"
                                                               , content = help.HTML(paste0(path.reference, "PRE_FATE.params_multipleSet.html"))
                                                  )
                                                )
                                    ) ## END main.panel2
                           )
                         )
               ) ## END main.panel
             )
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## tabPanel
