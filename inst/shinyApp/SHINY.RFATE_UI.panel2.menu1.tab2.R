
tabPanel(title = HTML("<span class='tabPanel_title'>Scenario files</span>")
         , value = "panel.scenario"
         , sidebarLayout(
           sidebarPanel = NULL,
           mainPanel = mainPanel(
             width = 12,
             fluidRow(
               column(6
                      , br()
                      , wellPanel(
                        HTML(paste0("<i class='far fa-heart' style='font-size:15px;'></i>"
                                    , " <strong>Save maps ?</strong>"))
                        , br()
                        , br()
                        , textInput(inputId = "save.maps.folder"
                                    , label = param.style("opt.folder.name")
                                    , value = NULL
                                    , width = "100%")
                        , br()
                        , br()
                        , numericInput(inputId = "save.maps.year1"
                                       , label = param.style("years.maps.start")
                                       , value = 0
                                       , min = 0
                                       , width = "100%")
                        , numericInput(inputId = "save.maps.year2"
                                       , label = param.style("years.maps.end")
                                       , value = 0
                                       , min = 0
                                       , width = "100%")
                        , numericInput(inputId = "save.maps.no"
                                       , label = param.style("years.maps.number")
                                       , value = 100
                                       , min = 0
                                       , max = 200
                                       , step = 10
                                       , width = "100%")
                        , br()
                        , br()
                        , actionButton(inputId = "create.save.maps"
                                       , label = "Create SAVE maps files"
                                       , icon = icon("file")
                                       , width = "100%"
                                       , style = button.style.action
                        )
                      ) %>% helper(type = "inline"
                                   , title = "Create SAVE maps files"
                                   , size = "l"
                                   , content = help.HTML(paste0(path.reference, "PRE_FATE.params_savingYears.html"))
                      )
               )
               , column(6
                        , br()
                        , wellPanel(
                          HTML(paste0("<i class='far fa-heart' style='font-size:15px;'></i>"
                                      , " <strong>Save simulation ?</strong>"))
                          , br()
                          , br()
                          , textInput(inputId = "save.objects.folder"
                                      , label = param.style("opt.folder.name")
                                      , value = NULL
                                      , width = "100%")
                          , br()
                          , br()
                          , numericInput(inputId = "save.objects.year1"
                                         , label = param.style("years.objects")
                                         , value = 0
                                         , min = 0
                                         , width = "100%")
                          , numericInput(inputId = "save.objects.year2"
                                         , label = NULL
                                         , value = 0
                                         , min = 0
                                         , width = "100%")
                          , numericInput(inputId = "save.objects.year3"
                                         , label = NULL
                                         , value = 0
                                         , min = 0
                                         , width = "100%")
                          , br()
                          , br()
                          , actionButton(inputId = "create.save.objects"
                                         , label = "Create SAVE objects files"
                                         , icon = icon("file")
                                         , width = "100%"
                                         , style = button.style.action
                          )
                        ) %>% helper(type = "inline"
                                     , title = "Create SAVE objects files"
                                     , size = "l"
                                     , content = help.HTML(paste0(path.reference, "PRE_FATE.params_savingYears.html"))
                        )
               )
               )
             , fluidRow(
               br()
               , br()
               , br()
               , br()
               , column(12
                        , wellPanel(style = panel.style.scrollY
                                    , uiOutput(outputId = "UI.files.save")))
               , column(12
                        , wellPanel(style = panel.style.scrollX
                                    , dataTableOutput(outputId = "created_table.save"))
               )
             )
         ) ## END mainPanel
         ) ## END sidebarLayout
         ) ## END tabPanel (Scenario files)
