
tabPanel(title = HTML("<span class='tabPanel_title'>Global parameters</span>")
         , value = "panel.global"
         , sidebarLayout(
           sidebarPanel = NULL,
           mainPanel = mainPanel(
             width = 12,
             fluidRow(
               column(4
                      , br()
                      , numericInput(inputId = "required.no_PFG"
                                     , label = HTML(paste0("<i class='fa fa-heart' style='font-size:15px;'></i>"
                                                           , param.style(" required.no_PFG")))
                                     , value = 1
                                     , min = 1
                                     , width = "100%")
                      , numericInput(inputId = "required.no_strata"
                                     , label = HTML(paste0("<i class='fa fa-heart' style='font-size:15px;'></i>"
                                                           , param.style(" required.no_strata")))
                                     , value = 1
                                     , min = 1
                                     , width = "100%")
                      , numericInput(inputId = "required.simul_duration"
                                     , label = HTML(paste0("<i class='fa fa-heart' style='font-size:15px;'></i>"
                                                           , param.style(" required.simul_duration")))
                                     , value = 1000
                                     , min = 1
                                     , width = "100%")
               )
               , column(4
                        , br()
                        , numericInput(inputId = "required.max_abund_low"
                                       , label = HTML(paste0("<i class='fa fa-heart' style='font-size:15px;'></i>"
                                                             , param.style(" required.max_abund_low")))
                                       , value = 3000
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.max_abund_medium"
                                       , label = HTML(paste0("<i class='fa fa-heart' style='font-size:15px;'></i>"
                                                             , param.style(" required.max_abund_medium")))
                                       , value = 6000
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.max_abund_high"
                                       , label = HTML(paste0("<i class='fa fa-heart' style='font-size:15px;'></i>"
                                                             , param.style(" required.max_abund_high")))
                                       , value = 9000
                                       , min = 1
                                       , width = "100%")
               )
               , column(4
                        , br()
                        , numericInput(inputId = "required.seeding_duration"
                                       , label = HTML(paste0("<i class='far fa-heart' style='font-size:15px;'></i>"
                                                             , param.style(" required.seeding_duration")))
                                       , value = 300
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.seeding_timestep"
                                       , label = HTML(paste0("<i class='far fa-heart' style='font-size:15px;'></i>"
                                                             , param.style(" required.seeding_timestep")))
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
                        , numericInput(inputId = "required.seeding_input"
                                       , label = HTML(paste0("<i class='far fa-heart' style='font-size:15px;'></i>"
                                                             , param.style(" required.seeding_input")))
                                       , value = 100
                                       , min = 0
                                       , width = "100%")
                        , numericInput(inputId = "opt.no_CPU"
                                       , label = HTML(paste0("<i class='far fa-heart' style='font-size:15px;'></i>"
                                                             , param.style(" opt.no_CPU")))
                                       , value = 1
                                       , min = 1
                                       , width = "100%")
               )
             )
             , fluidRow(
               column(4
                      , br()
                      , checkboxInput(inputId = "doDispersal"
                                      , label = HTML(paste0("<i class='fa fa-seedling' style='font-size:15px;'></i>"
                                                       , param.style(" doDispersal")))
                                      , value = FALSE
                                      , width = "100%")
                      , uiOutput(outputId = "UI.doDispersal")
               )
               , column(4
                        , br()
                        , checkboxInput(inputId = "doHabSuitability"
                                        , label = HTML(paste0("<i class='fas fa-globe' style='font-size:15px;'></i>"
                                                              , param.style(" doHabSuitability")))
                                        , value = FALSE
                                        , width = "100%")
                        , uiOutput(outputId = "UI.doHabSuitability")
               )
             )
             , fluidRow(
               column(4
                      , br()
                      , checkboxInput(inputId = "doLight"
                                      , label = HTML(paste0("<i class='fa fa-sun' style='font-size:15px;'></i>"
                                                            , param.style(" doLight")))
                                      , value = FALSE
                                      , width = "100%")
                      , uiOutput(outputId = "UI.doLight")
               )
               , column(4
                        , br()
                        , checkboxInput(inputId = "doSoil"
                                        , label = HTML(paste0("<i class='fas fa-recycle' style='font-size:15px;'></i>"
                                                              , param.style(" doSoil")))
                                        , value = FALSE
                                        , width = "100%")
                        , uiOutput(outputId = "UI.doSoil")
               )
             )
             , fluidRow(
               column(4
                        , br()
                        , checkboxInput(inputId = "doDisturbances"
                                        , label = HTML(paste0("<i class='fas fa-bolt' style='font-size:15px;'></i>"
                                                              , param.style(" doDisturbances")))
                                        , value = FALSE
                                        , width = "100%")
                        , uiOutput(outputId = "UI.doDisturbances")
               )
               , column(4
                        , br()
                        , checkboxInput(inputId = "doDrought"
                                        , label = HTML(paste0("<i class='fas fa-tint' style='font-size:15px;'></i>"
                                                              , param.style(" doDrought")))
                                        , value = FALSE
                                        , width = "100%")
                        , uiOutput(outputId = "UI.doDrought")
               )
               , column(4
                        , br()
                        , checkboxInput(inputId = "doAliens"
                                        , label = HTML(paste0("<i class='fab fa-reddit-alien' style='font-size:15px;'></i>"
                                                              , param.style(" doAliens")))
                                        , value = FALSE
                                        , width = "100%")
                        , uiOutput(outputId = "UI.doAliens")
               )
             )
             , fluidRow(
               column(4
                      , br()
                      , checkboxInput(inputId = "doFire"
                                      , label = HTML(paste0("<i class='fas fa-fire' style='font-size:15px;'></i>"
                                                            , param.style(" doFire")))
                                      , value = FALSE
                                      , width = "100%")
                      , uiOutput(outputId = "UI.doFire1")
               )
               , column(8
                        , br()
                        , uiOutput(outputId = "UI.doFire2.1")
                        , uiOutput(outputId = "UI.doFire2.2")
                        , uiOutput(outputId = "UI.doFire2.3")
                        , uiOutput(outputId = "UI.doFire2.4")
               )
             )
             , fluidRow(
               column(6
                      , br()
                      , actionButton(inputId = "create.global"
                                     , label = "Create Global parameters file"
                                     , icon = icon("file")
                                     , width = "100%"
                                     , style = button.style.action
                      ) %>% helper(type = "inline"
                                   , title = "Global parameter file"
                                   , size = "l"
                                   , content = help.HTML(paste0(path.reference, "PRE_FATE.params_globalParameters.html"))
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
                                    , uiOutput(outputId = "UI.files.global")))
               , column(12
                        , wellPanel(style = panel.style.scrollX
                                    , dataTableOutput(outputId = "created_table.global"))
               )
             )
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## END tabPanel (Global parameters)
