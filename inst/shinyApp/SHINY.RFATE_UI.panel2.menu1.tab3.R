
tabPanel(title = HTML("<span class='tabPanel_title'>PFG files</span>")
         , value = "panel.PFG"
         , sidebarLayout(
           sidebarPanel = NULL,
           mainPanel = mainPanel(
             width = 12,
             br(),
             wellPanel(
               fluidRow(
                 column(1
                        , br()
                        , HTML("<strong>PFG</strong>")
                 )
                 , column(2
                          , br()
                          , textInput(inputId = "name.PFG"
                                      , label = NULL
                                      , width = "100%")
                 )
                 , column(2
                          , br()
                          , shinyjs::disabled(
                            actionButton(inputId = "add.PFG.name"
                                         , label = "Add PFG"
                                         , icon = icon("plus")
                                         , width = "100%"
                                         , style = button.style.action
                            )
                          )
                 )
                 , column(6
                          , wellPanel(style = "overflow-x:scroll;"
                                      , htmlOutput(outputId = "names.PFG"))
                 )
                 , column(1
                          , br()
                          , actionButton(inputId = "delete.names.PFG"
                                         , label = NULL
                                         , icon = icon("trash")
                                         , style = button.style.action
                          )
                 )
               )
               , fluidRow(
                 column(5, br())
                 , column(6
                          , br()
                          , textInput(inputId = "PFG.folder"
                                      , label = param.style("opt.folder.name")
                                      , value = NULL
                                      , width = "100%"))
               )
             ) ## END wellPanel
             , tabsetPanel(id = "panel.PFG_sub"
                           , tabPanel(title = HTML(paste0("<span class='tabPanel_subtitle'>"
                                                          , "<i class='fa fa-heart' style='font-size:15px;'></i>"
                                                          , " Succession</span>"))
                                      , value = "panel.succ"
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , shinyjs::disabled(
                                                 actionButton(inputId = "add.PFG.succ"
                                                              , label = "Add PFG"
                                                              , icon = icon("plus")
                                                              , width = "100%"
                                                              , style = button.style.action
                                                 )
                                               )
                                        )
                                        , column(6
                                                 , br()
                                                 , shinyjs::disabled(
                                                   actionButton(inputId = "create.succ"
                                                                , label = "Create PFG succession files"
                                                                , icon = icon("file")
                                                                , width = "100%"
                                                                , style = button.style.action
                                                   ) %>% helper(type = "inline"
                                                                , title = "Create PFG succession files"
                                                                , size = "l"
                                                                , content = help.HTML(paste0(path.reference, "PRE_FATE.params_PFGsuccession.html"))
                                                   )
                                                 )
                                        )
                                      )
                                      , fluidRow(
                                        column(2
                                               , br()
                                               , br()
                                               , HTML("<strong>PFG</strong>")
                                               , uiOutput(outputId = "UI.succ.PFG")
                                        )
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>type</strong>")
                                                 , selectInput(inputId = "succ.type"
                                                               , label = NULL
                                                               , choices = c("H", "C", "P")
                                                               , selected = NULL
                                                               , multiple = F
                                                               , width = "100%"))
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>height</strong>")
                                                 , numericInput(inputId = "succ.height"
                                                                , label = NULL
                                                                , value = 0
                                                                , min = 0
                                                                , width = "100%"))
                                        , column(3
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>maturity</strong>")
                                                 , numericInput(inputId = "succ.maturity"
                                                                , label = NULL
                                                                , value = 0
                                                                , min = 0
                                                                , width = "100%"))
                                        , column(3
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>longevity</strong>")
                                                 , numericInput(inputId = "succ.longevity"
                                                                , label = NULL
                                                                , value = 0
                                                                , min = 0
                                                                , width = "100%"))
                                      )
                                      , fluidRow(
                                        column(11
                                               , br()
                                               , wellPanel(style = "overflow-x:scroll;"
                                                           , tableOutput(outputId = "mat.PFG.succ")))
                                        , column(1
                                                 , br()
                                                 , actionButton(inputId = "delete.PFG.succ"
                                                                , label = NULL
                                                                , icon = icon("trash")
                                                                , style = button.style.action
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
                                                             , uiOutput(outputId = "UI.files.succ")))
                                        , column(12
                                                 , wellPanel(style = panel.style.scrollX
                                                             , dataTableOutput(outputId = "created_table.succ"))
                                        )
                                      )
                           ) ## END tabPanel (succ)
                           , tabPanel(title = HTML(paste0("<span class='tabPanel_subtitle'>"
                                                          , "<i class='fa fa-sun' style='font-size:15px;'></i>"
                                                          , " Light</span>"))
                                      , value = "panel.light"
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , shinyjs::disabled(
                                                 actionButton(inputId = "add.PFG.light"
                                                              , label = "Add PFG"
                                                              , icon = icon("plus")
                                                              , width = "100%"
                                                              , style = button.style.action
                                                 )
                                               )
                                        )
                                        , column(6
                                                 , br()
                                                 , shinyjs::disabled(
                                                   actionButton(inputId = "create.light"
                                                                , label = "Create PFG light files"
                                                                , icon = icon("file")
                                                                , width = "100%"
                                                                , style = button.style.action
                                                   ) %>% helper(type = "inline"
                                                                , title = "Create PFG light files"
                                                                , size = "l"
                                                                , content = help.HTML(paste0(path.reference, "PRE_FATE.params_PFGlight.html"))
                                                   )
                                                 )
                                        )
                                      )
                                      , fluidRow(
                                        column(2
                                               , br()
                                               , br()
                                               , HTML("<strong>PFG</strong>")
                                               , uiOutput(outputId = "UI.light.PFG")
                                        )
                                        , column(4, br(), br(), uiOutput(outputId = "UI.light.opt.tl"))
                                      )
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , HTML("<strong>Active germination</strong>")
                                               , radioButtons(inputId = "light.opt.ag"
                                                              , label = NULL
                                                              , choices = c("by type", "by strategy", "user-defined")
                                                              , selected = "by type"
                                                              , inline = TRUE
                                                              , width = "100%"))
                                        , column(6, br(), uiOutput(outputId = "UI.light.opt.ag"))
                                      )
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , HTML("<strong>Tolerance</strong>")
                                               , radioButtons(inputId = "light.opt.tol"
                                                              , label = NULL
                                                              , choices = c("by type & light", "by strategy", "user-defined")
                                                              , selected = "by type & light"
                                                              , inline = TRUE
                                                              , width = "100%"))
                                        , column(6, br(), uiOutput(outputId = "UI.light.opt.tol1"))
                                      )
                                      , fluidRow(column(12, br(), uiOutput(outputId = "UI.light.opt.tol2")))
                                      , fluidRow(
                                        column(11
                                               , br()
                                               , wellPanel(style = "overflow-x:scroll;"
                                                           , tableOutput(outputId = "mat.PFG.light")))
                                        , column(1
                                                 , br()
                                                 , actionButton(inputId = "delete.PFG.light"
                                                                , label = NULL
                                                                , icon = icon("trash")
                                                                , style = button.style.action
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
                                                             , uiOutput(outputId = "UI.files.light")))
                                        , column(12
                                                 , wellPanel(style = panel.style.scrollX
                                                             , dataTableOutput(outputId = "created_table.light"))
                                        )
                                      )
                           ) ## END tabPanel (light)
                           , tabPanel(title = HTML(paste0("<span class='tabPanel_subtitle'>"
                                                          , "<i class='fas fa-recycle' style='font-size:15px;'></i>"
                                                          , " Soil</span>"))
                                      , value = "panel.soil"
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , shinyjs::disabled(
                                                 actionButton(inputId = "add.PFG.soil"
                                                              , label = "Add PFG"
                                                              , icon = icon("plus")
                                                              , width = "100%"
                                                              , style = button.style.action
                                                 )
                                               )
                                        )
                                        , column(6
                                                 , br()
                                                 , shinyjs::disabled(
                                                   actionButton(inputId = "create.soil"
                                                                , label = "Create PFG soil files"
                                                                , icon = icon("file")
                                                                , width = "100%"
                                                                , style = button.style.action
                                                   ) %>% helper(type = "inline"
                                                                , title = "Create PFG soil files"
                                                                , size = "l"
                                                                , content = help.HTML(paste0(path.reference, "PRE_FATE.params_PFGsoil.html"))
                                                   )
                                                 )
                                        )
                                      )
                                      , fluidRow(
                                        column(2
                                               , br()
                                               , br()
                                               , HTML("<strong>PFG</strong>")
                                               , uiOutput(outputId = "UI.soil.PFG")
                                        )
                                      )
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , HTML("<strong>Contribution</strong>")
                                               , radioButtons(inputId = "soil.opt.con"
                                                              , label = NULL
                                                              , choices = c("by strategy", "user-defined")
                                                              , selected = "by strategy"
                                                              , inline = TRUE
                                                              , width = "100%"))
                                        , column(6, br(), uiOutput(outputId = "UI.soil.opt.con"))
                                      )
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , HTML("<strong>Active germination</strong>")
                                               , radioButtons(inputId = "soil.opt.ag"
                                                              , label = NULL
                                                              , choices = c("by type", "by strategy", "user-defined")
                                                              , selected = "by type"
                                                              , inline = TRUE
                                                              , width = "100%"))
                                        , column(6, br(), uiOutput(outputId = "UI.soil.opt.ag"))
                                      )
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , HTML("<strong>Tolerance</strong>")
                                               , radioButtons(inputId = "soil.opt.tol"
                                                              , label = NULL
                                                              , choices = c("pre-defined", "by strategy", "user-defined")
                                                              , selected = "pre-defined"
                                                              , inline = TRUE
                                                              , width = "100%"))
                                        , column(6, br(), uiOutput(outputId = "UI.soil.opt.tol1"))
                                      )
                                      , fluidRow(column(12, br(), uiOutput(outputId = "UI.soil.opt.tol2")))
                                      , fluidRow(
                                        column(10
                                               , br()
                                               , wellPanel(style = "overflow-x:scroll;"
                                                           , tableOutput(outputId = "mat.PFG.soil")))
                                        , column(2
                                                 , br()
                                                 , actionButton(inputId = "delete.PFG.soil"
                                                                , label = NULL
                                                                , icon = icon("trash")
                                                                , style = button.style.action
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
                                                             , uiOutput(outputId = "UI.files.soil")))
                                        , column(12
                                                 , wellPanel(style = panel.style.scrollX
                                                             , dataTableOutput(outputId = "created_table.soil"))
                                        )
                                      )
                           ) ## END tabPanel (soil)
                           , tabPanel(title = HTML(paste0("<span class='tabPanel_subtitle'>"
                                                          , "<i class='fa fa-seedling' style='font-size:15px;'></i>"
                                                          , " Dispersal</span>"))
                                      , value = "panel.disp"
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , shinyjs::disabled(
                                                 actionButton(inputId = "add.PFG.disp"
                                                              , label = "Add PFG"
                                                              , icon = icon("plus")
                                                              , width = "100%"
                                                              , style = button.style.action
                                                 )
                                               )
                                        )
                                        , column(6
                                                 , br()
                                                 , shinyjs::disabled(
                                                   actionButton(inputId = "create.disp"
                                                                , label = "Create PFG dispersal files"
                                                                , icon = icon("file")
                                                                , width = "100%"
                                                                , style = button.style.action
                                                   ) %>% helper(type = "inline"
                                                                , title = "Create PFG dispersal files"
                                                                , size = "l"
                                                                , content = help.HTML(paste0(path.reference, "PRE_FATE.params_PFGdispersal.html"))
                                                   )
                                                 )
                                        )
                                      )
                                      , fluidRow(
                                        column(2
                                               , br()
                                               , br()
                                               , HTML("<strong>PFG</strong>")
                                               , uiOutput(outputId = "UI.disp.PFG")
                                        )
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>d50</strong>")
                                                 , numericInput(inputId = "disp.d50"
                                                                , label = NULL
                                                                , value = 0
                                                                , min = 0
                                                                , width = "100%"))
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>d99</strong>")
                                                 , numericInput(inputId = "disp.d99"
                                                                , label = NULL
                                                                , value = 0
                                                                , min = 0
                                                                , width = "100%"))
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>ldd</strong>")
                                                 , numericInput(inputId = "disp.ldd"
                                                                , label = NULL
                                                                , value = 0
                                                                , min = 0
                                                                , width = "100%"))
                                      )
                                      , fluidRow(
                                        column(10
                                               , br()
                                               , wellPanel(style = "overflow-x:scroll;"
                                                           , tableOutput(outputId = "mat.PFG.disp")))
                                        , column(2
                                                 , br()
                                                 , actionButton(inputId = "delete.PFG.disp"
                                                                , label = NULL
                                                                , icon = icon("trash")
                                                                , style = button.style.action
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
                                                             , uiOutput(outputId = "UI.files.disp")))
                                        , column(12
                                                 , wellPanel(style = panel.style.scrollX
                                                             , dataTableOutput(outputId = "created_table.disp"))
                                        )
                                      )
                           ) ## END tabPanel (dispersal)
                           , tabPanel(title = HTML(paste0("<span class='tabPanel_subtitle'>"
                                                          , "<i class='fas fa-bolt' style='font-size:15px;'></i>"
                                                          , " Disturbances</span>"))
                                      , value = "panel.dist"
                                      , fluidRow(column(12, br(), uiOutput(outputId = "UI.dist.opt.ages")))
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , actionButton(inputId = "add.PFG.dist"
                                                              , label = "Add disturbance"
                                                              , icon = icon("plus")
                                                              , width = "100%"
                                                              , style = button.style.action
                                               )
                                        )
                                        , column(6
                                                 , br()
                                                 , shinyjs::disabled(
                                                   actionButton(inputId = "create.dist"
                                                                , label = "Create PFG disturbance files"
                                                                , icon = icon("file")
                                                                , width = "100%"
                                                                , style = button.style.action
                                                   ) %>% helper(type = "inline"
                                                                , title = "Create PFG disturbance files"
                                                                , size = "l"
                                                                , content = help.HTML(paste0(path.reference, "PRE_FATE.params_PFGdisturbance.html"))
                                                   )
                                                 )
                                        )
                                      )
                                      , fluidRow(
                                        column(4
                                               , br()
                                               , br()
                                               , HTML("<strong>Disturbance name</strong>")
                                               , textInput(inputId = "dist.name"
                                                           , label = NULL
                                                           , width = "100%"))
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>Grouping</strong>")
                                                 , radioButtons(inputId = "dist.opt.group"
                                                                , label = NULL
                                                                , choices = c("by type", "by PFG")
                                                                , selected = "by type"
                                                                , width = "100%"))
                                        , column(3
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>Resprout & break ages</strong>")
                                                 , shinyjs::disabled(
                                                   radioButtons(inputId = "dist.opt.ages"
                                                                , label = NULL
                                                                , choices = c("pre-defined", "user-defined")
                                                                , selected = "pre-defined"
                                                                , width = "100%")
                                                 ))
                                        , column(3
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>Response</strong>")
                                                 , radioButtons(inputId = "dist.opt.resp"
                                                                , label = NULL
                                                                , choices = c("by strategy", "user-defined")
                                                                , selected = "by strategy"
                                                                , width = "100%"))
                                      )
                                      , fluidRow(column(12, br(), uiOutput(outputId = "UI.dist_table")))
                                      , fluidRow(
                                        column(10
                                               , br()
                                               , wellPanel(style = "overflow-x:scroll;"
                                                           , tableOutput(outputId = "mat.PFG.dist")))
                                        , column(2
                                                 , br()
                                                 , actionButton(inputId = "delete.PFG.dist"
                                                                , label = NULL
                                                                , icon = icon("trash")
                                                                , style = button.style.action
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
                                                             , uiOutput(outputId = "UI.files.dist")))
                                        , column(12
                                                 , wellPanel(style = panel.style.scrollX
                                                             , dataTableOutput(outputId = "created_table.dist"))
                                        )
                                      )
                           ) ## END tabPanel (disturbances)
                           , tabPanel(title = HTML(paste0("<span class='tabPanel_subtitle'>"
                                                          , "<i class='fas fa-tint' style='font-size:15px;'></i>"
                                                          , " Drought</span>"))
                                      , value = "panel.drought"
                                      , fluidRow(column(12, br(), uiOutput(outputId = "UI.drought.opt.ages")))
                                      , fluidRow(
                                        column(6
                                               , br()
                                               , actionButton(inputId = "add.PFG.drought"
                                                              , label = "Add drought"
                                                              , icon = icon("plus")
                                                              , width = "100%"
                                                              , style = button.style.action
                                               )
                                        )
                                        , column(6
                                                 , br()
                                                 , shinyjs::disabled(
                                                   actionButton(inputId = "create.drought"
                                                                , label = "Create PFG drought files"
                                                                , icon = icon("file")
                                                                , width = "100%"
                                                                , style = button.style.action
                                                   ) %>% helper(type = "inline"
                                                                , title = "Create PFG drought files"
                                                                , size = "l"
                                                                , content = help.HTML(paste0(path.reference, "PRE_FATE.params_PFGdrought.html"))
                                                   )
                                                 )
                                        )
                                      )
                                      , fluidRow(
                                        column(4
                                               , br()
                                               , br()
                                               , HTML("<strong>Drought name</strong>")
                                               , selectInput(inputId = "drought.name"
                                                             , label = NULL
                                                             , choices = c("immediate", "delayed")
                                                             , selected = "immediate"
                                                             , multiple = FALSE
                                                             , width = "100%"))
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>Grouping</strong>")
                                                 , radioButtons(inputId = "drought.opt.group"
                                                                , label = NULL
                                                                , choices = c("by type", "by PFG")
                                                                , selected = "by type"
                                                                , width = "100%"))
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>Resprout & break ages</strong>")
                                                 , shinyjs::disabled(
                                                   radioButtons(inputId = "drought.opt.ages"
                                                                , label = NULL
                                                                , choices = c("pre-defined", "user-defined")
                                                                , selected = "pre-defined"
                                                                , width = "100%")
                                                 ))
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>Response</strong>")
                                                 , radioButtons(inputId = "drought.opt.resp"
                                                                , label = NULL
                                                                , choices = c("by strategy", "user-defined")
                                                                , selected = "by strategy"
                                                                , width = "100%"))
                                        , column(2
                                                 , br()
                                                 , br()
                                                 , HTML("<strong>Sensitivity</strong>")
                                                 , radioButtons(inputId = "drought.opt.sens"
                                                                , label = NULL
                                                                , choices = c("by strategy", "user-defined")
                                                                , selected = "by strategy"
                                                                , width = "100%"))
                                      )
                                      , fluidRow(column(12, br(), uiOutput(outputId = "UI.drought_table")))
                                      , fluidRow(column(12, br(), uiOutput(outputId = "UI.drought_table.BIS")))
                                      , fluidRow(
                                        column(10
                                               , br()
                                               , wellPanel(style = "overflow-x:scroll;"
                                                           , tableOutput(outputId = "mat.PFG.drought")))
                                        , column(2
                                                 , br()
                                                 , actionButton(inputId = "delete.PFG.drought"
                                                                , label = NULL
                                                                , icon = icon("trash")
                                                                , style = button.style.action
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
                                                             , uiOutput(outputId = "UI.files.drought")))
                                        , column(12
                                                 , wellPanel(style = panel.style.scrollX
                                                             , dataTableOutput(outputId = "created_table.drought"))
                                        )
                                      )
                           ) ## END tabPanel (drought)
             ) ## END tabSetPanel
           ) ## END mainPanel
         ) ## END sidebarLayout
) ## END tabPanel (PFG files)
