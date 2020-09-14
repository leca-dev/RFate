
tabPanel(title = HTML("<p class='panel_title'><i class='fa fa-folder-plus'></i> New / &emsp;<i class='fa fa-folder-open'></i> Modify</p>")
         , value = "panel2"
         , sidebarLayout(
           
           # Inputs
           sidebarPanel(
             width = 3,
             style = panel.style,
             withMathJax(),
             
             div(id = "help2_1_1"
                 , fluidRow(
                   column(12
                          , actionButton(inputId = "HELP.panel2.menu1"
                                         , label = "Need some help"
                                         , icon = icon("question-circle")
                                         , width = "100%"
                                         , style = button.style.help)
                          , br()
                          , br()
                          , br()
                          , br()
                          , div(id = "help2_1_2"
                                , textInput(inputId = "name.simul"
                                            , label = param.style("Enter the simulation name :")
                                            , value = "FATE_simulation"
                                            , width = "100%")
                                , actionButton(inputId = "create.skeleton"
                                               , label = "Create folder"
                                               , icon = icon("folder")
                                               , width = "100%"
                                               , style = button.style.action)
                          )
                   )
                 ),
                 br(),
                 helpText(HTML("
                               <ul style='font-size:75%;padding-left:20px;'>
                               <li>DATA
                               <ul>
                               <li>GLOBAL PARAMETERS</li>
                               <li>MASK</li>
                               <li>SCENARIO</li>
                               <li>SAVE</li>
                               <li>PFGS :
                               <ul>
                               <li>SUCC</li>
                               <li>DISP</li>
                               <li>HABSUIT</li>
                               <li>LIGHT</li>
                               <li>SOIL</li>
                               <li>DIST</li>
                               <li>DROUGHT</li>
                               <li>ALIENS</li>
                               </ul>
                               </li>
                               </ul>
                               </li>
                               <li>PARAM SIMUL</li>
                               <li>RESULTS</li>
                               </ul>
                               "
                 )),
                 br(),
                 br(),
                 fluidRow(
                   column(12
                          , br()
                          , div(id = "help2_1_5"
                                , shinyjs::disabled(
                                  selectInput(inputId = "load.file"
                                              , label = param.style("Simulation parameters file :")
                                              , choices = NULL
                                              , selected = NULL
                                              , multiple = F
                                              , width = "100%"
                                  )
                                  , actionButton(inputId = "load.param"
                                                 , label = "Load parameters"
                                                 , icon = icon("upload")
                                                 , width = "100%"
                                                 , style = button.style.action
                                  )
                                )
                          )
                          , br()
                          , br()
                          , br()
                          , div(id = "help2_1_4"
                                , shinyjs::disabled(
                                  actionButton(inputId = "create.simul"
                                               , label = HTML("Create Simulation <br/>parameters file")
                                               , icon = icon("file")
                                               , width = "100%"
                                               , style = button.style.action
                                  )
                                )
                          )
                          , br()
                          , div(id = "help2_1_6"
                                , shinyjs::disabled(
                                  downloadButton(outputId = "FATE_simulation.zip"
                                                 , label = "Download folder"
                                                 , icon = icon("download")
                                                 , width = "100%"
                                                 , style = button.style
                                  )
                                )
                          )
                          , br()
                          , br()
                          , br()
                          , shinyjs::disabled(
                            actionButton(inputId = "refresh"
                                         , label = "Start new folder"
                                         , icon = icon("refresh")
                                         , width = "100%"
                                         , style = button.style.action
                            )
                          )
                   )
                 )
                 ) ## END div
                 ) ## END sidebarPanel
           
           # Output
           , mainPanel(
             width = 9,
             shinyjs::hidden(
               wellPanel(id = "main.panel"
                         , style = border.style
                         , tabsetPanel(
                           source("SHINY.RFATE_UI.panel2.menu1.tab1.R", local = TRUE)$value
                           , source("SHINY.RFATE_UI.panel2.menu1.tab2.R", local = TRUE)$value
                           , source("SHINY.RFATE_UI.panel2.menu1.tab3.R", local = TRUE)$value
                           , source("SHINY.RFATE_UI.panel2.menu1.tab4.R", local = TRUE)$value
                         ) ## END tabsetPanel
               ) ## END wellPanel
             ) ## END hidden
           ) ## END mainPanel
             ) %>% helper(type = "inline"
                          , title = "Create FATE parameter files"
                          , size = "l"
                          , content = help.HTML(html.file = paste0(path.articles, "rfate_tutorial_2_params.html")
                                                , target.anchor = 'class="section level2"'
                                                , target.class = "the-different-type-of-parameters-and-flags")
             ) ## END sidebarLayout
         ) ## END tabPanel
