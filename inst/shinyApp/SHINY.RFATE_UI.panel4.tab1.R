
tabPanel(title = HTML("<span class='tabPanel_title'>BROWSER</span>")
         , value = "panel.browser"
         , fluidRow(
           column(4
                  , br()
                  , checkboxInput(inputId = "browser.evolution"
                                  , label = "Temporal evolution"
                                  , value = TRUE
                                  , width = "100%")
           )
           , column(4
                    , br()
                    , checkboxInput(inputId = "browser.validation"
                                    , label = "Validation"
                                    , value = TRUE
                                    , width = "100%")
           )
           , column(4
                    , br()
                    , checkboxInput(inputId = "browser.map"
                                    , label = "Maps"
                                    , value = TRUE
                                    , width = "100%")
           )
         )
         , fluidRow(
           column(12
                  , br()
                  , shinyjs::disabled(
                    selectInput(inputId = "browser.files"
                                , label = param.style("Select the graphic to display :")
                                , choices = NULL
                                , selected = NULL
                                , multiple = F
                                , width = "100%")
                  )
           )
         )
         , fluidRow(
           column(11
                    , br()
                    , uiOutput(outputId = "UI.plot.browser")
           )
           , column(1
                    , br()
                    , br()
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "go.left"
                                   , label = ""
                                   , icon = icon("arrow-circle-left")
                                   , width = "100%"
                                   , style = button.style.help)
                    )
                    , br()
                    , br()
                    , shinyjs::disabled(
                      actionButton(inputId = "go.right"
                                   , label = ""
                                   , icon = icon("arrow-circle-right")
                                   , width = "100%"
                                   , style = button.style.help)
                    )
           )
         )
) ## END tabPanel (Global parameters)
