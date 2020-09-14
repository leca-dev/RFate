
tabPanel(title = HTML("<span class='tabPanel_title'>1. Dominant species</span>")
         , value = "panel.dominant"
         , fluidRow(
           column(3
                  , br()
                  , checkboxInput(inputId = "doRuleA"
                                  , label = param.style("doRuleA")
                                  , value = FALSE
                                  , width = "100%")
           )
           , column(3
                    , br()
                    , checkboxInput(inputId = "doRuleC"
                                    , label = param.style("doRuleC")
                                    , value = FALSE
                                    , width = "100%")
           )
           , column(6
                    , br()
                    , checkboxInput(inputId = "doRuleB"
                                    , label = param.style("doRuleB")
                                    , value = FALSE
                                    , width = "100%")
           )
         ) ## END fluidRow
         , fluidRow(
           column(3
                  , checkboxInput(inputId = "doRobustness"
                                  , label = param.style("doRobustness")
                                  , value = FALSE
                                  , width = "100%")
           )
           , column(9, br())
         ) ## END fluidRow
         , fluidRow(
           column(6
                  , br()
                  , uiOutput(outputId = "UI.doRuleAC")
           )
           , column(6
                    , br()
                    , uiOutput(outputId = "UI.doRuleB")
           )
         ) ## END fluidRow
         , fluidRow(
           column(12
                  , br()
                  , uiOutput(outputId = "UI.doRobustness")
           )
         ) ## END fluidRow
         , fluidRow(
           column(12
                  , br()
                  , actionButton(inputId = "select.dominant"
                                 , label = "Select dominant species"
                                 , icon = icon("list")
                                 , width = "100%"
                                 , style = button.style.action
                  ) %>% helper(type = "inline"
                               , title = "Selection of dominant species"
                               , size = "l"
                               , content = help.HTML(paste0(path.reference, "PRE_FATE.selectDominant.html"))
                  )
           )
         ) ## END fluidRow
) ## END tabPanel (Dominant species)
