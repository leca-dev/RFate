
tabPanel(title = HTML("<span class='tabPanel_title'>3. Hierarchical clustering</span>")
         , value = "panel.clustering"
         , fluidRow(
           column(12
                  , br()
                  , radioButtons(inputId = "choice.distance"
                                 , label = param.style("Use species distance...")
                                 , choices = c("from distance computation"
                                               , "from file")
                                 , selected = "from distance computation"
                                 , inline = TRUE
                                 , width = "100%")
                  , br()
                  , uiOutput(outputId = "UI.species.distance")
                  , br()
                  , uiOutput(outputId = "UI.no.clusters")
           )
         ) ## END fluidRow
         , fluidRow(
           column(12
                  , br()
                  , actionButton(inputId = "clustering.step1"
                                 , label = "Build dendrograms"
                                 , icon = icon("bezier-curve")
                                 , width = "100%"
                                 , style = button.style.action
                  ) %>% helper(type = "inline"
                               , title = "Clustering : build dendrograms"
                               , size = "l"
                               , content = help.HTML(paste0(path.reference, "PRE_FATE.speciesClustering_step1.html"))
                  )
                  , br()
                  , shinyjs::disabled(
                    actionButton(inputId = "clustering.step2"
                                 , label = "Build PFG"
                                 , icon = icon("object-group")
                                 , width = "100%"
                                 , style = button.style.action
                    ) %>% helper(type = "inline"
                                 , title = "Clustering : build PFG"
                                 , size = "l"
                                 , content = help.HTML(paste0(path.reference, "PRE_FATE.speciesClustering_step2.html"))
                    )
                  )
                  , br()
                  , shinyjs::disabled(
                    actionButton(inputId = "clustering.step3"
                                 , label = "Calculate PFG traits values"
                                 , icon = icon("clipboard-list")
                                 , width = "100%"
                                 , style = button.style.action
                    ) %>% helper(type = "inline"
                                 , title = "Clustering : calculate PFG traits values"
                                 , size = "l"
                                 , content = help.HTML(paste0(path.reference, "PRE_FATE.speciesClustering_step3.html"))
                    )
                  )
           )
         ) ## END fluidRow
) ## END tabPanel (Clustering)
