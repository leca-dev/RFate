
tabPanel(title = HTML("<span class='tabPanel_title'>Raster files</span>")
         , value = "panel.raster"
         , sidebarLayout(
           sidebarPanel = NULL,
           mainPanel = mainPanel(
             width = 12,
             br(),
             tabsetPanel(
               tabPanel(title = HTML("<span class='tabPanel_subtitle'>Initial</span>")
                        , value = "panel.spatial.init"
                        , fluidRow(
                          column(4
                                 , br()
                                 , wellPanel(
                                   HTML(paste0("<i class='fa fa-heart' style='font-size:15px;'></i>"
                                               , " <strong>Simulation mask</strong>"))
                                   , br()
                                   , br()
                                   , fileInput(inputId = "simul.mask"
                                               , label = NULL
                                               , multiple = FALSE
                                               , width = "100%"
                                   )
                                   , actionButton(inputId = "upload.mask"
                                                  , label = "Upload"
                                                  , icon = icon("upload")
                                                  , width = "100%"
                                                  , style = button.style.action
                                   )
                                 ) %>% helper(type = "inline"
                                              , title = "Simulation mask"
                                              , size = "l"
                                              , content = help.full(param.name.vec = c("<hr/>"
                                                                                       , "simul.mask")
                                                                    , param.desc.vec = c("<hr/>"
                                                                                         , "a <span style='font-family:Monospace;'>string</span> that corresponds to
                                                                                         the file name of a raster mask, with either 0 or 1 within each pixel, 1 corresponding to the cells of the studied area in which the succession
                                                                                         module of the FATE simulation will take place")
                                                                    ))
                                                                    )
                          , column(4
                                   , br()
                                   , wellPanel(
                                     id = "raster.dist"
                                     , HTML(paste0("<i class='fas fa-bolt' style='font-size:15px;'></i>"
                                                 , " <strong>Disturbances masks</strong>"))
                                     , br()
                                     , br()
                                     , fileInput(inputId = "dist.mask"
                                                 , label = NULL
                                                 , multiple = TRUE
                                                 , width = "100%"
                                     )
                                     , actionButton(inputId = "upload.dist.mask"
                                                    , label = "Upload"
                                                    , icon = icon("upload")
                                                    , width = "100%"
                                                    , style = button.style.action
                                     )
                                   ) %>% helper(type = "inline"
                                                , title = "Disturbance masks"
                                                , size = "l"
                                                , content = help.full(param.name.vec = c("<hr/>"
                                                                                         , "dist.mask")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                           the file name of a raster mask, with either 0 or 1 within each pixel, 1 corresponding to
                                                                                           the cells of the studied area in which the disturbance(s) will take place")
                                                                      ))
                                                                      )
                          , column(4
                                   , br()
                                   , wellPanel(
                                     id = "raster.drought"
                                     , HTML(paste0("<i class='fas fa-tint' style='font-size:15px;'></i>"
                                                 , " <strong>Drought masks</strong>"))
                                     , br()
                                     , br()
                                     , fileInput(inputId = "drought.mask"
                                                 , label = NULL
                                                 , multiple = TRUE
                                                 , width = "100%"
                                     )
                                     , actionButton(inputId = "upload.drought.mask"
                                                    , label = "Upload"
                                                    , icon = icon("upload")
                                                    , width = "100%"
                                                    , style = button.style.action
                                     )
                                   ) %>% helper(type = "inline"
                                                , title = "Drought masks"
                                                , size = "l"
                                                , content = help.full(param.name.vec = c("<hr/>"
                                                                                         , "drought.mask")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                           the file name of a raster mask, with values from 0 to 1 within each pixel, corresponding to
                                                                                           the drought intensity experienced by this pixel")
                                                                      ))
                                                                      )
                                                ) ## END fluidRow
                        , fluidRow(
                          column(4
                                 , br()
                                 , wellPanel(
                                   id = "raster.fire"
                                   , HTML(paste0("<i class='fas fa-fire' style='font-size:15px;'></i>"
                                               , " <strong>Fire masks</strong>"))
                                   , br()
                                   , br()
                                   , fileInput(inputId = "fire.mask"
                                               , label = NULL
                                               , multiple = TRUE
                                               , width = "100%"
                                   )
                                   , actionButton(inputId = "upload.fire.mask"
                                                  , label = "Upload"
                                                  , icon = icon("upload")
                                                  , width = "100%"
                                                  , style = button.style.action
                                   )
                                 ) %>% helper(type = "inline"
                                              , title = "Fire masks"
                                              , size = "l"
                                              , content = help.full(param.name.vec = c("<hr/>"
                                                                                       , "fire.mask")
                                                                    , param.desc.vec = c("<hr/>"
                                                                                         , "one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                         the file name of a raster mask, with either 0 or 1 within each pixel, 1 corresponding to
                                                                                         the cells of the studied area in which the fire(s) will take place")
                                                                    ))
                                                                    )
                          , column(4
                                   , br()
                                   , wellPanel(
                                     id = "raster.elevation"
                                     , HTML(paste0("<i class='fas fa-fire' style='font-size:15px;'></i>"
                                                 , " <strong>Elevation mask</strong>"))
                                     , br()
                                     , br()
                                     , fileInput(inputId = "elevation.mask"
                                                 , label = NULL
                                                 , multiple = TRUE
                                                 , width = "100%"
                                     )
                                     , actionButton(inputId = "upload.elevation.mask"
                                                    , label = "Upload"
                                                    , icon = icon("upload")
                                                    , width = "100%"
                                                    , style = button.style.action
                                     )
                                   ) %>% helper(type = "inline"
                                                , title = "Elevation mask"
                                                , size = "l"
                                                , content = help.full(param.name.vec = c("<hr/>"
                                                                                         , "elevation.mask")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "one <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                           the file name of a raster mask with values corresponding to altitude (digital elevation model)")
                                                                      ))
                                                )
                          , column(4
                                   , br()
                                   , wellPanel(
                                     id = "raster.slope"
                                     , HTML(paste0("<i class='fas fa-fire' style='font-size:15px;'></i>"
                                                 , " <strong>Slope mask</strong>"))
                                     , br()
                                     , br()
                                     , fileInput(inputId = "slope.mask"
                                                 , label = NULL
                                                 , multiple = TRUE
                                                 , width = "100%"
                                     )
                                     , actionButton(inputId = "upload.slope.mask"
                                                    , label = "Upload"
                                                    , icon = icon("upload")
                                                    , width = "100%"
                                                    , style = button.style.action
                                     )
                                   ) %>% helper(type = "inline"
                                                , title = "Slope mask"
                                                , size = "l"
                                                , content = help.full(param.name.vec = c("<hr/>"
                                                                                         , "slope.mask")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "one <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                           the file name of a raster mask with values corresponding to slope")
                                                                      ))
                                                )
                          ) ## END fluidRow
                        , fluidRow(
                          column(4
                                 , br()
                                 , wellPanel(
                                   id = "raster.habsuit"
                                   , HTML(paste0("<i class='fas fa-globe' style='font-size:15px;'></i>"
                                               , " <strong>Habitat suitability masks</strong>"))
                                   , br()
                                   , br()
                                   , textInput(inputId = "habsuit.folder"
                                               , label = param.style("habsuit.folder")
                                               , width = "100%"
                                   )
                                   , fileInput(inputId = "habsuit.mask"
                                               , label = param.style("habsuit.mask")
                                               , multiple = TRUE
                                               , width = "100%"
                                   )
                                   , actionButton(inputId = "upload.habsuit.mask"
                                                  , label = "Upload"
                                                  , icon = icon("upload")
                                                  , width = "100%"
                                                  , style = button.style.action
                                   )
                                 ) %>% helper(type = "inline"
                                              , title = "Habitat suitability masks"
                                              , size = "l"
                                              , content = help.full(param.name.vec = c("<hr/>"
                                                                                       , "habsuit.folder"
                                                                                       , "habsuit.mask")
                                                                    , param.desc.vec = c("<hr/>"
                                                                                         , "<em>(optional) a string that corresponds to the name of the folder
                                                                                         that will be created into the <span style='font-family:Monospace;'>name.simulation/DATA/PFGS/HABSUIT/</span> directory to store the habitat suitability maps"
                                                                                         , "one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                         the file name(s) of a raster mask, with either 0 or 1 within each pixel, 1 corresponding to
                                                                                         the cells of the studied area in which aliens introduction will take place")
                                                                    ))
                                                                    )
                          , column(4
                                   , br()
                                   , wellPanel(
                                     id = "raster.aliens"
                                     , HTML(paste0("<i class='fab fa-reddit-alien' style='font-size:15px;'></i>"
                                               , " <strong>Aliens masks</strong>"))
                                     , br()
                                     , br()
                                     , textInput(inputId = "aliens.folder"
                                                 , label = param.style("aliens.folder")
                                                 , width = "100%"
                                     )
                                     , fileInput(inputId = "aliens.mask"
                                                 , label = param.style("aliens.mask")
                                                 , multiple = TRUE
                                                 , width = "100%"
                                     )
                                     , actionButton(inputId = "upload.aliens.mask"
                                                    , label = "Upload"
                                                    , icon = icon("upload")
                                                    , width = "100%"
                                                    , style = button.style.action
                                     )
                                   ) %>% helper(type = "inline"
                                                , title = "Aliens masks"
                                                , size = "l"
                                                , content = help.full(param.name.vec = c("<hr/>"
                                                                                         , "aliens.folder"
                                                                                         , "aliens.mask")
                                                                      , param.desc.vec = c("<hr/>"
                                                                                           , "<em>(optional) a string that corresponds to the name of the folder
                                                                                           that will be created into the <span style='font-family:Monospace;'>name.simulation/DATA/PFGS/ALIENS/</span> directory to store the aliens maps"
                                                                                           , "one or several <span style='font-family:Monospace;'>string</span> corresponding to
                                                                                           the file name(s) of a raster mask, with values from 0 to 1 within each pixel, corresponding to the probability to find a specific PFG within this pixel")
                                                                      ))
                                                )
                                   ) ## END fluidRow
                                   ) ## END tabPanel (Initial)
               , tabPanel(title = HTML("<span class='tabPanel_subtitle'>Changing</span>")
                          , value = "panel.spatial.changing"
                          , fluidRow(
                            column(6
                                   , br()
                                   , shinyjs::disabled(
                                     actionButton(inputId = "add.changing"
                                                  , label = "Add changing year"
                                                  , icon = icon("plus")
                                                  , width = "100%"
                                                  , style = button.style.action
                                     )
                                   )
                            )
                            , column(6
                                     , br()
                                     , shinyjs::disabled(
                                       actionButton(inputId = "create.changing"
                                                    , label = "Create Scenario files"
                                                    , icon = icon("file")
                                                    , width = "100%"
                                                    , style = button.style.action
                                       ) %>% helper(type = "inline"
                                                    , title = "Create Scenario files"
                                                    , size = "l"
                                                    , content = help.HTML(paste0(path.reference, "PRE_FATE.params_changingYears.html"))
                                       )
                                     )
                            )
                          )
                          , fluidRow(
                            column(4
                                   , br()
                                   , br()
                                   , HTML("<em>opt.folder.name</em>")
                                   , textInput(inputId = "changing.folder"
                                               , label = NULL
                                               , width = "100%"))
                            , column(4
                                     , br()
                                     , br()
                                     , HTML("<strong>Type</strong>")
                                     , selectInput(inputId = "type.changing"
                                                   , label = NULL
                                                   , choices = c("MASK", "HABSUIT", "DIST", "DROUGHT"
                                                                 , "ALIENS", "ALIENS_F", "FIRE", "FIRE_F")
                                                   , selected = "MASK"
                                                   , multiple = FALSE
                                                   , width = "100%"))
                            , column(4
                                     , br()
                                     , br()
                                     , br()
                                     , actionButton(inputId = "refresh.changing"
                                                    , label = "Get available files"
                                                    , icon = icon("refresh")
                                                    , width = "100%"
                                                    , style = button.style.action
                                     ))
                          )
                          , fluidRow(
                            column(2
                                   , br()
                                   , HTML("<strong>Year</strong>")
                                   , numericInput(inputId = "changing.year"
                                                  , label = NULL
                                                  , value = 1
                                                  , min = 1
                                                  , width = "100%"))
                            , column(2
                                     , br()
                                     , HTML("<strong>Order</strong>")
                                     , numericInput(inputId = "changing.order"
                                                    , label = NULL
                                                    , value = 1
                                                    , min = 1
                                                    , width = "100%"))
                            , column(8
                                     , br()
                                     , HTML("<strong>File</strong>")
                                     , selectInput(inputId = "changing.file"
                                                   , label = NULL
                                                   , choices = NULL
                                                   , selected = NULL
                                                   , multiple = TRUE
                                                   , width = "100%")
                            )
                          )
                          , fluidRow(
                            column(10
                                   , br()
                                   , br()
                                   , wellPanel(style = "overflow-x:scroll;"
                                               , tableOutput(outputId = "mat.changing")))
                            , column(2
                                     , br()
                                     , actionButton(inputId = "delete.changing"
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
                                                 , uiOutput(outputId = "UI.files.changing")))
                            , column(12
                                     , wellPanel(style = panel.style.scrollX
                                                 , dataTableOutput(outputId = "created_table.changing"))
                            )
                          )
               ) ## END tabPanel (Changing)
                                              ) ## END tabsetPanel
                                              ) ## END mainPanel
                          ) ## END sidebarLayout
         ) ## END tabPanel (Raster files)
