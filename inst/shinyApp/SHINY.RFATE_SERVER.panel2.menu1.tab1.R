
observeEvent(input$required.simul_duration, {
  updateNumericInput(session
                     , inputId = "save.maps.year2"
                     , value = input$required.simul_duration)
})

####################################################################

output$UI.doDispersal = renderUI({
  if (input$doDispersal)
  {
    selectInput(inputId = "DISPERSAL.mode"
                , label = ""
                , choices = c("(1) uniform kernel"
                              , "(2) exponential kernel"
                              , "(3) exponential kernel with probability")
                , selected = "(1) uniform kernel"
                , multiple = FALSE
                , width = "100%")
  } 
})

observeEvent(input$doDispersal, {
  if (input$doDispersal)
  {
    showTab(inputId = "panel.PFG_sub", target = "panel.disp", select = TRUE)
  } else
  {
    hideTab(inputId = "panel.PFG_sub", target = "panel.disp")
  }
})

####################################################################

output$UI.doHabSuitability = renderUI({
  if (input$doHabSuitability)
  {
    selectInput(inputId = "HABSUIT.mode"
                , label = ""
                , choices = c("(1) random", "(2) PFG specific")
                , selected = "(1) random"
                , multiple = FALSE
                , width = "100%")
  } 
})

observeEvent(input$doHabSuitability, {
  if (input$doHabSuitability)
  {
    shinyjs::enable("raster.habsuit")
  } else
  {
    shinyjs::disable("raster.habsuit")
  }
})

####################################################################

output$UI.doLight = renderUI({
  if (input$doLight)
  {
    column(12
           , numericInput(inputId = "LIGHT.thresh_medium"
                          , label = param.style("LIGHT.thresh_medium")
                          , min = 1
                          , value = 5000
                          , width = "100%")
           , numericInput(inputId = "LIGHT.thresh_low"
                          , label = param.style("LIGHT.thresh_low")
                          , min = 1
                          , value = 8000
                          , width = "100%")
    )
  }
})

observeEvent(input$doLight, {
  if (input$doLight)
  {
    showTab(inputId = "panel.PFG_sub", target = "panel.light", select = TRUE)
  } else
  {
    hideTab(inputId = "panel.PFG_sub", target = "panel.light")
  }
})

####################################################################

output$UI.doSoil = renderUI({
  if (input$doSoil)
  {
    column(12
           , numericInput(inputId = "SOIL.init"
                          , label = param.style("SOIL.init")
                          , min = 1
                          , value = 1
                          , width = "100%")
           , sliderInput(inputId = "SOIL.retention"
                         , label = param.style("SOIL.retention")
                         , min = 0
                         , max = 1
                         , value = 0.8
                         , step = 0.05
                         , width = "100%")
    )
  }
})

observeEvent(input$doSoil, {
  if (input$doSoil)
  {
    showTab(inputId = "panel.PFG_sub", target = "panel.soil", select = TRUE)
  } else
  {
    hideTab(inputId = "panel.PFG_sub", target = "panel.soil")
  }
})

####################################################################

output$UI.doDisturbances = renderUI({
  if (input$doDisturbances)
  {
    column(12
           , numericInput(inputId = "DIST.no"
                          , label = param.style("DIST.no")
                          , min = 1
                          , value = 1
                          , width = "100%")
           , numericInput(inputId = "DIST.no_sub"
                          , label = param.style("DIST.no_sub")
                          , min = 4
                          , max = 4
                          , value = 1
                          , width = "100%")
           , numericInput(inputId = "DIST.freq"
                          , label = param.style("DIST.freq")
                          , min = 1
                          , value = 1
                          , width = "100%")
    )
  }
})

observeEvent(input$doDisturbances, {
  if (input$doDisturbances)
  {
    showTab(inputId = "panel.PFG_sub", target = "panel.dist", select = TRUE)
    shinyjs::enable("raster.dist")
  } else
  {
    hideTab(inputId = "panel.PFG_sub", target = "panel.dist")
    shinyjs::disable("raster.dist")
  }
})

####################################################################

output$UI.doDrought = renderUI({
  if (input$doDrought)
  {
    column(12
           , numericInput(inputId = "DROUGHT.no_sub"
                          , label = param.style("DROUGHT.no_sub")
                          , min = 4
                          , max = 4
                          , value = 1
                          , width = "100%")
    )
  }
})

observeEvent(input$doDrought, {
  if (input$doDrought)
  {
    showTab(inputId = "panel.PFG_sub", target = "panel.drought", select = TRUE)
    shinyjs::enable("raster.drought")
  } else
  {
    hideTab(inputId = "panel.PFG_sub", target = "panel.drought")
    shinyjs::disable("raster.drought")
  }
})

####################################################################

output$UI.doAliens = renderUI({
  if (input$doAliens)
  {
    column(12
           , numericInput(inputId = "ALIENS.no"
                          , label = param.style("ALIENS.no")
                          , min = 1
                          , value = 1
                          , width = "100%")
           , numericInput(inputId = "ALIENS.freq"
                          , label = param.style("ALIENS.freq")
                          , min = 1
                          , value = 1
                          , width = "100%")
    )
  }
})

observeEvent(input$doAliens, {
  if (input$doAliens)
  {
    shinyjs::enable("raster.aliens")
  } else
  {
    shinyjs::disable("raster.aliens")
  }
})

####################################################################

output$UI.doFire1 = renderUI({
  if (input$doFire)
  {
    column(12
           , numericInput(inputId = "FIRE.no"
                          , label = param.style("FIRE.no")
                          , min = 1
                          , value = 1
                          , width = "100%")
           , numericInput(inputId = "FIRE.no_sub"
                          , label = param.style("FIRE.no_sub")
                          , min = 4
                          , max = 4
                          , value = 1
                          , width = "100%")
           , numericInput(inputId = "FIRE.freq"
                          , label = param.style("FIRE.freq")
                          , min = 1
                          , value = 1
                          , width = "100%")
    )
  }
})

output$UI.doFire2.1 = renderUI({
  if (input$doFire)
  {
    fluidRow(
      column(6
             , selectInput(inputId = "FIRE.ignit_mode"
                           , label = param.style("FIRE.ignit_mode")
                           , choices = c("(1) random (fixed)"
                                         , "(2) random (normal distribution)"
                                         , "(3) random (historic distribution)"
                                         , "(4) probability (Li et al. 1997)"
                                         , "(5) map")
                           , selected = "(1) random (fixed)"
                           , multiple = FALSE
                           , width = "100%")
      )
      , column(6, uiOutput(outputId = "UI.doFire2.1bis"))
    )
  }
})

output$UI.doFire2.1bis = renderUI({
  switch (input$FIRE.ignit_mode
          , "(3) random (historic distribution)" = fluidRow(column(12, textAreaInput(inputId = "FIRE.ignit_noHist"
                                                                                     , label = param.style("FIRE.ignit_noHist")
                                                                                     , width = "100%")))
          , "(4) probability (Li et al. 1997)" = fluidRow(
            column(12,
                   fluidRow(
                     column(4, numericInput(inputId = "FIRE.ignit_logis1"
                                            , label = param.style("FIRE.ignit_logis")
                                            , value = 0.6
                                            , width = "100%"))
                     , column(4, numericInput(inputId = "FIRE.ignit_logis2"
                                              , label = param.style(".")
                                              , value = 2.5
                                              , width = "100%"))
                     , column(4, numericInput(inputId = "FIRE.ignit_logis3"
                                              , label = param.style(".")
                                              , value = 0.05
                                              , width = "100%"))
                   )
                   , fluidRow(column(12, numericInput(inputId = "FIRE.ignit_flammMax"
                                                      , label = param.style("FIRE.ignit_flammMax")
                                                      , min = 1
                                                      , value = 10
                                                      , width = "100%")))
            ))
          , "(5) map" = ""
          , fluidRow(column(12, numericInput(inputId = "FIRE.ignit_no"
                                             , label = param.style("FIRE.ignit_no")
                                             , min = 1
                                             , value = 10
                                             , width = "100%")))
  )
})

output$UI.doFire2.2 = renderUI({
  req(input$FIRE.ignit_mode != "(5) map")
  if (input$doFire)
  {
    fluidRow(
      column(6
             , selectInput(inputId = "FIRE.neigh_mode"
                           , label = param.style("FIRE.neigh_mode")
                           , choices = c("(1) 8 neighbours"
                                         , "(2) extent (fixed)"
                                         , "(3) extent (random)")
                           , selected = "(1) 8 neighbours"
                           , multiple = FALSE
                           , width = "100%")
      )
      , column(6, uiOutput(outputId = "UI.doFire2.2bis"))
    )
  }
})

output$UI.doFire2.2bis = renderUI({
  switch (input$FIRE.neigh_mode
          , "(1) 8 neighbours" = ""
          , fluidRow(
            column(3, numericInput(inputId = "FIRE.neigh_CC1"
                                   , label = param.style("north")
                                   , min = 1
                                   , value = 2
                                   , width = "100%"))
            , column(3, numericInput(inputId = "FIRE.neigh_CC2"
                                     , label = param.style("east")
                                     , min = 1
                                     , value = 2
                                     , width = "100%"))
            , column(3, numericInput(inputId = "FIRE.neigh_CC3"
                                     , label = param.style("south")
                                     , min = 1
                                     , value = 2
                                     , width = "100%"))
            , column(3, numericInput(inputId = "FIRE.neigh_CC4"
                                     , label = param.style("west")
                                     , min = 1
                                     , value = 2
                                     , width = "100%"))
          )
  )
})

output$UI.doFire2.3 = renderUI({
  req(input$FIRE.ignit_mode != "(5) map")
  if (input$doFire)
  {
    fluidRow(
      column(6
             , selectInput(inputId = "FIRE.prop_mode"
                           , label = param.style("FIRE.prop_mode")
                           , choices = unlist(ifelse(input$FIRE.ignit_mode == "(4) probability (Li et al. 1997)"
                                                     , "(5) probability (Li et al. 1997)"
                                                     , ifelse(input$doSoil
                                                              , list(c("(1) probability (fire intensity)"
                                                                       , "(2) probability (% of plants consumed)"
                                                                       , "(3) maximum amount (PFG)"
                                                                       , "(4) maximum amount (soil)"
                                                                       , "(5) probability (Li et al. 1997)"))
                                                              , list(c("(1) probability (fire intensity)"
                                                                       , "(2) probability (% of plants consumed)"
                                                                       , "(3) maximum amount (PFG)"
                                                                       , "(5) probability (Li et al. 1997)"))
                                                     )))
                           , selected = "(1) probability (fire intensity)"
                           , multiple = FALSE
                           , width = "100%")
      )
      , column(6, uiOutput(outputId = "UI.doFire2.3bis"))
    )
  }
})

output$UI.doFire2.3bis = renderUI({
  switch (input$FIRE.prop_mode
          , "(1) probability (fire intensity)" = fluidRow(column(12, textAreaInput(inputId = "FIRE.prop_intensity"
                                                                                   , label = param.style("FIRE.prop_intensity")
                                                                                   , width = "100%")))
          , "(5) probability (Li et al. 1997)" = fluidRow(
            column(4, numericInput(inputId = "FIRE.prop_logis1"
                                   , label = param.style("FIRE.prop_logis")
                                   , value = 0.6
                                   , width = "100%"))
            , column(4, numericInput(inputId = "FIRE.prop_logis2"
                                     , label = param.style(".")
                                     , value = 2.5
                                     , width = "100%"))
            , column(4, numericInput(inputId = "FIRE.prop_logis3"
                                     , label = param.style(".")
                                     , value = 0.05
                                     , width = "100%"))
          )
          , ""
  )
})

output$UI.doFire2.4 = renderUI({
  req(input$FIRE.ignit_mode != "(5) map")
  if (input$doFire)
  {
    fluidRow(
      column(6
             , selectInput(inputId = "FIRE.quota_mode"
                           , label = param.style("FIRE.quota_mode")
                           , choices = c("(1) maximum step"
                                         , "(2) maximum amount"
                                         , "(3) maximum cells"
                                         , "(4) keep going")
                           , selected = "(4) keep going"
                           , multiple = FALSE
                           , width = "100%")
      )
      , column(6, uiOutput(outputId = "UI.doFire2.4bis"))
    )
  }
})

output$UI.doFire2.4bis = renderUI({
  switch (input$FIRE.quota_mode
          , "(4) keep going" = ""
          , fluidRow(column(12, numericInput(inputId = "FIRE.quota_max"
                                             , label = param.style("FIRE.quota_max")
                                             , min = 1
                                             , value = 100
                                             , width = "100%")))
  )
})

observeEvent(input$doFire, {
  if (input$doFire)
  {
    shinyjs::enable("raster.fire")
    shinyjs::enable("raster.elevation")
    shinyjs::enable("raster.slope")
  } else
  {
    shinyjs::disable("raster.fire")
    shinyjs::disable("raster.elevation")
    shinyjs::disable("raster.slope")  }
})


####################################################################

observeEvent(input$create.global, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_globalParameters(name.simulation = input$name.simul
                                       , opt.no_CPU = input$opt.no_CPU
                                       , required.no_PFG = input$required.no_PFG
                                       , required.no_strata = input$required.no_strata
                                       , required.simul_duration = input$required.simul_duration
                                       , required.seeding_duration = input$required.seeding_duration
                                       , required.seeding_timestep = input$required.seeding_timestep
                                       , required.seeding_input = input$required.seeding_input
                                       , required.max_abund_low = input$required.max_abund_low
                                       , required.max_abund_medium = input$required.max_abund_medium
                                       , required.max_abund_high = input$required.max_abund_high
                                       , doDispersal = input$doDispersal
                                       , DISPERSAL.mode = as.vector(c("(1) uniform kernel" = 1
                                                                      , "(2) exponential kernel" = 2
                                                                      , "(3) exponential kernel with probability" = 3)[input$DISPERSAL.mode])
                                       , doHabSuitability = input$doHabSuitability
                                       , HABSUIT.mode = ifelse(input$HABSUIT.mode == "(1) random", 1, 2)
                                       , doLight = input$doLight
                                       , LIGHT.thresh_medium = input$LIGHT.thresh_medium
                                       , LIGHT.thresh_low = input$LIGHT.thresh_low
                                       , doSoil = input$doSoil
                                       , SOIL.init = input$SOIL.init
                                       , SOIL.retention = input$SOIL.retention
                                       , doDisturbances = input$doDisturbances
                                       , DIST.no = input$DIST.no
                                       , DIST.no_sub = input$DIST.no_sub
                                       , DIST.freq = rep(input$DIST.freq, input$DIST.no)
                                       , doDrought = input$doDrought
                                       , DROUGHT.no_sub = input$DROUGHT.no_sub
                                       , doAliens = input$doAliens
                                       , ALIEN.no = input$ALIENS.no
                                       , ALIEN.freq = input$ALIENS.freq
                                       , doFire = input$doFire
                                       , FIRE.no = input$FIRE.no
                                       , FIRE.no_sub = input$FIRE.no_sub
                                       , FIRE.freq = input$FIRE.freq
                                       , FIRE.ignit_mode = switch (input$FIRE.ignit_mode
                                                                   , "(1) random (fixed)" = 1
                                                                   , "(2) random (normal distribution)" = 2
                                                                   , "(3) random (historic distribution)" = 3
                                                                   , "(4) probability (Li et al. 1997)" = 4
                                                                   , "(5) map" = 5)
                                       , FIRE.ignit_no = input$FIRE.ignit_no
                                       , FIRE.ignit_noHist = input$FIRE.ignit_noHist
                                       , FIRE.ignit_logis = c(input$FIRE.ignit_logis1, input$FIRE.ignit_logis2, input$FIRE.ignit_logis3)
                                       , FIRE.ignit_flammMax = input$FIRE.ignit_flammMax
                                       , FIRE.neigh_mode = switch (input$FIRE.neigh_mode
                                                                   , "(1) 8 neighbours" = 1
                                                                   , "(2) extent (fixed)" =2
                                                                   , "(3) extent (random)" = 3)
                                       , FIRE.neigh_CC = input$FIRE.neigh_CC
                                       , FIRE.prop_mode = switch (input$FIRE.prop_mode
                                                                  , "(1) probability (fire intensity)" = 1
                                                                  , "(2) probability (% of plants consumed)" = 2
                                                                  , "(3) maximum amount (PFG)" = 3
                                                                  , "(4) maximum amount (soil)" = 4
                                                                  , "(5) probability (Li et al. 1997)" = 5)
                                       , FIRE.prop_intensity = input$FIRE.prop_intensity
                                       , FIRE.prop_logis = c(input$FIRE.prop_logis1, input$FIRE.prop_logis2, input$FIRE.prop_logis3)
                                       , FIRE.quota_mode = switch (input$FIRE.quota_mode
                                                                   , "(1) maximum step" = 1
                                                                   , "(2) maximum amount" = 2
                                                                   , "(3) maximum cells" = 3
                                                                   , "(4) keep going" = 4)
                                       , FIRE.quota_max = input$FIRE.quota_max
                                       
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/"))
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.global = eventReactive(paste(input$name.simul
                                     , input$create.global
                                     , RV$compt.global.no), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/")
                                         tab = get_files(path_folder)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.global.no = ncol(tab)
                                           RV$compt.global.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.global = renderUI({
  tab = get_tab.global()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.global.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.global.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.global.select"
                                , label = "Delete selected"
                                , icon = icon("trash-alt")
                                , width = "100%"
                                , style = button.style.action))
      ),
      hr(),
      fluidRow(
        column(10
               , lapply(1:ncol(tab)
                        , function(i) {
                          checkboxInput(inputId = paste0("check.global.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        , column(2
                 , lapply(1:ncol(tab)
                          , function(i) {
                            actionButton(inputId = paste0("upload.global.", colnames(tab)[i])
                                         , label = NULL
                                         , icon = icon("upload")
                                         , width = "100%"
                                         , style = button.style.action)
                          })
        )
      )
    )
  }
})

observeEvent(RV$compt.global.no, {
  for (i in 1:RV$compt.global.no)
  {
    observeEvent(input[[paste0("upload.global.", RV$compt.global.files[i])]], {
      get_update.global(file.globalParam = paste0(input$name.simul
                                                  , "/DATA/GLOBAL_PARAMETERS/"
                                                  , RV$compt.global.files[i]))
    })
  }
})


observeEvent(input$check.global.all, {
  for (col_tab in RV$compt.global.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.global.", col_tab)
                        , value = input$check.global.all)
  }
})

observeEvent(input$view.global.select, {
  output$created_table.global = renderDataTable({
    req(grep(pattern = "check.global.", x = names(input), value = TRUE))
    
    tab = get_tab.global()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.global.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.global.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.global.select, {
  if (input$check.global.all)
  {
    col_toKeep = rep(TRUE,RV$compt.global.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.global.no, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.global.", RV$compt.global.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.globalParam = RV$compt.global.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/ \n")
                               , paste0(gsub("__", "/", file.globalParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.globalParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/GLOBAL_PARAMETERS/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.global.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.global.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.global.no = min(0, RV$compt.global.no - sum(col_toKeep))
                 }
               })
  }
})

