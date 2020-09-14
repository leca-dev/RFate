
####################################################################

observeEvent(input$show.spatial_maps, {
  if (input$show.spatial_maps == "Validation statistics")
  {
    shinyjs::show("panel.validationStat")
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::hide("panel.PFGmap")
  } else if (input$show.spatial_maps == "Maps : PFG vs HS")
  {
    shinyjs::hide("panel.validationStat")
    shinyjs::show("panel.PFGvsHS")
    shinyjs::hide("panel.PFGmap")
  } else if (input$show.spatial_maps == "Maps : PFG cover, richness, CWM")
  {
    shinyjs::hide("panel.validationStat")
    shinyjs::hide("panel.PFGvsHS")
    shinyjs::show("panel.PFGmap")
  }
})

####################################################################

output$UI.graph.mat.PFG.obs = renderUI({
  shinyjs::enable("create.temporalEvolution")
  shinyjs::enable("create.evolutionCoverage")
  shinyjs::enable("create.evolutionPixels")
  shinyjs::enable("create.evolutionStability")
  shinyjs::enable("create.relativeAbund")
  shinyjs::enable("create.validationStat")
  shinyjs::enable("create.binaryMaps")
  shinyjs::enable("create.PFGvsHS")
  shinyjs::enable("create.PFGmap")
  
  fileInput(inputId = "graph.mat.PFG.obs"
            , label = NULL
            , buttonLabel = param.style("mat.PFG.obs")
            , multiple = FALSE
            , width = "100%")
})

observeEvent(input$graph.mat.PFG.obs.delete, {
  output$UI.graph.mat.PFG.obs = renderUI({
    fileInput(inputId = "graph.mat.PFG.obs"
              , label = NULL
              , buttonLabel = param.style("mat.PFG.obs")
              , multiple = FALSE
              , width = "100%")
  })
})

####################################################################

output$UI.graph.binMethod.opt = renderUI({
  if (input$graph.binMethod == "(1) fixed threshold")
  {
    sliderInput(inputId = "graph.binMethod.1"
                , label = NULL
                , min = 0
                , max = 1
                , value = 0.05
                , step = 0.05
                , width = "100%")
  } else if (input$graph.binMethod == "(2) optimizing TSS")
  {
    fileInput(inputId = "graph.binMethod.2"
              , label = NULL
              , multiple = FALSE
              , width = "100%")
  }
})

  
####################################################################

observeEvent(input$create.relativeAbund, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating relative PFG abundance maps with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()), "</li>"
                                    , "<li><strong>simulation parameter file :</strong> "
                                    , basename(get_param.simul()), "</li>"
                                    , "<li><strong>year(s) :</strong> ", paste0(as.numeric(input$graph.year), collapse = " ; "), "</li>"
                                    , "<li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU, "</li>"
                                    , "</ul>"))
                        , title = "Relative PFG abundance maps (specific year)"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.relativeAbund(name.simulation = get_name.simul()
                            , file.simulParam = get_param.simul()
                            , year = as.numeric(input$graph.year)
                            , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  removeModal()

  setwd(path.init)
})


####################################################################

observeEvent(input$create.binaryMaps, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating PFG binary maps with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()), "</li>"
                                    , "<li><strong>simulation parameter file :</strong> "
                                    , basename(get_param.simul()), "</li>"
                                    , "<li><strong>year(s) :</strong> ", paste0(as.numeric(input$graph.year), collapse = " ; "), "</li>"
                                    , "<li><strong>method :</strong> ", input$graph.binMethod, "</li>"
                                    , "<li><strong>method1.threshold :</strong> ", input$graph.binMethod.1, "</li>"
                                    , "<li><strong>method2.cutoff :</strong> ", input$graph.binMethod.2, "</li>"
                                    , "<li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU, "</li>"
                                    , "</ul>"))
                        , title = "PFG binary maps (specific year)"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.binaryMaps(name.simulation = get_name.simul()
                         , file.simulParam = get_param.simul()
                         , years = as.numeric(input$graph.year)
                         , method = as.vector(c("(1) fixed threshold" = 1
                                                , "(2) optimizing TSS" = 2)[input$graph.binMethod])
                         , method1.threshold = input$graph.binMethod.1
                         , method2.cutoff = input$graph.binMethod.2
                         , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  removeModal()
  
  setwd(path.init)
})

####################################################################

observeEvent(input$create.validationStat, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  graph.mat.PFG.obs = graph.opt.ras_habitat = ""
  if (is.data.frame(input$graph.mat.PFG.obs))
  {
    graph.mat.PFG.obs = fread(input$graph.mat.PFG.obs$datapath)
	graph.mat.PFG.obs = as.data.frame(graph.mat.PFG.obs, stringsAsFactors = FALSE)
  }
  if (is.data.frame(input$graph.opt.ras_habitat))
  {
    graph.opt.ras_habitat = input$graph.opt.ras_habitat$datapath
  }
  
  showModal(modalDialog(HTML(paste0("Creating statistics for quality assessment (sensitivity, specificity, TSS, AUC) with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()), "</li>"
                                    , "<li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>"
                                    , "<li><strong>year :</strong> ", as.numeric(input$graph.year),"</li>"
                                    , "<li><strong>mat.PFG.obs :</strong> ", is.data.frame(graph.mat.PFG.obs),"</li>"
                                    , "<li><strong>opt.ras_habitat :</strong> ", graph.opt.ras_habitat,"</li>"
                                    , "<li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU,"</li>"
                                    , "</ul>"))
                        , title = "Statistics for quality assessment (specific year)"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_validationStatistics(name.simulation = get_name.simul()
                                           , file.simulParam = get_param.simul()
                                           , years = as.numeric(input$graph.year)
                                           , mat.PFG.obs = graph.mat.PFG.obs
                                           , opt.ras_habitat = graph.opt.ras_habitat
    )
  ))
  removeModal()
  
  print(str(get_res))
  res.plot = get_res[[1]] ## simulParam
  if (is.list(res.plot) && length(res.plot) == 2 && names(res.plot)[2] == "plot") { ## tab and plot
    res.plot = res.plot$plot
    if (is.list(res.plot) && length(res.plot) > 0) { ## years
      res.plot = res.plot[[1]]
      if (is.list(res.plot) && length(res.plot) > 0) { ## habitat
        res.plot = res.plot[[1]]
        if (!is.null(res.plot)) {
          output$plot.validationStat = renderPlot({ plot(res.plot) })
        }
      }
    }
  }

  # shinyjs::show("plot.validationStat")
  
  setwd(path.init)
})

####################################################################

observeEvent(input$create.PFGvsHS, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating maps of PFG habitat suitability vs simulated occurrences with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()), "</li>"
                                    , "<li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>"
                                    , "<li><strong>year(s) :</strong> ", as.numeric(input$graph.year), "</li>"
                                    , "<li><strong>opt.stratum :</strong> ", input$graph.opt.stratum, "</li>"
                                    , "</ul>"))
                        , title = "Maps of PFG habitat suitability vs simulated occurrences (specific year)"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFGvsHS(name.simulation = get_name.simul()
                                 , file.simulParam = get_param.simul()
                                 , years = as.numeric(input$graph.year)
                                 , opt.stratum = input$graph.opt.stratum
    )
  ))
  removeModal()
  
  output$plot.PFGvsHS = renderPlotly({
    plot(get_res[[1]][[1]][[1]])
  })
  
  setwd(path.init)
})

####################################################################

observeEvent(input$create.PFGmap, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating maps of PFG richness, relative cover, CWM with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()), "</li>"
                                    , "<li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>"
                                    , "<li><strong>year(s) :</strong> ", as.numeric(input$graph.year), "</li>"
                                    , "<li><strong>opt.stratum_min :</strong> ", input$graph.opt.stratum_minmax[1], "</li>"
                                    , "<li><strong>opt.stratum_max :</strong> ", input$graph.opt.stratum_minmax[2], "</li>"
                                    , "<li><strong>opt.doBinary :</strong> ", input$graph.opt.doBinary,"</li>"
                                    , "<li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU,"</li>"
                                    , "</ul>"))
                        , title = "Maps of PFG richness, relative cover, CWM (specific year)"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_mapPFG(name.simulation = get_name.simul()
                             , file.simulParam = get_param.simul()
                             , years = as.numeric(input$graph.year)
                             , opt.stratum_min = input$graph.opt.stratum_minmax[1]
                             , opt.stratum_max = input$graph.opt.stratum_minmax[2]
                             , opt.doBinary = input$graph.opt.doBinary
                             , opt.no_CPU = input$graph.opt.no_CPU
    )
  ))
  removeModal()
  
  output$plot.PFGmap = renderPlotly({
    plot(get_res[[1]][[1]]$plot[[1]])
  })
  
  setwd(path.init)
})

