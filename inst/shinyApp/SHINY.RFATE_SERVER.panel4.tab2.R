
####################################################################

observeEvent(input$show.through_time, {
  if (input$show.through_time == "Total abundance & coverage")
  {
    shinyjs::show("panel.evolutionCoverage")
    shinyjs::hide("panel.evolutionPixels")
    shinyjs::hide("panel.evolutionStability")
  } else if (input$show.through_time == "Pixels abundance & resources")
  {
    shinyjs::hide("panel.evolutionCoverage")
    shinyjs::show("panel.evolutionPixels")
    shinyjs::hide("panel.evolutionStability")
  } else if (input$show.through_time == "Community composition stability")
  {
    shinyjs::hide("panel.evolutionCoverage")
    shinyjs::hide("panel.evolutionPixels")
    shinyjs::show("panel.evolutionStability")
  }
})

####################################################################

output$UI.opt.cells_ID = renderUI({
  if (input$graph.opt.cells_ID)
  {
    lapply(1:5, function(x)
    {
      numericInput(inputId = paste0("cells_ID.", x)
                   , label = NULL
                   , value = 0
                   , width = "100%")
    })
  }
})

####################################################################

observeEvent(input$create.temporalEvolution, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating PFG abundance tables with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()), "</li>"
                                    , "<li><strong>simulation parameter file :</strong> "
                                    , basename(get_param.simul()), "</li>"
                                    , "<li><strong>no_years :</strong> ", as.numeric(input$graph.no.years), "</li>"
                                    , "<li><strong>opt.ras_habitat :</strong> ", input$graph.opt.ras_habitat, "</li>"
                                    , "<li><strong>opt.no_CPU :</strong> ", input$graph.opt.no_CPU, "</li>"
                                    , "</ul>"))
                        , title = "Relative PFG abundance tables through time"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.temporalEvolution(name.simulation = get_name.simul()
                                , file.simulParam = get_param.simul()
                                , no_years = as.numeric(input$graph.no.years)
                                , opt.ras_habitat = NULL #"" #input$graph.opt.ras_habitat
                                , opt.no_CPU = as.numeric(input$graph.opt.no_CPU)
    )
  ))
  removeModal()
  
  setwd(path.init)
})

####################################################################

observeEvent(input$create.evolutionCoverage, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating evolution coverage graphic with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()),"</li>"
                                    , "<li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>"
                                    , "<li><strong>opt.fixedScale :</strong> ", input$graph.opt.fixedScale, "</li>"
                                    , "</ul>"))
                        , title = "Evolution coverage graphic"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionCoverage(name.simulation = get_name.simul()
                                        , file.simulParam = get_param.simul()
                                        , opt.fixedScale = input$graph.opt.fixedScale
    )
  ))
  removeModal()
  
  output$plot.evolutionCoverage1 = renderPlotly({
    plot(get_res[[1]]$plot.spaceOccupancy)
  })
  
  output$plot.evolutionCoverage2 = renderPlotly({
    plot(get_res[[1]]$plot.totalAbundance)
  })
  
  setwd(path.init)
})


####################################################################

observeEvent(input$create.evolutionPixels, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  if (input$graph.opt.cells_ID)
  {
    graph.opt.cells_ID = c(input$cells_ID.1, input$cells_ID.2, input$cells_ID.3
                           , input$cells_ID.4, input$cells_ID.5)
  } else
  {
    graph.opt.cells_ID = ""
  }
  
  showModal(modalDialog(HTML(paste0("Creating evolution of PFG abundance (pixels) graphic with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()), "</li>"
                                    , "<li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>"
                                    , "<li><strong>opt.cells_ID :</strong> ", graph.opt.cells_ID, "</li>"
                                    , "<li><strong>opt.fixedScale :</strong> ", input$graph.opt.fixedScale, "</li>"
                                    , "</ul>"))
                        , title = "Evolution of PFG abundance through time (pixels)"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionPixels(name.simulation = get_name.simul()
                                      , file.simulParam = get_param.simul()
                                      , opt.cells_ID = graph.opt.cells_ID
                                      , opt.fixedScale = input$graph.opt.fixedScale
    )
  ))
  removeModal()
  
  output$plot.evolutionPixels = renderPlotly({
    plot(get_res[[1]]$plot)
  })
  
  setwd(path.init)
})


####################################################################

observeEvent(input$create.evolutionStability, {
  
  path.init = getwd()
  setwd(get_path.folder())
  
  showModal(modalDialog(HTML(paste0("Creating evolution of habitat composition graphic with : <ul>"
                                    , "<li><strong>folder :</strong> ", basename(get_name.simul()), "</li>"
                                    , "<li><strong>simulation parameter file :</strong> ", basename(get_param.simul()), "</li>"
                                    , "<li><strong>movingWindow_size :</strong> ", input$graph.mw.size, "</li>"
                                    , "<li><strong>movingWindow_step :</strong> ", input$graph.mw.step, "</li>"
                                    , "</ul>"))
                        , title = "Evolution of habitat composition through time"
                        , footer = NULL))
  Sys.sleep(3)
  get_res = print_messages(as.expression(
    POST_FATE.graphic_evolutionStability(name.simulation = get_name.simul()
                                         , file.simulParam = get_param.simul()
                                         , movingWindow_size = as.numeric(input$graph.mw.size)
                                         , movingWindow_step = as.numeric(input$graph.mw.step)
    )
  ))
  removeModal()
  
  if (!is.null(get_res[[1]]$plot.stab))
  {
    output$plot.evolutionStability = renderPlotly({
      plot(get_res[[1]]$plot.stab)
    })
  }
  
  setwd(path.init)
})

