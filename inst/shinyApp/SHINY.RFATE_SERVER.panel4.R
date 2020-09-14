
####################################################################

observeEvent(input$HELP.panel4, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = c(paste0("#help4_", 1:2),"#main.panel")
                                                , intro = c(paste0("<p>A folder name with a typical <code>FATE</code> organization, that can be created with the function 
                                                              <a href='", path.reference, "PRE_FATE.skeletonDirectory.html'>PRE_FATE.skeletonDirectory</a>.</p>
                                                              <p><strong>RESULTS</strong> folder should contain <code>FATE</code> output maps.</p>")
                                                            , paste0("<p><strong>ParamSimulation file</strong> : containing all links to the files created with the previous functions.<br>
                                                              It can be created with the function <a href='", path.reference, "PRE_FATE.params_simulParameters.html'>PRE_FATE.params_simulParameters</a>.</p>
                                                              <p>Parameters will be extracted and used to produce output graphics.</p>")
                                                            , "<p><em>1. BROWSER</em></p>
                                                            <ul><li>Explore <strong>pre-existing graphics</strong> into the selected simulation folder</li></ul>
                                                            <p><em>2. Evolution of simulation through time</em></p>
                                                            <ul><li>Create graphics with abundances of PFG <strong>over several years</strong></li></ul>
                                                            <p><em>3. Specific year</em></p>
                                                            <ul><li>Create graphics with abundances of PFG <strong>for a specific year</strong> of simulation</li></ul>
                                                            "))
          )
  )
})


####################################################################

get_path.simul = eventReactive(input$graph.folder.simul, {
  if (input$graph.folder.simul > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'graph.folder.simul'))
    updateDirectoryInput(session, 'graph.folder.simul', value = path)
    return(path)
  }
})

get_name.simul = eventReactive(input$graph.folder.simul, {
  return(basename(get_path.simul()))
})

get_path.folder = eventReactive(input$graph.folder.simul, {
  return(dirname(get_path.simul()))
})

get_param.simul = eventReactive(input$graph.simulParam, {
  return(paste0(get_path.simul(), "/PARAM_SIMUL/", input$graph.simulParam))
})

####################################################################

observeEvent(input$graph.folder.simul, {
  if (input$graph.folder.simul > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.simul(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "graph.simulParam"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      shinyjs::enable("graph.simulParam")
      shinyjs::enable("create.temporalEvolution")
      shinyjs::enable("create.evolutionCoverage")
      shinyjs::enable("create.evolutionPixels")
      shinyjs::enable("create.evolutionStability")
      shinyjs::enable("create.relativeAbund")
      shinyjs::enable("create.validationStat")
      shinyjs::enable("create.binaryMaps")
      shinyjs::enable("create.PFGvsHS")
      shinyjs::enable("create.PFGmap")
    } else
    {
      # shinyjs::reset("graph.simulParam")
      # shinyjs::disable("graph.simulParam")
      # shinyjs::disable("create.temporalEvolution")
      # shinyjs::disable("create.evolutionCoverage")
      # shinyjs::disable("create.evolutionPixels")
      # shinyjs::disable("create.evolutionStability")
      # shinyjs::disable("create.relativeAbund")
      # shinyjs::disable("create.validationStat")
      # shinyjs::disable("create.binaryMaps")
      # shinyjs::disable("create.PFGvsHS")
      # shinyjs::disable("create.PFGmap")
    }
    
    update_browser.files()
  } else
  {
    # shinyjs::reset("graph.simulParam")
    # shinyjs::disable("graph.simulParam")
    # shinyjs::disable("create.temporalEvolution")
    # shinyjs::disable("create.evolutionCoverage")
    # shinyjs::disable("create.evolutionPixels")
    # shinyjs::disable("create.evolutionStability")
    # shinyjs::disable("create.relativeAbund")
    # shinyjs::disable("create.validationStat")
    # shinyjs::disable("create.binaryMaps")
    # shinyjs::disable("create.PFGvsHS")
    # shinyjs::disable("create.PFGmap")
  }
})

####################################################################

get_last.createdFiles1 = function(pattern_path)
{
  system(command = paste0("ls -lat "
                          , get_path.simul()
                          , "/RESULTS/"
                          , pattern_path
                          , " | awk '{print $9}'")
         , intern = TRUE)
}

get_last.createdFiles2 = function(pattern_path = "", pattern_head, pattern_tail)
{
  last.createdFiles = get_last.createdFiles1(pattern_path = pattern_path)
  last.createdFiles = last.createdFiles[grep(pattern = pattern_head, last.createdFiles)]
  last.createdFiles = last.createdFiles[grep(pattern = pattern_tail, last.createdFiles)]
  return(paste0(get_path.simul()
                , "/RESULTS/"
                , pattern_path
                , last.createdFiles[1]))
}

####################################################################

get_globalParam = eventReactive(input$graph.simulParam, {
  if (nchar(input$graph.simulParam) > 0)
  {
    shinyjs::enable("create.temporalEvolution")
    shinyjs::enable("create.evolutionCoverage")
    shinyjs::enable("create.evolutionPixels")
    shinyjs::enable("create.evolutionStability")
    shinyjs::enable("create.relativeAbund")
    shinyjs::enable("create.validationStat")
    shinyjs::enable("create.binaryMaps")
    shinyjs::enable("create.PFGvsHS")
    shinyjs::enable("create.PFGmap")
    
    file.globalParam = .getParam(params.lines = get_param.simul()
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    file.globalParam = paste0(get_path.folder(), "/", file.globalParam)
    file.globalParam
    
  } else
  {
    # shinyjs::disable("graph.simulParam")
    # shinyjs::disable("create.temporalEvolution")
    # shinyjs::disable("create.evolutionCoverage")
    # shinyjs::disable("create.evolutionPixels")
    # shinyjs::disable("create.evolutionStability")
    # shinyjs::disable("create.relativeAbund")
    # shinyjs::disable("create.validationStat")
    # shinyjs::disable("create.binaryMaps")
    # shinyjs::disable("create.PFGvsHS")
    # shinyjs::disable("create.PFGmap")
    return("")
  }
})

get_dir.save = eventReactive(input$graph.simulParam, {
  if (nchar(input$graph.simulParam) > 0)
  {
    dir.save = .getParam(params.lines = get_param.simul()
                         , flag = "SAVING_DIR"
                         , flag.split = "^--.*--$"
                         , is.num = FALSE)
    dir.save = paste0(get_path.folder(), "/", dir.save)
    dir.save
  }
})


####################################################################

observeEvent(input$graph.simulParam, {
  
  ## -------------------------------------------------------------    
  years.available = list.files(paste0(get_dir.save(), "/ABUND_perPFG_allStrata"))
  years.available = sapply(sub("Abund_YEAR_", "", years.available)
                           , function(x) strsplit(as.character(x), "_")[[1]][1])
  years.available = rev(sort(unique(as.numeric(as.character(years.available)))))
  
  if (length(years.available) > 0)
  {
    updateSelectInput(session
                      , inputId = "graph.year"
                      , choices = years.available
                      , selected = max(years.available))
    shinyjs::enable("graph.year")
  } else
  {
    shinyjs::reset("graph.year")
    shinyjs::disable("graph.year")
  }
  
  ## -------------------------------------------------------------    
  strata.available = list.files(paste0(get_dir.save(), "/ABUND_perPFG_perStrata"))
  strata.available = sapply(sub(".*_STRATA_", "", strata.available)
                            , function(x) strsplit(as.character(x), "[.]")[[1]][1])
  strata.available = sort(unique(as.numeric(strata.available)))
  
  if (length(strata.available) > 0)
  {
    updateSelectInput(session
                      , inputId = "graph.opt.stratum"
                      , choices = c("all", strata.available)
                      , selected = "all")
    updateSliderInput(session
                      , inputId = "graph.opt.stratum_minmax"
                      , min = min(strata.available)
                      , max = max(strata.available)
                      , step = 1
                      , value = range(strata.available))
  } else
  {
    shinyjs::reset("graph.opt.stratum")
    shinyjs::reset("graph.opt.stratum_minmax")
  }
})
