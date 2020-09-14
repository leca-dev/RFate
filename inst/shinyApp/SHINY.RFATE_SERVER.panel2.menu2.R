

####################################################################

observeEvent(input$HELP.panel2.menu2, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = paste0("#help2_2_", 1:5)
                                                , intro = c("<p>Multiple sets of global parameters (i.e. whose parameterization depends on the model, not on the data)
                                                            can be easily created from a pre-existing simulation folder.</p>
                                                            <p>The basic values and their variation range can be defined in three ways.</p>"
                                                            , paste0("<p>A folder name with a typical <code>FATE</code> organization, that can be created with the function 
                                                            <a href='", path.reference, "PRE_FATE.skeletonDirectory.html'>PRE_FATE.skeletonDirectory</a>.</p>
                                                            <p><strong>ParamSimulation file</strong> : containing all links to the parameter files.<br>
                                                            It can be created with the function <a href='", path.reference, "PRE_FATE.params_simulParameters.html'>PRE_FATE.params_simulParameters</a>.</p>
                                                            <p>Parameters will be extracted and used to produce a new simulation folder.</p>")
                                                            , "<p>Parameters presented here are the ones that will be varied. They are global parameters, not dependent on data rather on
                                                            model processes or implementation. They can be linked to specific modules, and these modules will need to be activated
                                                            within the given ParamSimulation file to be taken into account.</p>"
                                                            , "<p>When only one ParamSimulation file is given, these parameters represent the percentage of variation that will be applied
                                                            to each selected and corresponding parameter.</p>
                                                            <p>When two ParamSimulation files are given, this variation is calculated as the difference between the values filled in
                                                            these two files.</p>"
                                                            , "<p>This number corresponds to the number of ParamSimulation files that will be created (and all the associated files)
                                                            using <strong>Latin Hypercube Sampling</strong> to represent the whole space of possible parameter values.</p>"))
                                                )
          )
})

####################################################################

observeEvent(input$set.strategy, {
  if (!is.null(input$set.strategy))
  {
    shinyjs::show("main.panel")
    shinyjs::enable("create.multiple_set")
    
    if (input$set.strategy == "From 1 folder, 1 simulation file")
    {
      shinyjs::hide("set.folder1.simulParam2")
      output$UI.set.folders.strat3 = renderUI({ br() })
      shinyjs::show("set.slider.1")
      shinyjs::show("set.slider.2")
      shinyjs::show("set.slider.3")
    } else if (input$set.strategy == "From 1 folder, 2 simulation files")
    {
      shinyjs::show("set.folder1.simulParam2")
      output$UI.set.folders.strat3 = renderUI({ br() })
      shinyjs::hide("set.slider.1")
      shinyjs::hide("set.slider.2")
      shinyjs::hide("set.slider.3")
      
    } else if (input$set.strategy == "From 2 folders, 2 simulation files")
    {
      shinyjs::hide("set.folder1.simulParam2")
      output$UI.set.folders.strat3 = renderUI({
        tagList(
          directoryInput(inputId = "set.folder2"
                         , label = param.style("Select the simulation folder :")
                         , value = '~')
          , shinyjs::disabled(
            selectInput(inputId = "set.folder2.simulParam2"
                        , label = param.style("Select the simulation parameters file :")
                        , choices = NULL
                        , selected = NULL
                        , multiple = F
                        , width = "100%")
          )
        )
      })
      shinyjs::hide("set.slider.1")
      shinyjs::hide("set.slider.2")
      shinyjs::hide("set.slider.3")
    }
  }
})


####################################################################

get_path.folder1 = eventReactive(input$set.folder1, {
  if (input$set.folder1 > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'set.folder1'))
    updateDirectoryInput(session, 'set.folder1', value = path)
    return(path)
  }
})

get_path.folder2 = eventReactive(input$set.folder2, {
  if (input$set.folder2 > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'set.folder2'))
    updateDirectoryInput(session, 'set.folder2', value = path)
    return(path)
  }
})

####################################################################

observeEvent(paste(input$set.folder1, input$set.strategy), {
  if (input$set.folder1 > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.folder1(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "set.folder1.simulParam1"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      updateSelectInput(session
                        , inputId = "set.folder1.simulParam2"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      shinyjs::enable("set.folder1.simulParam1")
      shinyjs::enable("set.folder1.simulParam2")
    } else
    {
      shinyjs::reset("set.folder1.simulParam1")
      shinyjs::reset("set.folder1.simulParam2")
      shinyjs::disable("set.folder1.simulParam1")
      shinyjs::disable("set.folder1.simulParam2")
    }
  } else
  {
    shinyjs::reset("set.folder1.simulParam1")
    shinyjs::reset("set.folder1.simulParam2")
    shinyjs::disable("set.folder1.simulParam1")
    shinyjs::disable("set.folder1.simulParam2")
  }
})

observeEvent(input$set.folder2, {
  if (input$set.folder2 > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.folder2(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "set.folder2.simulParam2"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      
      shinyjs::enable("set.folder2.simulParam2")
    } else
    {
      shinyjs::reset("set.folder2.simulParam2")
      shinyjs::disable("set.folder2.simulParam2")
    }
  } else
  {
    shinyjs::reset("set.folder2.simulParam2")
    shinyjs::disable("set.folder2.simulParam2")
  }
})


####################################################################

get_checked = eventReactive(lapply(grep(pattern = "^set.choices.",
                                        x = names(input),
                                        value = TRUE), function(x) input[[x]]),
                            {
                              return(list(input$set.choices.1
                                          , input$set.choices.2
                                          , input$set.choices.3
                                          , input$set.choices.4
                                          , input$set.choices.5
                                          , input$set.choices.6))
                            })

observeEvent(input$create.multiple_set, {
  params.checked = get_checked()
  
  if (length(unlist(params.checked)) == 0)
  {
    shinyalert(type = "warning", text = "You must select some parameters to vary !")
  } else
  {
    showModal(modalDialog(HTML(paste0("Creating multiple set of parameters with :
                                        <ul>
                                        <li><strong>simulation parameter file (1) :</strong> "
                                      , input$set.folder1.simulParam1
                                      , "</li>
                                      <li><strong>simulation parameter file (2) :</strong> "
                                      , ifelse(input$set.strategy == "From 1 folder, 1 simulation file"
                                               , ""
                                               , ifelse(input$set.strategy == "From 1 folder, 2 simulation files"
                                                        , input$set.folder1.simulParam2
                                                        , input$set.folder2.simulParam2)
                                      )
                                      , "</li>
                                        <li><strong>parameters selected :</strong> "
                                      , paste0(unlist(params.checked), collapse = ', ')
                                      , "</li></ul>"))
                          , title = HTML("Creating multiple sets of parameters for a <code>FATE</code> simulation")
                          , footer = NULL))
    Sys.sleep(3)
    get_res = print_messages(as.expression(
      PRE_FATE.params_multipleSet(name.simulation.1 = get_path.folder1()
                                  , name.simulation.2 = ifelse(input$set.strategy == "From 2 folders, 2 simulation files"
                                                               , get_path.folder2()
                                                               , "")
                                  , file.simulParam.1 = input$set.folder1.simulParam1
                                  , file.simulParam.2 = ifelse(input$set.strategy == "From 1 folder, 1 simulation file"
                                                               , ""
                                                               , ifelse(input$set.strategy == "From 1 folder, 2 simulation files"
                                                                        , input$set.folder1.simulParam2
                                                                        , input$set.folder2.simulParam2)
                                  )
                                  , no_simulations = input$set.num_simul
                                  , opt.percent_max = input$set.slider.1 / 100
                                  , opt.percent_seeding = input$set.slider.2 / 100
                                  , opt.percent_light = input$set.slider.3 / 100
                                  , do.max_abund_low = ("max_abund_low" %in% unlist(params.checked))
                                  , do.max_abund_medium = ("max_abund_medium" %in% unlist(params.checked))
                                  , do.max_abund_high = ("max_abund_high" %in% unlist(params.checked))
                                  , do.seeding_duration = ("seeding_duration" %in% unlist(params.checked))
                                  , do.seeding_timestep = ("seeding_timestep" %in% unlist(params.checked))
                                  , do.seeding_input = ("seeding_input" %in% unlist(params.checked))
                                  , do.LIGHT.thresh_medium = ("light_thresh_medium" %in% unlist(params.checked))
                                  , do.LIGHT.thresh_low = ("light_thresh_low" %in% unlist(params.checked))
                                  , do.HABSUIT.mode = ("habsuit_mode" %in% unlist(params.checked))
                                  , do.DISPERSAL.mode = ("dispersal_mode" %in% unlist(params.checked))
                                  , do.no_strata =("strata_limits" %in% unlist(params.checked))
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/SUCC/"))
    removeModal()
    
  }
})


