
####################################################################

observeEvent(input$HELP.panel3, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = paste0("#help3_", 1:5)
                                                , intro = c(paste0("<p>A folder name with a typical <code>FATE</code> organization, that can be created with the function 
                                                              <a href='", path.reference, "PRE_FATE.skeletonDirectory.html'>PRE_FATE.skeletonDirectory</a>.</p>")
                                                            , paste0("<p><strong>ParamSimulation file</strong> : containing all links to the files created with the previous functions.<br>
                                                              It can be created with the function <a href='", path.reference, "PRE_FATE.params_simulParameters.html'>PRE_FATE.params_simulParameters</a>.</p>
                                                              <p>Parameters will be extracted and used to run the <code>FATE</code> simulation.</p>")
                                                            , "<code>FATE</code> simulations can be automatically parallelised to speed up computation time. 
                                                            This works only if the user machine is multi-core !"
                                                            , "A <code>FATE</code> simulation renders a lot of printed information. The user can choose the amount of information 
                                                            based on its importance :
                                                            <ol start = '0'>
                                                            <li>Shows any message</li>
                                                            <li>Shows any message, except debug</li>
                                                            <li>Shows only warning and error messages</li>
                                                            <li>Shows only error messages</li>
                                                            <li>Mute</li>
                                                            </ol>"
                                                            , "Vizualisation of <code>FATE</code> messages and informations will be done into the R console."
                                                ))
          )
  )
})

####################################################################

get_path.run = eventReactive(input$run.folder.simul, {
  if (input$run.folder.simul > 0)
  {
    path = choose.dir(default = readDirectoryInput(session, 'run.folder.simul'))
    updateDirectoryInput(session, 'run.folder.simul', value = path)
    return(path)
  }
})

####################################################################

observeEvent(input$run.folder.simul, {
  if (input$run.folder.simul > 0)
  {
    names.simulParam = list.files(path = paste0(get_path.run(), "/PARAM_SIMUL")
                                  , pattern = ".txt$"
                                  , all.files = FALSE
                                  , full.names = TRUE)
    names.simulParam = basename(names.simulParam)
    if (length(names.simulParam) > 0)
    {
      updateSelectInput(session
                        , inputId = "run.simulParam"
                        , choices = names.simulParam
                        , selected = names.simulParam[1])
      shinyjs::enable("run.simulParam")
    } else
    {
      shinyjs::reset("run.simulParam")
      shinyjs::disable("run.simulParam")
    }
  } else
  {
    shinyjs::reset("run.simulParam")
    shinyjs::disable("run.simulParam")
  }
})


####################################################################

observeEvent(input$run, {
  if (input$run.folder.simul > 0 && nchar(input$run.simulParam) > 0)
  {
    if (dir.exists(dirname(get_path.run())))
    {
      path.init = getwd()
      setwd(dirname(get_path.run()))
      cat(paste0("\n\n >>>>> Directory sets to : ", getwd(), " <<<<<\n\n"))
      
      showModal(modalDialog(HTML(paste0("Running simulation with : <ul>"
                                        , "<li><strong>folder :</strong> ", basename(get_path.run()), "</li>"
                                        , "<li><strong>simulation parameter file :</strong> ", input$run.simulParam, "</li>"
                                        , "<li><strong>opt.no_CPU :</strong> ", input$run.opt.no_CPU, "</li>"
                                        , "<li><strong>opt.verbose_level :</strong> ", input$run.opt.verbose_level, "</li>"
                                        , "</ul>"))
                            , title = HTML("Run <code>FATE</code> simulation")
                            , footer = NULL))
      Sys.sleep(3)
      get_res = print_messages(as.expression(
        FATE(simulParam = paste0(basename(get_path.run()), "/PARAM_SIMUL/", input$run.simulParam)
             , no_CPU = input$run.opt.no_CPU
             , verboseLevel = input$run.opt.verbose_level)
      ))
      removeModal()
      
      setwd(path.init)
      cat(paste0("\n\n >>>>> Directory sets to : ", getwd(), " <<<<<\n\n"))
    } else
    {
      shinyalert(type = "warning", text = "The simulation folder does not exist !")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must provide a simulation folder and a simulation parameter file !")
  }
})

