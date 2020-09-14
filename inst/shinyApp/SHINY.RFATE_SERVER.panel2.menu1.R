
####################################################################

observeEvent(input$HELP.panel2.menu1, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = c(paste0("#help2_1_", 1:2),"#main.panel",paste0("#help2_", 4:6))
                                                # , position = c("auto", "auto", "bottom-middle-aligned", "auto", "auto")
                                                , intro = c("<p><code>FATE</code> requires only one input parameter, which is a file containing
                                                            the names of parameter files, which may themselves contain parameters or other
                                                            file names. The point is : the user could give names of files stored everywhere
                                                            on a machine, and does not have to put them all in one same place.</p>
                                                            <p>But as this is more practical, this panel proposes a way to organize all
                                                            those files or parameter files that will or could be used by a <code>FATE</code>
                                                            simulation.</p>"
                                                            , "<p><code>PRE_FATE.skeletonDirectory</code> function creates a user-friendly directory tree to run a <code>FATE</code> simulation.
                                                            <p>The tree structure is detailed below the button.</p>"
                                                            , paste0("<p><em>1. Simulation parameterization</em></p>
                                                            <ul>
                                                            <li>
                                                            <strong>Global parameters</strong> (simulation duration, computer resources, manage abundance values, modules loaded...)<br>
                                                            (<a href='", path.reference, "PRE_FATE.params_globalParameters.html'>PRE_FATE.params_globalParameters</a>)
                                                            </li>
                                                            <li>
                                                            <strong>Years to save abundance rasters and simulation outputs</strong>
                                                            (<a href='", path.reference, "PRE_FATE.params_saveYears.html'>PRE_FATE.params_saveYears</a>)
                                                            </li>
                                                            <li>
                                                            <strong>Years and files to change rasters</strong> for the succession, habitat suitability or disturbance modules<br>
                                                            (<a href='", path.reference, "PRE_FATE.params_changingYears.html'>PRE_FATE.params_changingYears</a>)
                                                            </li>
                                                            </ul>
                                                            <p><em>2. For each PFG : behavior and characteristics</em></p>
                                                            <ul>
                                                            <li>
                                                            <strong>Succession files</strong> (<a href='", path.reference, "PRE_FATE.params_PFGsuccession.html'>PRE_FATE.params_PFGsuccession</a>)
                                                            </li>
                                                            <li>
                                                            <strong>Dispersal files</strong> (<a href='", path.reference, "PRE_FATE.params_PFGdispersal.html'>PRE_FATE.params_PFGdispersal</a>)
                                                            </li>
                                                            <li>
                                                            <strong>Light files</strong> (<a href='", path.reference, "PRE_FATE.params_PFGlight.html'>PRE_FATE.params_PFGlight</a>)
                                                            </li>
                                                            <li>
                                                            <strong>Soil files</strong> (<a href='", path.reference, "PRE_FATE.params_PFGsoil.html'>PRE_FATE.params_PFGsoil</a>)
                                                            </li>
                                                            <li>
                                                            <strong>Disturbance files</strong> : response to perturbations in terms of resprouting and mortality<br>
                                                            (<a href='", path.reference, "PRE_FATE.params_PFGdisturbance.html'>PRE_FATE.params_PFGdisturbance</a>)
                                                            </li>
                                                            </ul>")
                                                            , paste0("<p><em>3. Parameter management</em></p>
                                                            <ul>
                                                            <li>
                                                            <strong>ParamSimulation file</strong> : containing all links to the files created with the previous functions.<br>
                                                            This is the file that will be given as the only argument to the <code>FATE</code> executable file into the command line.<br>
                                                            It can be created with the function <a href='", path.reference, "PRE_FATE.params_simulParameters.html'>PRE_FATE.params_simulParameters</a>
                                                            </li>
                                                            </ul>")
                                                            , "<p>A pre-existing ParamSimulation file can be given to load previous parameters and help create new parameter files from basis.</p>"
                                                            , "<p>Download the complete simulation folder as an archive file (<code>FATE_simulation.zip</code>).</p>"))
          )
  )
})


####################################################################

observeEvent(input$create.skeleton, {
  get_res = print_messages(as.expression(
    PRE_FATE.skeletonDirectory(name.simulation = input$name.simul)
  ))
  
  if (as.character(get_res) != "0")
  {
    shinyjs::show("main.panel")
    shinyjs::enable("load.file")
    shinyjs::enable("load.param")
    shinyjs::enable("create.simul")
    shinyjs::enable("FATE_simulation.zip")
    shinyjs::enable("refresh")
  }
})

####################################################################

observeEvent(input$name.simul, {
  file.simulParam = get_files.names(path_folder = paste0(input$name.simul, "/PARAM_SIMUL/"))
  if (length(file.simulParam) > 0)
  {
    newFiles = basename(file.simulParam)
    newFiles = sub("paramSimul_", "", newFiles)
    newFiles = sub(".txt", "", newFiles)
    updateSelectInput(session = session
                      , inputId = "load.file"
                      , choices = newFiles
                      , selected = newFiles[1]
    )
  }
})

####################################################################

get_files_simulParam = function(simulParam.fi, simulParam.val, flag, flag.split = "^--.*--$", is.num = FALSE)
{
  res = ifelse(length(grep(flag, simulParam.val)) > 0
               , list(.getParam(params.lines = simulParam.fi
                                , flag = flag
                                , flag.split = flag.split
                                , is.num = is.num))
               , "")
  res = unlist(res)
  if (sum(file.exists(res)) > 0)
  {
    res = res[file.exists(res)]
  } else
  {
    res = ""
  }
  return(res)
}

get_val_param = function(filename)
{
  if (file.exists(filename))
  {
    val.file = readLines(filename)
    ind.comment = grep("^#", val.file)
    if (length(ind.comment) > 0)
    {
      val.file = val.file[-ind.comment]
    }
  } else
  {
    val.file = ""
  }
  return(val.file)
}

####################################################################

get_update.global = function(file.globalParam)
{
  update.param = list(
    "opt.no_CPU" = .getParam(params.lines = file.globalParam
                             , flag = "NO_CPU"
                             , flag.split = " "
                             , is.num = TRUE)
    , "required.no_PFG" = .getParam(params.lines = file.globalParam
                                    , flag = "NO_PFG"
                                    , flag.split = " "
                                    , is.num = TRUE)
    , "required.no_strata" = .getParam(params.lines = file.globalParam
                                       , flag = "NO_STRATA"
                                       , flag.split = " "
                                       , is.num = TRUE)
    , "required.simul_duration" = .getParam(params.lines = file.globalParam
                                            , flag = "SIMULATION_DURATION"
                                            , flag.split = " "
                                            , is.num = TRUE)
    , "required.seeding_duration" = .getParam(params.lines = file.globalParam
                                              , flag = "SEEDING_DURATION"
                                              , flag.split = " "
                                              , is.num = TRUE)
    , "required.seeding_timestep" = .getParam(params.lines = file.globalParam
                                              , flag = "SEEDING_TIMESTEP"
                                              , flag.split = " "
                                              , is.num = TRUE)
    , "required.seeding_input" = .getParam(params.lines = file.globalParam
                                           , flag = "SEEDING_INPUT"
                                           , flag.split = " "
                                           , is.num = TRUE)
    , "required.max_abund_low" = .getParam(params.lines = file.globalParam
                                           , flag = "MAX_ABUND_LOW"
                                           , flag.split = " "
                                           , is.num = TRUE)
    , "required.max_abund_medium" = .getParam(params.lines = file.globalParam
                                              , flag = "MAX_ABUND_MEDIUM"
                                              , flag.split = " "
                                              , is.num = TRUE)
    , "required.max_abund_high" = .getParam(params.lines = file.globalParam
                                            , flag = "MAX_ABUND_HIGH"
                                            , flag.split = " "
                                            , is.num = TRUE)
    , "doDispersal" = .getParam(params.lines = file.globalParam
                                , flag = "DO_DISPERSAL"
                                , flag.split = " "
                                , is.num = TRUE)
    , "DISPERSAL.mode" = .getParam(params.lines = file.globalParam
                                   , flag = "DISPERSAL_MODE"
                                   , flag.split = " "
                                   , is.num = TRUE)
    , "doHabSuitability" = .getParam(params.lines = file.globalParam
                                     , flag = "DO_HAB_SUITABILITY"
                                     , flag.split = " "
                                     , is.num = TRUE)
    , "HABSUIT.mode" = .getParam(params.lines = file.globalParam
                                 , flag = "HABSUIT_MODE"
                                 , flag.split = " "
                                 , is.num = TRUE)
    , "doLight" = .getParam(params.lines = file.globalParam
                            , flag = "DO_LIGHT_COMPETITION"
                            , flag.split = " "
                            , is.num = TRUE)
    , "LIGHT.thresh_medium" = .getParam(params.lines = file.globalParam
                                        , flag = "LIGHT_THRESH_MEDIUM"
                                        , flag.split = " "
                                        , is.num = TRUE)
    , "LIGHT.thresh_low" = .getParam(params.lines = file.globalParam
                                     , flag = "LIGHT_THRESH_LOW"
                                     , flag.split = " "
                                     , is.num = TRUE)
    , "doSoil" = .getParam(params.lines = file.globalParam
                           , flag = "DO_SOIL_COMPETITION"
                           , flag.split = " "
                           , is.num = TRUE)
    , "SOIL.init" = .getParam(params.lines = file.globalParam
                              , flag = "SOIL_INIT"
                              , flag.split = " "
                              , is.num = TRUE)
    , "SOIL.retention" = .getParam(params.lines = file.globalParam
                                   , flag = "SOIL_RETENTION"
                                   , flag.split = " "
                                   , is.num = TRUE)
    , "doDisturbances" = .getParam(params.lines = file.globalParam
                                   , flag = "DO_DISTURBANCES"
                                   , flag.split = " "
                                   , is.num = TRUE)
    , "DIST.no" = .getParam(params.lines = file.globalParam
                            , flag = "DIST_NO"
                            , flag.split = " "
                            , is.num = TRUE)
    , "DIST.no_sub" = .getParam(params.lines = file.globalParam
                                , flag = "DIST_NOSUB"
                                , flag.split = " "
                                , is.num = TRUE)
    , "DIST.freq" = .getParam(params.lines = file.globalParam
                              , flag = "DIST_FREQ"
                              , flag.split = " "
                              , is.num = TRUE)
    , "doDrought" = .getParam(params.lines = file.globalParam
                              , flag = "DO_DROUGHT_DISTURBANCE"
                              , flag.split = " "
                              , is.num = TRUE)
    , "DROUGHT.no_sub" = .getParam(params.lines = file.globalParam
                                   , flag = "DROUGHT_NOSUB"
                                   , flag.split = " "
                                   , is.num = TRUE)
    , "doAliens" = .getParam(params.lines = file.globalParam
                             , flag = "DO_ALIENS_INTRODUCTION"
                             , flag.split = " "
                             , is.num = TRUE)
    , "ALIENS.no" = .getParam(params.lines = file.globalParam
                              , flag = "ALIENS_NO"
                              , flag.split = " "
                              , is.num = TRUE)
    , "ALIENS.freq" = .getParam(params.lines = file.globalParam
                                , flag = "ALIENS_FREQ"
                                , flag.split = " "
                                , is.num = TRUE)
    , "doFire" = .getParam(params.lines = file.globalParam
                           , flag = "DO_FIRE_DISTURBANCE"
                           , flag.split = " "
                           , is.num = TRUE)
    , "FIRE.no" = .getParam(params.lines = file.globalParam
                            , flag = "FIRE_NO"
                            , flag.split = " "
                            , is.num = TRUE)
    , "FIRE.no_sub" = .getParam(params.lines = file.globalParam
                                , flag = "FIRE_NOSUB"
                                , flag.split = " "
                                , is.num = TRUE)
    , "FIRE.freq" = .getParam(params.lines = file.globalParam
                              , flag = "FIRE_FREQ"
                              , flag.split = " "
                              , is.num = TRUE)
    , "FIRE.ignit_mode" = .getParam(params.lines = file.globalParam
                                    , flag = "FIRE_IGNIT_MODE"
                                    , flag.split = " "
                                    , is.num = TRUE)
    , "FIRE.ignit_no" = .getParam(params.lines = file.globalParam
                                  , flag = "FIRE_IGNIT_NO"
                                  , flag.split = " "
                                  , is.num = TRUE)
    , "FIRE.ignit_nohist" = .getParam(params.lines = file.globalParam
                                      , flag = "FIRE_IGNIT_NOHIST"
                                      , flag.split = " "
                                      , is.num = TRUE)
    , "FIRE.ignit_logis" = .getParam(params.lines = file.globalParam
                                     , flag = "FIRE_IGNIT_LOGIS"
                                     , flag.split = " "
                                     , is.num = TRUE)
    , "FIRE.ignit_flammmax" = .getParam(params.lines = file.globalParam
                                        , flag = "FIRE_IGNIT_FLAMMMAX"
                                        , flag.split = " "
                                        , is.num = TRUE)
    , "FIRE.neigh_mode" = .getParam(params.lines = file.globalParam
                                    , flag = "FIRE_NEIGH_MODE"
                                    , flag.split = " "
                                    , is.num = TRUE)
    , "FIRE.neigh_cc" = .getParam(params.lines = file.globalParam
                                  , flag = "FIRE_NEIGH_CC"
                                  , flag.split = " "
                                  , is.num = TRUE)
    , "FIRE.prop_mode" = .getParam(params.lines = file.globalParam
                                   , flag = "FIRE_PROP_MODE"
                                   , flag.split = " "
                                   , is.num = TRUE)
    , "FIRE.prop_intensity" = .getParam(params.lines = file.globalParam
                                        , flag = "FIRE_PROP_INTENSITY"
                                        , flag.split = " "
                                        , is.num = TRUE)
    , "FIRE.prop_logis" = .getParam(params.lines = file.globalParam
                                    , flag = "FIRE_PROP_LOGIS"
                                    , flag.split = " "
                                    , is.num = TRUE)
    , "FIRE.quota_mode" = .getParam(params.lines = file.globalParam
                                    , flag = "FIRE_QUOTA_MODE"
                                    , flag.split = " "
                                    , is.num = TRUE)
    , "FIRE.quota_max" = .getParam(params.lines = file.globalParam
                                   , flag = "FIRE_QUOTA_MAX"
                                   , flag.split = " "
                                   , is.num = TRUE)
  )
  
  ## update shiny input parameters
  updateShinyInputs(session = session
                    , updates = update.param)
}

get_update.save = function(file.saveMaps, file.saveObjects, file.PFGsucc)
{
  val.saveMaps = get_val_param(file.saveMaps)
  val.saveObjects = get_val_param(file.saveObjects)
  
  update.param = list(
    "save.maps.folder" = sub(paste0(input$name.simul, "/DATA/SAVE"), "", dirname(file.saveMaps))
    , "save.maps.year1" = val.saveMaps[1]
    , "save.maps.year2" = val.saveMaps[length(val.saveMaps)]
    , "save.maps.no" = length(val.saveMaps)
    , "save.objects.folder" = sub(paste0(input$name.simul, "/DATA/SAVE"), "", dirname(file.saveObjects))
    , "save.objects.year1" = val.saveObjects[1]
    , "save.objects.year2" = ifelse(length(val.saveObjects) > 1, val.saveObjects[2], "")
    , "save.objects.year3" = ifelse(length(val.saveObjects) > 2, val.saveObjects[3], "")
    , "PFG.folder" = sub(paste0(input$name.simul, "/DATA/PFGS/SUCC"), "", unique(dirname(file.PFGsucc)))
  )
  
  ## update shiny input parameters
  updateShinyInputs(session = session
                    , updates = update.param)
}

observeEvent(input$load.param, {
  
  file.globalParam = ""
  file.saveMaps = file.saveObjects = ""
  file.PFGsucc = file.PFGdisp = file.PFGlight = file.PFGsoil = ""
  file.PFGdist = file.PFGdrought = file.PFGfire = ""
  file.changeMask_t = file.changeMask_m = ""
  file.changeHabsuit_t = file.changeHabsuit_m = ""
  file.changeDist_t = file.changeDist_m = ""
  file.changeDrought_t = file.changeDrought_m = ""
  file.changeAliens_t = file.changeAliens_m = ""
  file.changeFire_t = file.changeFire_m = ""
  
  if (nchar(input$load.file) > 0)
  {
    file.simulParam = paste0(input$name.simul, "/PARAM_SIMUL/paramSimul_", input$load.file, ".txt")
    print(file.simulParam)
    if (file.exists(file.simulParam))
    {
      val.simulParam = readLines(file.simulParam)
      
      file.globalParam = get_files_simulParam(file.simulParam, val.simulParam, flag = "GLOBAL_PARAMS")
      file.saveMaps = get_files_simulParam(file.simulParam, val.simulParam, flag = "SAVING_YEARS_MAPS")
      file.saveObjects = get_files_simulParam(file.simulParam, val.simulParam, flag = "SAVING_YEARS_OBJECTS")
      file.PFGsucc = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_PARAMS_LIFE_HISTORY")
      file.PFGdisp = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_PARAMS_DISPERSAL")
      file.PFGlight = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_PARAMS_LIGHT")
      file.PFGsoil = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_PARAMS_SOIL")
      file.PFGdist = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_PARAMS_DISTURBANCES")
      file.PFGdrought = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_PARAMS_DROUGHT")
      file.PFGfire = get_files_simulParam(file.simulParam, val.simulParam, flag = "PFG_PARAMS_FIRE")
      file.changeMask_t = get_files_simulParam(file.simulParam, val.simulParam, flag = "MASK_CHANGEMASK_YEARS")
      file.changeMask_m = get_files_simulParam(file.simulParam, val.simulParam, flag = "MASK_CHANGEMASK_FILES")
      file.changeHabsuit_t = get_files_simulParam(file.simulParam, val.simulParam, flag = "HABSUIT_CHANGEMASK_YEARS")
      file.changeHabsuit_m = get_files_simulParam(file.simulParam, val.simulParam, flag = "HABSUIT_CHANGEMASK_FILES")
      file.changeDist_t = get_files_simulParam(file.simulParam, val.simulParam, flag = "DIST_CHANGEMASK_YEARS")
      file.changeDist_m = get_files_simulParam(file.simulParam, val.simulParam, flag = "DIST_CHANGEMASK_FILES")
      file.changeDrought_t = get_files_simulParam(file.simulParam, val.simulParam, flag = "DIST_CHANGEMASK_YEARS")
      file.changeDrought_m = get_files_simulParam(file.simulParam, val.simulParam, flag = "DIST_CHANGEMASK_FILES")
      file.changeAliens_t = get_files_simulParam(file.simulParam, val.simulParam, flag = "ALIENS_CHANGEMASK_YEARS")
      file.changeAliens_m = get_files_simulParam(file.simulParam, val.simulParam, flag = "ALIENS_CHANGEMASK_FILES")
      file.changeFire_t = get_files_simulParam(file.simulParam, val.simulParam, flag = "FIRE_CHANGEMASK_YEARS")
      file.changeFire_m = get_files_simulParam(file.simulParam, val.simulParam, flag = "FIRE_CHANGEMASK_FILES")
    } else
    {
      shinyalert(type = "warning", text = paste0("The file '", file.simulParam, "'does not exist !"))
    }
  } else
  {
    shinyalert(type = "warning", text = "You must give the name of an existing simulation parameters file !")
  }
  
  if (length(file.globalParam) > 0 && nchar(file.globalParam) > 0)
  {
    ## update global /save values
    get_update.global(file.globalParam = file.globalParam)
    get_update.save(file.saveMaps = file.saveMaps
                    , file.saveObjects = file.saveObjects
                    , file.PFGsucc = file.PFGsucc)
    
    ## update shiny reactiveValues
    if (length(file.PFGsucc) > 0 && nchar(file.PFGsucc) > 0)
    {
      RV$names.PFG = sub(".txt", "", sub("SUCC_", "", basename(file.PFGsucc)))
      RV$mat.PFG.ALL = foreach(fi = file.PFGsucc, .combine = 'rbind') %do%
      {
        PFG = .getParam(params.lines = fi
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
        type = .getParam(params.lines = fi
                         , flag = "TYPE"
                         , flag.split = " "
                         , is.num = FALSE)
        height = .getParam(params.lines = fi
                           , flag = "HEIGHT"
                           , flag.split = " "
                           , is.num = TRUE)
        maturity = .getParam(params.lines = fi
                             , flag = "MATURITY"
                             , flag.split = " "
                             , is.num = TRUE)
        longevity = .getParam(params.lines = fi
                              , flag = "LONGEVITY"
                              , flag.split = " "
                              , is.num = TRUE)
        max_stratum = .getParam(params.lines = fi
                              , flag = "MAX_STRATUM"
                              , flag.split = " "
                              , is.num = TRUE)
        max_abundance = .getParam(params.lines = fi
                              , flag = "MAX_ABUNDANCE"
                              , flag.split = " "
                              , is.num = TRUE)
        light = 0
        
        return(data.frame(PFG = ifelse(is.null(PFG), "", PFG)
                          , type = ifelse(is.null(type), "", type)
                          , height = ifelse(is.null(height), "", height)
                          , maturity = ifelse(is.null(maturity), "", maturity)
                          , longevity = ifelse(is.null(longevity), "", longevity)
                          , max_stratum = ifelse(is.null(max_stratum), "", max_stratum)
                          , max_abundance = ifelse(is.null(max_abundance), "", max_abundance)
                          , light = 0
        ))
      }
      if (length(file.PFGlight) > 0)
      {
        for(fi in file.PFGlight)
        {
          PFG = .getParam(params.lines = fi
                          , flag = "NAME"
                          , flag.split = " "
                          , is.num = FALSE)
          light = .getParam(params.lines = fi
                            , flag = "LIGHT"
                            , flag.split = " "
                            , is.num = TRUE)
          light = ifelse(is.null(light), NULL, light)
          if (PFG %in% RV$mat.PFG.ALL$PFG && !is.null(light))
          {
            RV$mat.PFG.ALL$light[which(RV$mat.PFG.ALL$PFG == PFG)] = light
          }
        }
      }
    }
    ## Soil
    if (length(file.PFGsoil) > 0 && nchar(file.PFGsoil) > 0)
    {
      RV$mat.PFG.soil = foreach(fi = file.PFGsoil, .combine = 'rbind') %do%
      {
        PFG = .getParam(params.lines = fi
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
        soil_contrib = .getParam(params.lines = fi
                                 , flag = "SOIL_CONTRIB"
                                 , flag.split = " "
                                 , is.num = TRUE)
        soil_tol_min = .getParam(params.lines = fi
                                 , flag = "SOIL_LOW"
                                 , flag.split = " "
                                 , is.num = TRUE)
        soil_tol_max = .getParam(params.lines = fi
                                 , flag = "SOIL_HIGH"
                                 , flag.split = " "
                                 , is.num = TRUE)
        soil_tol = .getParam(params.lines = fi
                             , flag = "SOIL_TOL"
                             , flag.split = " "
                             , is.num = TRUE)
        
        return(data.frame(PFG = ifelse(is.null(PFG), "", PFG)
                          , soil_contrib = ifelse(is.null(soil_contrib), "", soil_contrib)
                          , soil_tol_min = ifelse(is.null(soil_tol_min), "", soil_tol_min)
                          , soil_tol_max = ifelse(is.null(soil_tol_max), "", soil_tol_max)
                          , lifeStage = rep(c("Germinant", "Immature", "Mature"), each = 3)
                          , soilResources = rep(c("Low", "Medium", "High"), 3)
                          , soil_tol = ifelse(is.null(soil_tol), "", soil_tol)
        ))
      }
      if (length(file.PFGsucc) > 0 && nchar(file.PFGsucc) > 0)
      {
        RV$mat.PFG.soil = merge(RV$mat.PFG.soil, RV$mat.PFG.ALL[, c("PFG", "type")], by = "PFG", all.x = TRUE)
      }
    }
    ## Dispersal
    if (length(file.PFGdisp) > 0 && nchar(file.PFGdisp) > 0)
    {
      RV$mat.PFG.disp = foreach(fi = file.PFGdisp, .combine = 'rbind') %do%
      {
        PFG = .getParam(params.lines = fi
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
        dd = .getParam(params.lines = fi
                       , flag = "DISPERS_DIST"
                       , flag.split = " "
                       , is.num = TRUE)
        
        return(data.frame(PFG = ifelse(is.null(PFG), "", PFG)
                          , d50 = ifelse(is.null(dd), "", dd[1])
                          , d99 = ifelse(is.null(dd), "", dd[2])
                          , ldd = ifelse(is.null(dd), "", dd[3])
        ))
      }
    }
    ## Disturbances
    if (length(file.PFGdist) > 0 && nchar(file.PFGdist) > 0)
    {
      res = foreach(fi = file.PFGdist) %do%
      {
        PFG = .getParam(params.lines = fi
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
        FATES = .getParam(params.lines = fi
                          , flag = "FATES"
                          , flag.split = " "
                          , is.num = TRUE)
        
        no.dist = length(FATES) / (4 * 2)
        ind_Kill = seq(1, 4 * 2, 2)
        res = foreach(di = 1:no.dist, .combine = "rbind") %do%
        {
          res = data.frame(name = paste0("DIST_", di), responseStage = 1:4)
          eval(parse(text = paste0("res$KilledIndiv_", PFG, " = FATES[ind_Kill + (di -1) * 8]")))
          eval(parse(text = paste0("res$ResproutIndiv_", PFG, " = FATES[ind_Kill + 1 + (di -1) * 8]")))
          return(res)
        }
        return(res)
      }
      RV$mat.PFG.dist = Reduce(f = function(x, y) merge(x, y, by = c("name", "responseStage")), x = res)
    }
    ## Changing
    if ((length(file.changeMask_t) > 0 && nchar(file.changeMask_t) > 0) ||
        (length(file.changeHabsuit_t) > 0 && nchar(file.changeHabsuit_t) > 0) ||
        (length(file.changeDist_t) > 0 && nchar(file.changeDist_t) > 0) ||
        (length(file.changeDrought_t) > 0 && nchar(file.changeDrought_t) > 0) ||
        (length(file.changeAliens_t) > 0 && nchar(file.changeAliens_t) > 0) ||
        (length(file.changeFire_t) > 0 && nchar(file.changeFire_t) > 0))
    {
      RV$mat.changing = foreach(ty = c("MASK", "HABSUIT", "DIST", "DROUGHT", "ALIENS", "FIRE")
                                , .combine = 'rbind') %do%
      {
        file.change_m = switch (ty
                                , "MASK" = file.changeMask_m
                                , "HABSUIT" = file.changeHabsuit_m
                                , "DIST" = file.changeDist_m
                                , "DROUGHT" = file.changeDrought_m
                                , "ALIENS" = file.changeAliens_m
                                , "FIRE" = file.changeFire_m
        )
        if (length(file.change_m) > 0 && nchar(file.change_m) > 0)
        {
          res = foreach(fi = file.change_m, .combine = "rbind") %do%
          {
            li = readLines(fi)
            return(data.frame(opt.folder.name = sub(paste0(input$name.simul, "/DATA/SCENARIO"), "", dirname(fi))
                              , type.changing = ty
                              , year = gsub("t|.txt", "", tail(strsplit(basename(fi), "_")[[1]], 1))
                              , order = 1:length(li)
                              , file.name = li))
          }
          return(res)
        }
      }
    }
  }
})

####################################################################

observeEvent(input$create.simul, {
  if (input$create.skeleton > 0)
  {
    mask.file = list.files(path = paste0(input$name.simul, "/DATA/MASK")
                           , pattern = "^MASK_")
    if (input$upload.mask > 0 && length(mask.file) > 0)
    {
      get_res = print_messages(as.expression(
        PRE_FATE.params_simulParameters(name.simulation = input$name.simul
                                        , name.mask = mask.file
        )
      ))
    } else
    {
      shinyalert(type = "warning", text = "You must upload a simulation mask first !")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

output$FATE_simulation.zip = downloadHandler(
  filename = function(){
    paste0(input$name.simul, "_", Sys.Date(), ".zip")
  },
  content = function(file){
    zip(zipfile = file, input$name.simul)
    file.copy(file, file)
  },
  contentType = "application/zip"
)

####################################################################

observeEvent(input$refresh, {
  shinyalert(type = "warning"
             , text = paste0("The current simulation folder will be removed !\n"
                             , "Make sure you saved your previous work.")
             , showCancelButton = TRUE
             , showConfirmButton = TRUE
             , callbackR = function(x)
             {
               if (x)
               {
                 system(command = paste0("rm -r ", input$name.simul))
                 shinyjs::hide("main.panel")
                 shinyjs::disable("load.file")
                 shinyjs::disable("load.param")
                 shinyjs::disable("create.simul")
                 shinyjs::disable("FATE_simulation.zip")
                 shinyjs::disable("refresh")
               }
             })
})

