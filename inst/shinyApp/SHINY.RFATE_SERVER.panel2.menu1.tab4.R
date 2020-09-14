
####################################################################

observeEvent(input$upload.mask, {
  if (input$create.skeleton > 0)
  {
    if(!is.null(input$simul.mask))
    {
      file1 = input$simul.mask$datapath
      file2 = gsub(" ", "_", input$simul.mask$name)
      file2 = paste0("MASK_", file2)
      file2 = paste0(input$name.simul, "/DATA/MASK/", file2)
      get_res = file.copy(from = file1, to = file2)

      if(sum(get_res) == length(get_res))
      {
        if (length(grep(" ", input$simul.mask$name)) > 0)
        {
          showNotification("Spaces within name.mask have been replaced by `_` !", type = "warning")
        }
        shinyalert(type = "success", text = paste0("The file ", input$simul.mask$name
                                                   , " has been correctly uploaded and renamed as "
                                                   , sub(paste0(input$name.simul, "/DATA/MASK/")
                                                         , paste0(input$name.simul, "/DATA/MASK/ ")
                                                         , file2)
                                                   , " !"))
      } else
      {
        shinyalert(type = "error", text = "Oops. Something went wrong !")
      }
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

observeEvent(input$upload.habsuit.mask, {
  if (input$create.skeleton > 0)
  {
    if(!is.null(input$habsuit.mask))
    {
      if (nchar(input$habsuit.folder) > 0)
      {
        dir.create(paste0(input$name.simul, "/DATA/PFGS/HABSUIT/", input$habsuit.folder))
        opt.folder.name = paste0(input$habsuit.folder, "/")
      } else
      {
        opt.folder.name = ""
      }
      
      file1 = input$habsuit.mask$datapath
      file2 = gsub(" ", "_", input$habsuit.mask$name)
      file2 = paste0("HABSUIT_", file2)
      file2 = paste0(input$name.simul, "/DATA/PFGS/HABSUIT/", opt.folder.name, file2)
      get_res = file.copy(from = file1, to = file2)
      
      if(sum(get_res) == length(get_res))
      {
        if (length(grep(" ", input$habsuit.mask$name)) > 0)
        {
          showNotification("Spaces within habsuit.mask have been replaced by `_` !", type = "warning")
        }
        shinyalert(type = "success", text = paste0("The file ", input$habsuit.mask$name
                                                   , " has been correctly uploaded and renamed as "
                                                   , sub(paste0(input$name.simul, "/DATA/PFGS/HABSUIT/", opt.folder.name)
                                                         , paste0(input$name.simul, "/DATA/PFGS/HABSUIT/", opt.folder.name, " ")
                                                         , file2)
                                                   , " !"))
      } else
      {
        shinyalert(type = "error", text = "Oops. Something went wrong !")
      }
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})
####################################################################

observeEvent(input$upload.dist.mask, {
  if (input$create.skeleton > 0)
  {
    if(!is.null(input$dist.mask))
    {
      file1 = input$dist.mask$datapath
      file2 = gsub(" ", "_", input$dist.mask$name)
      file2 = paste0("DIST_", file2)
      file2 = paste0(input$name.simul, "/DATA/MASK/", file2)
      get_res = file.copy(from = file1, to = file2)

      if(sum(get_res) == length(get_res))
      {
        if (length(grep(" ", input$dist.mask$name)) > 0)
        {
          showNotification("Spaces within dist.mask have been replaced by `_` !", type = "warning")
        }
        shinyalert(type = "success", text = paste0("The file ", input$dist.mask$name
                                                   , " has been correctly uploaded and renamed as "
                                                   , sub(paste0(input$name.simul, "/DATA/MASK/")
                                                         , paste0(input$name.simul, "/DATA/MASK/ ")
                                                         , file2)
                                                   , " !"))
      } else
      {
        shinyalert(type = "error", text = "Oops. Something went wrong !")
      }
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

observeEvent(input$refresh.changing, {
  names.raster1 = list.files(path = paste0(input$name.simul, "/DATA/MASK")
                             , pattern = ".tif$|.img$"
                             , all.files = FALSE
                             , full.names = TRUE)
  names.raster2 = list.files(path = paste0(input$name.simul, "/DATA/PFGS/HABSUIT")
                             , pattern = ".tif$|.img$"
                             , all.files = FALSE
                             , full.names = TRUE
                             , recursive = TRUE)
  names.raster = c(names.raster1, names.raster2)
  names.raster = sub(paste0(".*", input$name.simul), input$name.simul, names.raster)
  if (length(names.raster) > 0)
  {
    updateSelectInput(session = session
                      , inputId = "changing.file"
                      , label = NULL
                      , choices = names.raster
                      , selected = names.raster[1])
    
    shinyjs::enable("add.changing")
  } else
  {
    shinyjs::reset("changing.file")
    shinyjs::disable("add.changing")
  }
})


####################################################################

output$mat.changing = renderTable({ RV$mat.changing })

observeEvent(input$add.changing, {
  RV$mat.changing <- rbind(RV$mat.changing
                         , data.frame(opt.folder.name = input$changing.folder
                                      , type.changing = input$type.changing
                                      , year = input$changing.year
                                      , order = input$changing.order
                                      , file.name = input$changing.file))
  
  shinyjs::enable("create.changing")
})

observeEvent(input$delete.changing, {
  RV$mat.changing <- data.frame()
  shinyjs::disable("create.changing")
})

####################################################################

observeEvent(input$create.changing, {
  if (input$create.skeleton > 0)
  {
    mat.changing.split = split(RV$mat.changing, list(RV$mat.changing$type.changing, RV$mat.changing$opt.folder.name))
    for(i in 1:length(mat.changing.split))
    {
      tab = mat.changing.split[[i]]
      tab$opt.folder.name = as.character(tab$opt.folder.name)
      tab$file.name = as.character(tab$file.name)
      if (nrow(tab) > 0 && sum(nchar(tab$file.name) == 0) == 0)
      {
        if (nchar(unique(tab$opt.folder.name)) > 0)
        {
          dir.create(paste0(input$name.simul, "/DATA/SCENARIO/", unique(tab$opt.folder.name)))
        }
        
        get_res = print_messages(as.expression(
          PRE_FATE.params_changingYears(name.simulation = input$name.simul
                                        , type.changing = unique(tab$type.changing)
                                        , mat.changing = data.frame(year = tab$year
                                                                    , order = tab$order
                                                                    , file.name = tab$file.name)
                                        , opt.folder.name = unique(tab$opt.folder.name)
          )
        ), cut_pattern = paste0(input$name.simul, "/DATA/SCENARIO/"))

      }
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.changing = eventReactive(paste(input$name.simul
                                     , input$create.changing
                                     , RV$compt.changing.no), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/SCENARIO/")
                                         tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.changing.no = ncol(tab)
                                           RV$compt.changing.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.changing = renderUI({
  tab = get_tab.changing()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.changing.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.changing.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.changing.select"
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
                          checkboxInput(inputId = paste0("check.changing.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.changing.", colnames(tab)[i])
        #                                  , label = NULL
        #                                  , icon = icon("upload")
        #                                  , width = "100%"
        #                                  , style = button.style.action)
        #                   })
        # )
      )
    )
  }
})

# observeEvent(RV$compt.changing.no, {
#   for (i in 1:RV$compt.changing.no)
#   {
#     observeEvent(input[[paste0("upload.changing.", RV$compt.changing.files[i])]], {
#       get_update.changing(file.changingParam = paste0(input$name.simul
#                                                   , "/DATA/SCENARIO/"
#                                                   , RV$compt.changing.files[i]))
#     })
#   }
# })


observeEvent(input$check.changing.all, {
  for (col_tab in RV$compt.changing.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.changing.", col_tab)
                        , value = input$check.changing.all)
  }
})

observeEvent(input$view.changing.select, {
  output$created_table.changing = renderDataTable({
    req(grep(pattern = "check.changing.", x = names(input), value = TRUE))
    
    tab = get_tab.changing()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.changing.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.changing.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.changing.select, {
  if (input$check.changing.all)
  {
    col_toKeep = rep(TRUE,RV$compt.changing.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.changing.no, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.changing.", RV$compt.changing.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.changingParam = RV$compt.changing.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/SCENARIO/ \n")
                               , paste0(gsub("__", "/", file.changingParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.changingParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/SCENARIO/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/SCENARIO/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.changing.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.changing.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.changing.no = min(0, RV$compt.changing.no - sum(col_toKeep))
                 }
               })
  }
})


