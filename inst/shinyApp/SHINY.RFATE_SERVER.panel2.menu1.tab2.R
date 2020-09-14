
####################################################################

observeEvent(input$create.save.maps, {
  if (input$create.skeleton > 0)
  {
    if (is.numeric(input$save.maps.year1) &&
        is.numeric(input$save.maps.year2) &&
        is.numeric(input$save.maps.no))
    {
      if (input$save.maps.no > 0)
      {
        years.maps = round(seq(input$save.maps.year1
                               , input$save.maps.year2
                               , length.out = input$save.maps.no))
        years.maps = years.maps[which(years.maps > 0)]
        years.maps = sort(unique(years.maps))
        if (length(years.maps) > 0)
        {
          opt.folder.name = ifelse(nchar(input$save.maps.folder) > 0
                                   , gsub(" ", "_", input$save.maps.folder)
                                   , "")
          if (length(grep(" ", input$save.maps.folder)) > 0)
          {
            showNotification("Spaces within opt.folder.name have been replaced by `_` !", type = "warning")
          }
          
          get_res = print_messages(as.expression(
            PRE_FATE.params_saveYears(name.simulation = input$name.simul
                                      , years.maps = years.maps
                                      , years.objects = NULL
                                      , opt.folder.name = opt.folder.name
            )
          ), cut_pattern = paste0(input$name.simul, "/DATA/SAVE/"))
          
        } else
        {
          shinyalert(type = "warning", text = "No years have been selected !")
        }
      } else
      {
        shinyalert(type = "warning", text = "You must give a positive number of years !")
      }
    } else
    {
      shinyalert(type = "warning", text = "You must give numeric values !")
    } 
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

observeEvent(input$create.save.objects, {
  if (input$create.skeleton > 0)
  {
    cond1 = ifelse(!is.na(input$save.objects.year1), input$save.objects.year1 > 0, FALSE)
    cond2 = ifelse(!is.na(input$save.objects.year2), input$save.objects.year2 > 0, FALSE)
    cond3 = ifelse(!is.na(input$save.objects.year3), input$save.objects.year3 > 0, FALSE)
    if (cond1 || cond2 || cond3)
    {
      years.objects = c(input$save.objects.year1
                        , input$save.objects.year2
                        , input$save.objects.year3)
      years.objects = years.objects[which(years.objects > 0)]
      years.objects = sort(years.objects)
      
      opt.folder.name = ifelse(nchar(input$save.objects.folder) > 0
                               , gsub(" ", "_", input$save.objects.folder)
                               , "")
      if (length(grep(" ", input$save.objects.folder)) > 0)
      {
        showNotification("Spaces within opt.folder.name have been replaced by `_` !", type = "warning")
      }
      
      get_res = print_messages(as.expression(
        PRE_FATE.params_saveYears(name.simulation = input$name.simul
                                  , years.maps = NULL
                                  , years.objects = years.objects
                                  , opt.folder.name = opt.folder.name
        )
      ), cut_pattern = paste0(input$name.simul, "/DATA/SAVE/"))
      
    } else
    {
      shinyalert(type = "warning", text = "No years have been selected !")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.save = eventReactive(paste(input$name.simul
                                     , input$create.save.maps
                                     , input$create.save.objects
                                     , RV$compt.save.no), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/SAVE/")
                                         tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.save.no = ncol(tab)
                                           RV$compt.save.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.save = renderUI({
  tab = get_tab.save()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.save.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.save.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.save.select"
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
                          checkboxInput(inputId = paste0("check.save.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        , column(2
                 , lapply(1:ncol(tab)
                          , function(i) {
                            actionButton(inputId = paste0("upload.save.", colnames(tab)[i])
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

observeEvent(RV$compt.save.no, {
  for (i in 1:RV$compt.save.no)
  {
    observeEvent(input[[paste0("upload.save.", RV$compt.save.files[i])]], {
      get_update.save(file.saveParam = paste0(input$name.simul
                                                  , "/DATA/SAVE/"
                                                  , RV$compt.save.files[i]))
    })
  }
})


observeEvent(input$check.save.all, {
  for (col_tab in RV$compt.save.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.save.", col_tab)
                        , value = input$check.save.all)
  }
})

observeEvent(input$view.save.select, {
  output$created_table.save = renderDataTable({
    req(grep(pattern = "check.save.", x = names(input), value = TRUE))
    
    tab = get_tab.save()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.save.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.save.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.save.select, {
  if (input$check.save.all)
  {
    col_toKeep = rep(TRUE,RV$compt.save.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.save.no, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.save.", RV$compt.save.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.saveParam = RV$compt.save.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/SAVE/ \n")
                               , paste0(gsub("__", "/", file.saveParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.saveParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/SAVE/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/SAVE/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.save.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.save.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.save.no = min(0, RV$compt.save.no - sum(col_toKeep))
                 }
               })
  }
})
