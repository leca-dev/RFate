
####################################################################

output$UI.disp.PFG = renderUI({
  if (length(RV$names.PFG) == 0)
  {
    shinyjs::disabled(
      selectInput(inputId = "disp.PFG"
                  , label = NULL
                  , choices = RV$names.PFG
                  , selected = RV$names.PFG[1]
                  , multiple = F
                  , width = "100%")
    )
  } else
  {
    selectInput(inputId = "disp.PFG"
                , label = NULL
                , choices = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.disp$PFG))]
                , selected = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.disp$PFG))][1]
                , multiple = F
                , width = "100%")
  }
})


####################################################################

output$mat.PFG.disp = renderTable({ RV$mat.PFG.disp })

observeEvent(input$add.PFG.disp, {
  RV$mat.PFG.disp <- rbind(RV$mat.PFG.disp
                           , data.frame(PFG = input$disp.PFG
                                        , d50 = as.numeric(input$disp.d50)
                                        , d99 = as.numeric(input$disp.d99)
                                        , ldd = as.numeric(input$disp.ldd)))
})

observeEvent(input$delete.PFG.disp, {
  RV$mat.PFG.disp <- data.frame()
})

observeEvent(RV$mat.PFG.disp, {
  if (nrow(RV$mat.PFG.disp) > 0)
  {
    shinyjs::enable("create.disp")
  } else
  {
    shinyjs::disable("create.disp")
  }
})

####################################################################

observeEvent(input$create.disp, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGdispersal(name.simulation = input$name.simul
                                   , mat.PFG.disp = RV$mat.PFG.disp
                                   , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/DISP/"))
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.disp = eventReactive(paste(input$name.simul
                                     , input$create.disp
                                     , RV$compt.disp.no), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/PFGS/DISP/")
                                         tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.disp.no = ncol(tab)
                                           RV$compt.disp.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.disp = renderUI({
  tab = get_tab.disp()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.disp.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.disp.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.disp.select"
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
                          checkboxInput(inputId = paste0("check.disp.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.disp.", colnames(tab)[i])
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

# observeEvent(RV$compt.disp.no, {
#   for (i in 1:RV$compt.disp.no)
#   {
#     observeEvent(input[[paste0("upload.disp.", RV$compt.disp.files[i])]], {
#       get_update.disp(file.dispParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/DISP/"
#                                                   , RV$compt.disp.files[i]))
#     })
#   }
# })


observeEvent(input$check.disp.all, {
  for (col_tab in RV$compt.disp.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.disp.", col_tab)
                        , value = input$check.disp.all)
  }
})

observeEvent(input$view.disp.select, {
  output$created_table.disp = renderDataTable({
    req(grep(pattern = "check.disp.", x = names(input), value = TRUE))
    
    tab = get_tab.disp()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.disp.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.disp.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.disp.select, {
  if (input$check.disp.all)
  {
    col_toKeep = rep(TRUE,RV$compt.disp.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.disp.no, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.disp.", RV$compt.disp.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.dispParam = RV$compt.disp.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/DISP/ \n")
                               , paste0(gsub("__", "/", file.dispParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.dispParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/DISP/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/DISP/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.disp.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.disp.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.disp.no = min(0, RV$compt.disp.no - sum(col_toKeep))
                 }
               })
  }
})

