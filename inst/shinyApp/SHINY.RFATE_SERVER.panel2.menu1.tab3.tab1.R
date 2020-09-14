
####################################################################

output$UI.succ.PFG = renderUI({
  if (length(RV$names.PFG) == 0)
  {
    shinyjs::disabled(
      selectInput(inputId = "succ.PFG"
                  , label = NULL
                  , choices = RV$names.PFG
                  , selected = RV$names.PFG[1]
                  , multiple = F
                  , width = "100%")
    )
  } else
  {
    selectInput(inputId = "succ.PFG"
                , label = NULL
                , choices = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.succ$PFG))]
                , selected = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.succ$PFG))][1]
                , multiple = F
                , width = "100%")
  }
})


####################################################################

output$mat.PFG.succ = renderTable({ RV$mat.PFG.succ })

observeEvent(input$add.PFG.succ, {
  req(input$succ.PFG)
  RV$mat.PFG.succ <- rbind(RV$mat.PFG.succ
                           , data.frame(PFG = input$succ.PFG
                                        , type = input$succ.type
                                        , height = as.numeric(input$succ.height)
                                        , maturity = as.numeric(input$succ.maturity)
                                        , longevity = as.numeric(input$succ.longevity)
                           ))
})

observeEvent(input$delete.PFG.succ, {
  RV$mat.PFG.succ <- data.frame()
})

observeEvent(RV$mat.PFG.succ, {
  if (nrow(RV$mat.PFG.succ) > 0)
  {
    shinyjs::enable("create.succ")
  } else
  {
    shinyjs::disable("create.succ")
  }
})

####################################################################


observeEvent(input$create.succ, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGsuccession(name.simulation = input$name.simul
                                    , mat.PFG.succ = RV$mat.PFG.succ[, c("PFG", "type", "height", "maturity", "longevity")]
                                    , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/SUCC/"))
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})


####################################################################

get_tab.succ = eventReactive(paste(input$name.simul
                                   , input$create.succ
                                   , RV$compt.succ.no), {
                                     if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                     {
                                       path_folder = paste0(input$name.simul, "/DATA/PFGS/SUCC/")
                                       tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                       
                                       if (!is.null(tab) && ncol(tab) > 0)
                                       {
                                         RV$compt.succ.no = ncol(tab)
                                         RV$compt.succ.files = colnames(tab)
                                         return(tab)
                                       }
                                     }
                                   })

output$UI.files.succ = renderUI({
  tab = get_tab.succ()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.succ.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.succ.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.succ.select"
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
                          checkboxInput(inputId = paste0("check.succ.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.succ.", colnames(tab)[i])
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

# observeEvent(RV$compt.succ.no, {
#   for (i in 1:RV$compt.succ.no)
#   {
#     observeEvent(input[[paste0("upload.succ.", RV$compt.succ.files[i])]], {
#       get_update.succ(file.succParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/SUCC/"
#                                                   , RV$compt.succ.files[i]))
#     })
#   }
# })


observeEvent(input$check.succ.all, {
  for (col_tab in RV$compt.succ.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.succ.", col_tab)
                        , value = input$check.succ.all)
  }
})

observeEvent(input$view.succ.select, {
  output$created_table.succ = renderDataTable({
    req(grep(pattern = "check.succ.", x = names(input), value = TRUE))
    
    tab = get_tab.succ()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.succ.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.succ.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.succ.select, {
  if (input$check.succ.all)
  {
    col_toKeep = rep(TRUE,RV$compt.succ.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.succ.no, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.succ.", RV$compt.succ.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.succParam = RV$compt.succ.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/SUCC/ \n")
                               , paste0(gsub("__", "/", file.succParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.succParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/SUCC/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/SUCC/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.succ.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.succ.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.succ.no = min(0, RV$compt.succ.no - sum(col_toKeep))
                 }
               })
  }
})

