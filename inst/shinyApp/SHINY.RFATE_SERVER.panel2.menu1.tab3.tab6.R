
####################################################################

observeEvent(input$drought.opt.resp, {
  if (input$drought.opt.resp == "user-defined")
  {
    shinyjs::enable("drought.opt.ages")
  } else
  {
    updateRadioButtons(session
                       , inputId = "drought.opt.ages"
                       , selected = "pre-defined")
    shinyjs::disable("drought.opt.ages")
  }
})

####################################################################

output$UI.drought_table = renderUI({
  max.stage = ifelse(req(input$DROUGHT.no_sub), input$DROUGHT.no_sub, 4)
  
  tagList(
    fluidRow(
      column(2, br())
      , lapply(1:max.stage, function(i)
      {
        column(max(trunc(10/max.stage), 1)
               , HTML(paste0("<strong>STAGE ", i, "</strong>"))
               , br(), br()
        )
      })
    )
    , fluidRow(uiOutput(outputId = "UI.drought_ages.BIS"))
    , fluidRow(uiOutput(outputId = "UI.drought_resp.BIS"))
  )
})

output$UI.drought_ages.BIS = renderUI({
  if (input$drought.opt.ages == "user-defined")
  {
    if (input$drought.opt.group == "by type")
    {
      names.col = c("H", "C", "P")
    } else
    {
      names.col = RV$names.PFG
    }
    
    if (length(names.col) > 0)
    {
      max.stage = ifelse(req(input$DROUGHT.no_sub), input$DROUGHT.no_sub, 4)
      selIn.names = as.vector(sapply(1:max.stage, function(x) paste0("drought.", x, ".", c("resproutAge", "breakAge"), ".")))
      selIn.names = selIn.names[-length(selIn.names)]
      
      tagList(
        fluidRow(
          column(2, br())
          , lapply(1:(2 * max.stage - 1), function(i)
          {
            column(max(trunc(10/max.stage) / 2, 1)
                   , HTML(paste0("<strong>", c("Break", "Resprout")[i %% 2 + 1], " age</strong>"))
            )
          })
        )
        , lapply(names.col, function(j)
        {
          fluidRow(
            column(2, HTML(paste0("<p style='text-align:right;'><strong>", j, "</strong></p>")))
            , lapply(paste0(selIn.names, j)
                     , function(i) {
                       column(max(trunc(10/max.stage) / 2, 1)
                              , numericInput(inputId = i
                                             , label = NULL
                                             , value = 0
                                             , min = 0
                                             , step = 1
                                             , width = "100%"))
                     })
          )
        })
      )
    }
  }
})

output$UI.drought_resp.BIS = renderUI({
  if (input$drought.opt.group == "by type")
  {
    names.col = c("H", "C", "P")
  } else
  {
    names.col = RV$names.PFG
  }
  
  if (length(names.col) > 0)
  {
    if (input$drought.opt.resp == "user-defined")
    {
      max.stage = ifelse(req(input$DROUGHT.no_sub), input$DROUGHT.no_sub, 4)
      
      tagList(
        fluidRow(
          column(2, br())
          , lapply(1:max.stage, function(i)
          {
            column(max(trunc(10/max.stage), 1)
                   , fluidRow(
                     column(6, HTML("<strong>% killed</strong>"), br(), br())
                     , column(6, HTML("<strong>% resprout</strong>"), br(), br())
                   ))
          })
        )
        , lapply(names.col, function(j)
        {
          fluidRow(
            column(2, HTML(paste0("<p style='text-align:right;'><strong>", j, "</strong></p>")))
            , lapply(as.vector(sapply(1:max.stage, function(x) paste0("drought.", x, ".", c("kill", "resprout"), ".", j)))
                     , function(i) {
                       column(max(trunc(10/max.stage) / 2, 1)
                              , selectInput(inputId = i
                                            , label = NULL
                                            , choices = seq(0,100,10)
                                            , multiple = FALSE
                                            , width = "100%"))
                     })
          )
        })
      )
    } else if (input$drought.opt.resp == "by strategy")
    {
      lapply(names.col, function(j)
      {
        fluidRow(
          column(2, HTML(paste0("<p style='text-align:right;'><strong>", j, "</strong></p>")))
          , column(9
                   , selectInput(inputId = paste0("drought.strategy_tol.", j)
                                 , label = NULL
                                 , choices = c("herbs_cham_1", "herbs_cham_2", "herbs_cham_3"
                                               , "trees_1", "trees_2", "trees_3")
                                 , selected = "herbs_cham_1"
                                 , multiple = F
                                 , width = "100%"))
        )
      })
    }
  }
})

output$UI.drought_table.BIS = renderUI({
  if (input$drought.opt.group == "by type")
  {
    names.col = c("H", "C", "P")
  } else
  {
    names.col = RV$names.PFG
  }
  
  if (length(names.col) > 0)
  {
    if (input$drought.opt.sens == "user-defined")
    {
      tagList(
        fluidRow(
          column(2, br())
          , column(2, HTML("<strong>Threshold<br/> moderate</strong>"), br(), br())
          , column(2, HTML("<strong>Threshold<br/> severe</strong>"), br(), br())
          , column(2, HTML("<strong>Counter<br/> recovery</strong>"), br(), br())
          , column(2, HTML("<strong>Counter<br/> sensitivity</strong>"), br(), br())
          , column(2, HTML("<strong>Counter<br/> cumulative</strong>"), br(), br())
        )
        , lapply(names.col, function(j)
        {
          fluidRow(
            column(2, HTML(paste0("<p style='text-align:right;'><strong>", j, "</strong></p>")))
            , lapply(paste0("drought.", c("threshMod", "threshSev"
                                          , "countRec", "countSens", "countCum"), ".", j)
                     , function(i) {
                       column(2, numericInput(inputId = i
                                              , label = NULL
                                              , value = 0
                                              , width = "100%"))
                     })
          )
        })
      )
    } else if (input$drought.opt.sens == "by strategy")
    {
      tagList(
        fluidRow(
          column(2, br())
          , column(2, HTML("<strong>Threshold<br/> moderate</strong>"), br(), br())
          , column(2, HTML("<strong>Threshold<br/> severe</strong>"), br(), br())
          , column(6, HTML("<strong>Sensitivity</strong>"), br(), br())
        )
        , lapply(names.col, function(j)
        {
          fluidRow(
            column(2, HTML(paste0("<p style='text-align:right;'><strong>", j, "</strong></p>")))
            , column(2, numericInput(inputId = paste0("drought.threshMod.", j)
                                     , label = NULL
                                     , value = 0
                                     , width = "100%"))
            , column(2, numericInput(inputId = paste0("drought.threshSev.", j)
                                     , label = NULL
                                     , value = 0
                                     , width = "100%"))
            , column(6
                     , selectInput(inputId = paste0("drought.strategy_sens.", j)
                                   , label = NULL
                                   , choices = c("herbs", "chamaephytes", "trees_shrubs")
                                   , selected = "herbs"
                                   , multiple = F
                                   , width = "100%"))
          )
        })
      )
    }
  }
})


####################################################################

output$mat.PFG.drought = renderTable({ RV$mat.PFG.drought })

observeEvent(input$drought.name, {
  if (nchar(input$drought.name) > 0)
  {
    shinyjs::enable("add.PFG.drought")
  } else
  {
    shinyjs::disable("add.PFG.drought")
  }
})

observeEvent(input$add.PFG.drought, {
  if ((input$drought.opt.group == "by type" && RV$compt.drought.options[2]) ||
      (input$drought.opt.group == "by PFG" && RV$compt.drought.options[1]))
  {
    shinyalert(type = "warning", text = "You can not mix Grouping 'by type' and 'by PFG' !")
  } else if ((input$drought.opt.ages == "pre-defined" && RV$compt.drought.options[4]) ||
             (input$drought.opt.ages == "user-defined" && RV$compt.drought.options[3]))
  {
    shinyalert(type = "warning", text = "You can not mix Resprout & break ages 'pre-defined' and 'user-defined' !")
  } else if ((input$drought.opt.resp == "by strategy" && RV$compt.drought.options[6]) ||
             (input$drought.opt.resp == "user-defined" && RV$compt.drought.options[5]))
  {
    shinyalert(type = "warning", text = "You can not mix Responses 'by strategy' and 'user-defined' !")
  } else if ((input$drought.opt.sens == "by strategy" && RV$compt.drought.options[8]) ||
             (input$drought.opt.sens == "user-defined" && RV$compt.drought.options[7]))
  {
    shinyalert(type = "warning", text = "You can not mix Sensitivity 'by strategy' and 'user-defined' !")
  } else
  {
    name.cols = unlist(ifelse(input$drought.opt.group == "by type", list(c("H", "C", "P")), list(RV$names.PFG)))
    if (length(name.cols) > 0)
    {
      max.stage = ifelse(req(input$DROUGHT.no_sub), input$DROUGHT.no_sub, 4)
      if (max.stage != 4 && input$drought.opt.resp == "by strategy") {
        showNotification(paste0(input$drought.name
                                , " : Responses defined 'by strategy' implies 4 response stages ("
                                , input$DROUGHT.no_sub
                                , " indicated in Global Parameters panel). "
                                , "Please check or consider changing the value!"), type = "warning")
      }
      
      
      res = expand.grid(nameDist = input$drought.name
                        , responseStage = 1:max.stage
                        , PFG = name.cols
                        , stringsAsFactors = FALSE)
      for (group in name.cols)
      {
        for (st in 1:max.stage)
        {
          ind.li = which(res$PFG == group & res$responseStage == st)
          if (input$drought.opt.ages == "user-defined") {
            if (st == 1){
              res[['breakAge']][ind.li] = 0
            } else
            {
              if (max.stage > 1){
                eval(parse(text = paste0("res[['breakAge']][ind.li] = input$drought.", st - 1, ".breakAge.", group)))
              }
            }
            eval(parse(text = paste0("res[['resproutAge']][ind.li] = input$drought.", st, ".resproutAge.", group)))
          }
          if (input$drought.opt.resp == "user-defined")
          {
            eval(parse(text = paste0("res[['killedIndiv']][ind.li] = as.numeric(input$drought.", st, ".kill.", group, " / 10)")))
            eval(parse(text = paste0("res[['resproutIndiv']][ind.li] = as.numeric(input$drought.", st, ".resprout.", group, " / 10)")))
          } else
          {
            eval(parse(text = paste0("res[['strategy_tol']][ind.li] = as.character(input$drought.strategy_tol.", group, ")")))
          }
        }
      }
      if (input$drought.opt.ages != "user-defined" && input$drought.opt.resp != "user-defined")
      {
        res = unique(res[, -which(colnames(res) == "responseStage")])
      }
      RV$mat.PFG.drought <- rbind(RV$mat.PFG.drought, res)
      
      mat.tol = foreach(group = name.cols, .combine = "rbind") %do%
      {
        eval(parse(text = paste0("threshMod = input$drought.threshMod.", group)))
        eval(parse(text = paste0("threshSev = input$drought.threshSev.", group)))
        res = data.frame(PFG = group
                         , threshold_moderate = threshMod
                         , threshold_severe = threshSev
                         , stringsAsFactors = FALSE)
        if (input$drought.opt.sens == "user-defined")
        {
          eval(parse(text = paste0("res[['counter_recovery']] = input$drought.countRec.", group)))
          eval(parse(text = paste0("res[['counter_sens']] = input$drought.countSens.", group)))
          eval(parse(text = paste0("res[['counter_cum']] = input$drought.countCum.", group)))
        } else
        {
          eval(parse(text = paste0("res[['strategy_drou']] = input$drought.strategy_sens.", group)))
        }
        return(res)
      }
      RV$mat.PFG.drought.tol <- rbind(RV$mat.PFG.drought.tol, mat.tol)
      
      RV$compt.drought.options = c(input$drought.opt.group == "by type"
                                   , input$drought.opt.group == "by PFG"
                                   , input$drought.opt.ages == "pre-defined"
                                   , input$drought.opt.ages == "user-defined"
                                   , input$drought.opt.resp == "by strategy"
                                   , input$drought.opt.resp == "user-defined"
                                   , input$drought.opt.sens == "by strategy"
                                   , input$drought.opt.sens == "user-defined")
    }
  }
})

observeEvent(input$delete.PFG.drought, {
  RV$mat.PFG.drought <- data.frame()
  RV$mat.PFG.drought.tol <- data.frame()
  RV$compt.drought.options <- rep(FALSE, 8)
})

observeEvent(RV$mat.PFG.drought, {
  if (nrow(RV$mat.PFG.drought) > 0)
  {
    shinyjs::enable("create.drought")
  } else
  {
    shinyjs::disable("create.drought")
  }
})

####################################################################

observeEvent(input$create.drought, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGdrought(name.simulation = input$name.simul
                                 , mat.PFG.tol = RV$mat.PFG.drought
                                 , mat.PFG.drought = RV$mat.PFG.drought.tol
                                 , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/DROUGHT/"))
    
    if (2 != length(unique(RV$mat.PFG.drought$nameDist))) {
      showNotification(paste0(length(unique(RV$mat.PFG.drought$nameDist))
                              , " drought were defined (2 expected : 'immediate' and 'delayed'). "
                              , "Please check!"), type = "warning")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.drought = eventReactive(paste(input$name.simul
                                      , input$create.drought
                                      , RV$compt.drought.no), {
                                        if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                        {
                                          path_folder = paste0(input$name.simul, "/DATA/PFGS/DROUGHT/")
                                          tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                          
                                          if (!is.null(tab) && ncol(tab) > 0)
                                          {
                                            RV$compt.drought.no = ncol(tab)
                                            RV$compt.drought.files = colnames(tab)
                                            return(tab)
                                          }
                                        }
                                      })

output$UI.files.drought = renderUI({
  tab = get_tab.drought()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.drought.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.drought.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.drought.select"
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
                          checkboxInput(inputId = paste0("check.drought.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.drought.", colnames(tab)[i])
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

# observeEvent(RV$compt.drought.no, {
#   for (i in 1:RV$compt.drought.no)
#   {
#     observeEvent(input[[paste0("upload.drought.", RV$compt.drought.files[i])]], {
#       get_update.drought(file.droughtParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/DROUGHT/"
#                                                   , RV$compt.drought.files[i]))
#     })
#   }
# })


observeEvent(input$check.drought.all, {
  for (col_tab in RV$compt.drought.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.drought.", col_tab)
                        , value = input$check.drought.all)
  }
})

observeEvent(input$view.drought.select, {
  output$created_table.drought = renderDataTable({
    req(grep(pattern = "check.drought.", x = names(input), value = TRUE))
    
    tab = get_tab.drought()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.drought.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.drought.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.drought.select, {
  if (input$check.drought.all)
  {
    col_toKeep = rep(TRUE,RV$compt.drought.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.drought.no, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.drought.", RV$compt.drought.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.droughtParam = RV$compt.drought.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/DROUGHT/ \n")
                               , paste0(gsub("__", "/", file.droughtParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.droughtParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/DROUGHT/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/DROUGHT/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.drought.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.drought.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.drought.no = min(0, RV$compt.drought.no - sum(col_toKeep))
                 }
               })
  }
})

