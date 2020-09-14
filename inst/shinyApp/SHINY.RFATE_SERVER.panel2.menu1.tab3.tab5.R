
####################################################################

observeEvent(input$dist.opt.resp, {
  if (input$dist.opt.resp == "user-defined")
  {
    shinyjs::enable("dist.opt.ages")
  } else
  {
    updateRadioButtons(session
                       , inputId = "dist.opt.ages"
                       , selected = "pre-defined")
    shinyjs::disable("dist.opt.ages")
  }
})

####################################################################

output$UI.dist_table = renderUI({
  max.stage = ifelse(req(input$DIST.no_sub), input$DIST.no_sub, 4)
  
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
    , fluidRow(uiOutput(outputId = "UI.dist_ages.BIS"))
    , fluidRow(uiOutput(outputId = "UI.dist_resp.BIS"))
  )
})

output$UI.dist_ages.BIS = renderUI({
  if (input$dist.opt.ages == "user-defined")
  {
    if (input$dist.opt.group == "by type")
    {
      names.col = c("H", "C", "P")
    } else
    {
      names.col = RV$names.PFG
    }
    
    if (length(names.col) > 0)
    {
      max.stage = ifelse(req(input$DIST.no_sub), input$DIST.no_sub, 4)
      selIn.names = as.vector(sapply(1:max.stage, function(x) paste0("dist.", x, ".", c("resproutAge", "breakAge"), ".")))
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

output$UI.dist_resp.BIS = renderUI({
  if (input$dist.opt.group == "by type")
  {
    names.col = c("H", "C", "P")
  } else
  {
    names.col = RV$names.PFG
  }
  
  if (length(names.col) > 0)
  {
    if (input$dist.opt.resp == "user-defined")
    {
      max.stage = ifelse(req(input$DIST.no_sub), input$DIST.no_sub, 4)
      
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
            , lapply(as.vector(sapply(1:max.stage, function(x) paste0("dist.", x, ".", c("kill", "resprout"), ".", j)))
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
    } else if (input$dist.opt.resp == "by strategy")
    {
      lapply(names.col, function(j)
      {
        fluidRow(
          column(2, HTML(paste0("<p style='text-align:right;'><strong>", j, "</strong></p>")))
          , column(9
                   , selectInput(inputId = paste0("dist.strategy_tol.", j)
                                 , label = NULL
                                 , choices = c("indifferent", "mowing_herbs", "mowing_trees"
                                               , "grazing_herbs_1", "grazing_herbs_2", "grazing_herbs_3"
                                               , "grazing_trees_1", "grazing_trees_2", "grazing_trees_3")
                                 , selected = "indifferent"
                                 , multiple = F
                                 , width = "100%"))
        )
      })
    }
  }
})


####################################################################

output$mat.PFG.dist = renderTable({ RV$mat.PFG.dist })

observeEvent(input$dist.name, {
  if (nchar(input$dist.name) > 0)
  {
    shinyjs::enable("add.PFG.dist")
  } else
  {
    shinyjs::disable("add.PFG.dist")
  }
})

observeEvent(input$add.PFG.dist, {
  if ((input$dist.opt.group == "by type" && RV$compt.dist.options[2]) ||
      (input$dist.opt.group == "by PFG" && RV$compt.dist.options[1]))
  {
    shinyalert(type = "warning", text = "You can not mix Grouping 'by type' and 'by PFG' !")
  } else if ((input$dist.opt.ages == "pre-defined" && RV$compt.dist.options[4]) ||
             (input$dist.opt.ages == "user-defined" && RV$compt.dist.options[3]))
  {
      shinyalert(type = "warning", text = "You can not mix Resprout & break ages 'pre-defined' and 'user-defined' !")
  } else if ((input$dist.opt.resp == "by strategy" && RV$compt.dist.options[6]) ||
             (input$dist.opt.resp == "user-defined" && RV$compt.dist.options[5]))
  {
    shinyalert(type = "warning", text = "You can not mix Responses 'by strategy' and 'user-defined' !")
  } else
  {
    name.cols = unlist(ifelse(input$dist.opt.group == "by type", list(c("H", "C", "P")), list(RV$names.PFG)))
    if (length(name.cols) > 0)
    {
      max.stage = ifelse(req(input$DIST.no_sub), input$DIST.no_sub, 4)
      if (max.stage != 4 && input$dist.opt.resp == "by strategy") {
        showNotification(paste0(input$dist.name
                                , " : Responses defined 'by strategy' implies 4 response stages ("
                                , input$DIST.no_sub
                                , " indicated in Global Parameters panel). "
                                , "Please check or consider changing the value!"), type = "warning")
      }
      

      res = expand.grid(nameDist = input$dist.name
                        , responseStage = 1:max.stage
                        , PFG = name.cols
                        , stringsAsFactors = FALSE)
      for (group in name.cols)
      {
        for (st in 1:max.stage)
        {
          ind.li = which(res$PFG == group & res$responseStage == st)
          if (input$dist.opt.ages == "user-defined") {
            if (st == 1){
              res[['breakAge']][ind.li] = 0
            } else
            {
              if (max.stage > 1){
                eval(parse(text = paste0("res[['breakAge']][ind.li] = input$dist.", st - 1, ".breakAge.", group)))
              }
            }
            eval(parse(text = paste0("res[['resproutAge']][ind.li] = input$dist.", st, ".resproutAge.", group)))
          }
          if (input$dist.opt.resp == "user-defined")
          {
            eval(parse(text = paste0("res[['killedIndiv']][ind.li] = as.numeric(input$dist.", st, ".kill.", group, " / 10)")))
            eval(parse(text = paste0("res[['resproutIndiv']][ind.li] = as.numeric(input$dist.", st, ".resprout.", group, " / 10)")))
          } else
          {
            eval(parse(text = paste0("res[['strategy_tol']][ind.li] = as.character(input$dist.strategy_tol.", group, ")")))
          }
        }
      }
      if (input$dist.opt.ages != "user-defined" && input$dist.opt.resp != "user-defined")
      {
        res = unique(res[, -which(colnames(res) == "responseStage")])
      }
      RV$mat.PFG.dist <- rbind(RV$mat.PFG.dist, res)
      RV$compt.dist.options = c(input$dist.opt.group == "by type"
                                , input$dist.opt.group == "by PFG"
                                , input$dist.opt.ages == "pre-defined"
                                , input$dist.opt.ages == "user-defined"
                                , input$dist.opt.resp == "by strategy"
                                , input$dist.opt.resp == "user-defined")
    }
  }
})

observeEvent(input$delete.PFG.dist, {
  RV$mat.PFG.dist <- data.frame()
  RV$compt.dist.options <- rep(FALSE, 6)
})

observeEvent(RV$mat.PFG.dist, {
  if (nrow(RV$mat.PFG.dist) > 0)
  {
    shinyjs::enable("create.dist")
  } else
  {
    shinyjs::disable("create.dist")
  }
})

####################################################################

observeEvent(input$create.dist, {
  if (input$create.skeleton > 0)
  {
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGdisturbance(name.simulation = input$name.simul
                                     , mat.PFG.tol = RV$mat.PFG.dist
                                     , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/DIST/"))
    
    if (input$DIST.no != length(unique(RV$mat.PFG.dist$nameDist))) {
      showNotification(paste0(length(unique(RV$mat.PFG.dist$nameDist))
                              , " disturbances were defined ("
                              , input$DIST.no
                              , " indicated in Global Parameters panel). "
                              , "Please check or consider changing the value!"), type = "warning")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.dist = eventReactive(paste(input$name.simul
                                   , input$create.dist
                                   , RV$compt.dist.no), {
                                     if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                     {
                                       path_folder = paste0(input$name.simul, "/DATA/PFGS/DIST/")
                                       tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                       
                                       if (!is.null(tab) && ncol(tab) > 0)
                                       {
                                         RV$compt.dist.no = ncol(tab)
                                         RV$compt.dist.files = colnames(tab)
                                         return(tab)
                                       }
                                     }
                                   })

output$UI.files.dist = renderUI({
  tab = get_tab.dist()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.dist.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.dist.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.dist.select"
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
                          checkboxInput(inputId = paste0("check.dist.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.dist.", colnames(tab)[i])
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

# observeEvent(RV$compt.dist.no, {
#   for (i in 1:RV$compt.dist.no)
#   {
#     observeEvent(input[[paste0("upload.dist.", RV$compt.dist.files[i])]], {
#       get_update.dist(file.distParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/DIST/"
#                                                   , RV$compt.dist.files[i]))
#     })
#   }
# })


observeEvent(input$check.dist.all, {
  for (col_tab in RV$compt.dist.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.dist.", col_tab)
                        , value = input$check.dist.all)
  }
})

observeEvent(input$view.dist.select, {
  output$created_table.dist = renderDataTable({
    req(grep(pattern = "check.dist.", x = names(input), value = TRUE))
    
    tab = get_tab.dist()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.dist.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
          {
            eval(parse(text = paste0("res = input$check.dist.", colnames(tab)[i])))
            return(res)
          }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.dist.select, {
  if (input$check.dist.all)
  {
    col_toKeep = rep(TRUE,RV$compt.dist.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.dist.no, .combine = "c") %do%
      {
        eval(parse(text = paste0("res = input$check.dist.", RV$compt.dist.files[i])))
        return(res)
      }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.distParam = RV$compt.dist.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/DIST/ \n")
                               , paste0(gsub("__", "/", file.distParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.distParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/DIST/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/DIST/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.dist.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.dist.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.dist.no = min(0, RV$compt.dist.no - sum(col_toKeep))
                 }
               })
  }
})

