
####################################################################

output$UI.soil.PFG = renderUI({
  if (length(RV$names.PFG) == 0)
  {
    shinyjs::disabled(
      selectInput(inputId = "soil.PFG"
                  , label = NULL
                  , choices = RV$names.PFG
                  , selected = RV$names.PFG[1]
                  , multiple = F
                  , width = "100%")
    )
  } else
  {
    selectInput(inputId = "soil.PFG"
                , label = NULL
                , choices = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.soil$PFG))]
                , selected = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.soil$PFG))][1]
                , multiple = F
                , width = "100%")
  }
})


####################################################################

output$UI.soil.opt.con = renderUI({
  if (input$soil.opt.con == "by strategy")
  {
    fluidRow(
      column(12
             , HTML("<strong>Contribution</strong>")
             , selectInput(inputId = "soil.strategy_con"
                           , label = NULL
                           , choices = c("ubiquist")
                           , selected = "ubiquist"
                           , multiple = F
                           , width = "100%"))
    )
  } else if (input$soil.opt.con == "user-defined")
  {
    fluidRow(
      column(4
             , HTML("<strong>Contribution</strong>")
             , numericInput(inputId = "soil.contrib"
                            , label = NULL
                            , value = 0
                            , min = 0
                            , max = 5
                            , width = "100%"))
      , column(4
               , HTML("<strong>Min value tolerated</strong>")
               , numericInput(inputId = "soil.tol_min"
                              , label = NULL
                              , value = 0
                              , min = 0
                              , max = 5
                              , width = "100%"))
      , column(4
               , HTML("<strong>Max value tolerated</strong>")
               , numericInput(inputId = "soil.tol_max"
                              , label = NULL
                              , value = 0
                              , min = 0
                              , max = 5
                              , width = "100%"))
    )
  }
})

####################################################################

output$UI.soil.opt.ag = renderUI({
  if (input$soil.opt.ag == "by type")
  {
    fluidRow(
      column(12
             , HTML("<strong>type</strong>")
             , selectInput(inputId = "soil.type"
                           , label = NULL
                           , choices = c("H", "C", "P")
                           , multiple = F
                           , width = "100%"))
    )
  } else if (input$soil.opt.ag == "by strategy")
  {
    fluidRow(
      column(12
             , HTML("<strong>Germination</strong>")
             , selectInput(inputId = "soil.strategy_ag"
                           , label = NULL
                           , choices = c("poor_lover", "indifferent", "rich_lover")
                           , selected = "indifferent"
                           , multiple = F
                           , width = "100%"))
    )
  } else if (input$soil.opt.ag == "user-defined")
  {
    fluidRow(
      lapply(1:3, function(x)
      {
        column(4
               , HTML(paste0("<strong>", c("Low", "Medium", "High")[x], "</strong>"))
               , selectInput(inputId = paste0("soil.Ge.", c("L", "M", "H")[x], ".act")
                             , label = NULL
                             , choices = seq(0,100,10)
                             , selected = 100
                             , multiple = FALSE
                             , width = "100%"))
      })
    )
  }
})

####################################################################

output$UI.soil.opt.tol1 = renderUI({
  if (input$soil.opt.tol == "by strategy")
  {
    fluidRow(
      column(12
           , HTML("<strong>Tolerance</strong>")
           , selectInput(inputId = "soil.strategy_tol"
                         , label = NULL
                         , choices = c("ubiquist")
                         , selected = "ubiquist"
                         , multiple = F
                         , width = "100%"))
    )
  }
})

output$UI.soil.opt.tol2 = renderUI({
  if (input$soil.opt.tol == "user-defined")
  {
    req(input$soil.PFG)
    
    tagList(
      fluidRow(
        column(4, HTML("<strong>Germinant</strong>"))
        , column(4, HTML("<strong>Immature</strong>"))
        , column(4, HTML("<strong>Mature</strong>"))
      )
      , fluidRow(
        lapply(1:3, function(i)
        {
          column(4
                 , fluidRow(
                   column(4, HTML("<strong>L</strong>"), br(), br())
                   , column(4, HTML("<strong>H</strong>"), br(), br())
                   , column(4, HTML("<strong>M</strong>"), br(), br())
                 ))
        })
      )
      , fluidRow(
        lapply(c("Ge", "Im", "Ma"), function(k)
        {
          column(4
                 , fluidRow(
                   lapply(c("L", "M", "H"), function(l)
                   {
                     column(4
                            , selectInput(inputId = paste0("soil.", k, ".", l, ".tol")
                                          , label = NULL
                                          , choices = seq(0,100,10)
                                          , multiple = FALSE
                                          , width = "100%"))
                   })
                 ))
        })
      )
    )
  }
})


####################################################################

output$mat.PFG.soil = renderTable({
  RV$mat.PFG.soil[, which(apply(RV$mat.PFG.soil, 2, function(x) length(which(!is.na(x)))) > 0)]
})

observeEvent(input$add.PFG.soil, {
  if ((input$soil.opt.con == "by strategy" && RV$compt.soil.options[2]) ||
      (input$soil.opt.con == "user-defined" && RV$compt.soil.options[1]))
  {
    shinyalert(type = "warning", text = "You can not mix Contribution 'by strategy' and 'user-defined' !")
  } else if ((input$soil.opt.ag == "by type" && (RV$compt.soil.options[4] || RV$compt.soil.options[5])) ||
             (input$soil.opt.ag == "by strategy" && (RV$compt.soil.options[3] || RV$compt.soil.options[5])) ||
             (input$soil.opt.ag == "user-defined" && (RV$compt.soil.options[3] || RV$compt.soil.options[4])))
  {
    shinyalert(type = "warning", text = "You can not mix Active germination 'by type', 'by strategy' and 'user-defined' !")
  } else if ((input$soil.opt.tol == "pre-defined" && (RV$compt.soil.options[7] || RV$compt.soil.options[8])) ||
             (input$soil.opt.tol == "by strategy" && (RV$compt.soil.options[6] || RV$compt.soil.options[8])) ||
             (input$soil.opt.tol == "user-defined" && (RV$compt.soil.options[6] || RV$compt.soil.options[7])))
  {
    shinyalert(type = "warning", text = "You can not mix Tolerance 'pre-defined', 'by strategy' and 'user-defined' !")
  } else
  {
    RV$mat.PFG.soil <- rbind(RV$mat.PFG.soil
                             , data.frame(PFG = input$soil.PFG
                                          , strategy_contrib = ifelse(input$soil.opt.con == "by strategy"
                                                                      , input$soil.strategy_con
                                                                      , NA)
                                          , soil_tol_min = ifelse(input$soil.opt.con == "user-defined"
                                                                  , as.numeric(input$soil.tol_min)
                                                                  , NA)
                                          , soil_contrib = ifelse(input$soil.opt.con == "user-defined"
                                                                  , as.numeric(input$soil.contrib)
                                                                  , NA)
                                          , soil_tol_max = ifelse(input$soil.opt.con == "user-defined"
                                                                  , as.numeric(input$soil.tol_max)
                                                                  , NA)
                                          , type = ifelse(input$soil.opt.ag == "by type"
                                                          , input$soil.type
                                                          , NA)
                                          , strategy_ag = ifelse(input$soil.opt.ag == "by strategy"
                                                                 , input$soil.strategy_ag
                                                                 , NA)
                                          , active_germ_low = ifelse(input$soil.opt.ag == "user-defined"
                                                                     , as.numeric(input$soil.Ge.L.act)
                                                                     , NA)
                                          , active_germ_medium = ifelse(input$soil.opt.ag == "user-defined"
                                                                        , as.numeric(input$soil.Ge.M.act)
                                                                        , NA)
                                          , active_germ_high = ifelse(input$soil.opt.ag == "user-defined"
                                                                      , as.numeric(input$soil.Ge.H.act)
                                                                      , NA)
                                          , strategy_tol = ifelse(input$soil.opt.tol == "by strategy"
                                                                  , input$soil.strategy_tol
                                                                  , NA)
                             ))
    if (input$soil.opt.tol == "user-defined")
    {
      combi = expand.grid(lifeStage = c("Ge", "Im", "Ma"), resources = c("L", "M", "H"))
      mat.tol = foreach(ls = combi$lifeStage, re = combi$resources, .combine = "rbind") %do%
        {
          eval(parse(text = paste0("tol = as.numeric(input$soil.", ls, ".", re, ".tol) / 10")))
          return(data.frame(PFG = input$soil.PFG
                            , lifeStage = c("Ge" = "Germinant", "Im" = "Immature", "Ma" = "Mature")[ls]
                            , resources = c("L" = "Low", "M" = "Medium", "H" = "High")[re]
                            , tolerance = tol))
        }
      RV$mat.PFG.soil.tol <- rbind(RV$mat.PFG.soil.tol, mat.tol)
    }
    RV$compt.soil.options = c(input$soil.opt.con == "by strategy"
                              , input$soil.opt.con == "user-defined"
                              , input$soil.opt.ag == "by type"
                              , input$soil.opt.ag == "by strategy"
                              , input$soil.opt.ag == "user-defined"
                              , input$soil.opt.tol == "pre-defined"
                              , input$soil.opt.tol == "by strategy"
                              , input$soil.opt.tol == "user-defined")
  }
  shinyjs::enable("create.soil")
})

observeEvent(input$delete.PFG.soil, {
  RV$mat.PFG.soil <- data.frame()
  RV$compt.soil.options <- rep(FALSE, 8)
})

observeEvent(RV$mat.PFG.soil, {
  if (nrow(RV$mat.PFG.soil) > 0)
  {
    shinyjs::enable("create.soil")
  } else
  {
    shinyjs::disable("create.soil")
  }
})

####################################################################

observeEvent(input$create.soil, {
  if (input$create.skeleton > 0)
  {
    col.soil = c("PFG", switch (input$soil.opt.con
                                , "by strategy" = "strategy_contrib"
                                , "user-defined" = c("soil_contrib"
                                                     , "soil_tol_min"
                                                     , "soil_tol_max")))
    col.soil = c(col.soil, switch (input$soil.opt.ag
                                   , "by type" = "type"
                                   , "by strategy" = "strategy_ag"
                                   , "user-defined" = c("active_germ_low"
                                                        , "active_germ_medium"
                                                        , "active_germ_high")))
    mat.tol = switch (input$soil.opt.tol
                      , "pre-defined" = NULL
                      , "by strategy" = RV$mat.PFG.soil[, c("PFG", "strategy_tol")]
                      , "user-defined" = RV$mat.PFG.soil.tol)
    
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGsoil(name.simulation = input$name.simul
                              , mat.PFG.soil = RV$mat.PFG.soil[, unique(col.soil)]
                              , mat.PFG.tol = mat.tol
                              , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/SOIL/"))
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})

####################################################################

get_tab.soil = eventReactive(paste(input$name.simul
                                     , input$create.soil
                                     , RV$compt.soil.no), {
                                       if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                       {
                                         path_folder = paste0(input$name.simul, "/DATA/PFGS/SOIL/")
                                         tab = get_files(path_folder, skip.no = 0, opt.sub_folder = TRUE)
                                         
                                         if (!is.null(tab) && ncol(tab) > 0)
                                         {
                                           RV$compt.soil.no = ncol(tab)
                                           RV$compt.soil.files = colnames(tab)
                                           return(tab)
                                         }
                                       }
                                     })

output$UI.files.soil = renderUI({
  tab = get_tab.soil()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.soil.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.soil.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.soil.select"
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
                          checkboxInput(inputId = paste0("check.soil.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.soil.", colnames(tab)[i])
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

# observeEvent(RV$compt.soil.no, {
#   for (i in 1:RV$compt.soil.no)
#   {
#     observeEvent(input[[paste0("upload.soil.", RV$compt.soil.files[i])]], {
#       get_update.soil(file.soilParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/SOIL/"
#                                                   , RV$compt.soil.files[i]))
#     })
#   }
# })


observeEvent(input$check.soil.all, {
  for (col_tab in RV$compt.soil.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.soil.", col_tab)
                        , value = input$check.soil.all)
  }
})

observeEvent(input$view.soil.select, {
  output$created_table.soil = renderDataTable({
    req(grep(pattern = "check.soil.", x = names(input), value = TRUE))
    
    tab = get_tab.soil()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.soil.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.soil.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.soil.select, {
  if (input$check.soil.all)
  {
    col_toKeep = rep(TRUE,RV$compt.soil.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.soil.no, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.soil.", RV$compt.soil.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.soilParam = RV$compt.soil.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/SOIL/ \n")
                               , paste0(gsub("__", "/", file.soilParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.soilParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/SOIL/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/SOIL/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.soil.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.soil.", fi)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.soil.no = min(0, RV$compt.soil.no - sum(col_toKeep))
                 }
               })
  }
})

