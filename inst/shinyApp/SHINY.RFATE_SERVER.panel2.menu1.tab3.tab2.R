
####################################################################

output$UI.light.PFG = renderUI({
  if (length(RV$names.PFG) == 0)
  {
    shinyjs::disabled(
      selectInput(inputId = "light.PFG"
                  , label = NULL
                  , choices = RV$names.PFG
                  , selected = RV$names.PFG[1]
                  , multiple = F
                  , width = "100%")
    )
  } else
  {
    selectInput(inputId = "light.PFG"
                , label = NULL
                , choices = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.light$PFG))]
                , selected = RV$names.PFG[which(!(RV$names.PFG %in% RV$mat.PFG.light$PFG))][1]
                , multiple = F
                , width = "100%")
  }
})


####################################################################

output$UI.light.opt.tl = renderUI({
  if (input$light.opt.tol == "by type & light")
  {
    fluidRow(
      column(6
             , HTML("<strong>type</strong>")
             , selectInput(inputId = "light.type"
                           , label = NULL
                           , choices = c("H", "C", "P")
                           , multiple = F
                           , width = "100%"))
      , column(6
               , HTML("<strong>light</strong>")
               , selectInput(inputId = "light.light"
                             , label = NULL
                             , choices = 0:5
                             , multiple = F
                             , width = "100%"))
    )
  } else if (input$light.opt.ag == "by type")
  {
    fluidRow(
      column(6
             , HTML("<strong>type</strong>")
             , selectInput(inputId = "light.type"
                           , label = NULL
                           , choices = c("H", "C", "P")
                           , multiple = F
                           , width = "100%"))
      , column(6, br())
    )
  }
})

####################################################################

output$UI.light.opt.ag = renderUI({
  if (input$light.opt.ag == "by strategy")
  {
    fluidRow(
      column(12
             , HTML("<strong>Germination</strong>")
             , selectInput(inputId = "light.strategy_ag"
                           , label = NULL
                           , choices = c("light_lover", "indifferent", "shade_lover")
                           , selected = "indifferent"
                           , multiple = F
                           , width = "100%"))
    )
  } else if (input$light.opt.ag == "user-defined")
  {
    fluidRow(
      lapply(1:3, function(x)
      {
        column(4
               , HTML(paste0("<strong>", c("Low", "Medium", "High")[x], "</strong>"))
               , selectInput(inputId = paste0("light.Ge.", c("L", "M", "H")[x], ".act")
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

output$UI.light.opt.tol1 = renderUI({
  if (input$light.opt.tol == "by strategy")
  {
    fluidRow(
      column(12
             , HTML("<strong>Tolerance</strong>")
             , selectInput(inputId = "light.strategy_tol"
                           , label = NULL
                           , choices = c("full_light", "pioneer", "ubiquist"
                                         , "semi_shade", "undergrowth")
                           , selected = "ubiquist"
                           , multiple = F
                           , width = "100%"))
    )
  }
})

output$UI.light.opt.tol2 = renderUI({
  if (input$light.opt.tol == "user-defined")
  {
    req(input$light.PFG)
    
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
                            , selectInput(inputId = paste0("light.", k, ".", l, ".tol")
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

output$mat.PFG.light = renderTable({
  RV$mat.PFG.light[, which(apply(RV$mat.PFG.light, 2, function(x) length(which(!is.na(x)))) > 0)]
})

observeEvent(input$add.PFG.light, {
  if ((input$light.opt.ag == "by type" && (RV$compt.light.options[2] || RV$compt.light.options[3])) ||
      (input$light.opt.ag == "by strategy" && (RV$compt.light.options[1] || RV$compt.light.options[3])) ||
      (input$light.opt.ag == "user-defined" && (RV$compt.light.options[1] || RV$compt.light.options[2])))
  {
    shinyalert(type = "warning", text = "You can not mix Active germination 'by type', 'by strategy' and 'user-defined' !")
  } else if ((input$light.opt.tol == "by type & light" && (RV$compt.light.options[5] || RV$compt.light.options[6])) ||
             (input$light.opt.tol == "by strategy" && (RV$compt.light.options[4] || RV$compt.light.options[6])) ||
             (input$light.opt.tol == "user-defined" && (RV$compt.light.options[4] || RV$compt.light.options[5])))
  {
    shinyalert(type = "warning", text = "You can not mix Tolerance 'by type & light', 'by strategy' and 'user-defined' !")
  } else
  {
    RV$mat.PFG.light <- rbind(RV$mat.PFG.light
                              , data.frame(PFG = input$light.PFG
                                           , type = ifelse(input$light.opt.tol == "by type & light" ||
                                                             input$light.opt.ag == "by type"
                                                           , input$light.type
                                                           , NA)
                                           , light_need = ifelse(input$light.opt.tol == "by type & light"
                                                                 , as.numeric(input$light.light)
                                                                 , NA)
                                           , strategy_ag = ifelse(input$light.opt.ag == "by strategy"
                                                                  , input$light.strategy_ag
                                                                  , NA)
                                           , active_germ_low = ifelse(input$light.opt.ag == "user-defined"
                                                                      , as.numeric(input$light.Ge.L.act)
                                                                      , NA)
                                           , active_germ_medium = ifelse(input$light.opt.ag == "user-defined"
                                                                         , as.numeric(input$light.Ge.M.act)
                                                                         , NA)
                                           , active_germ_high = ifelse(input$light.opt.ag == "user-defined"
                                                                       , as.numeric(input$light.Ge.H.act)
                                                                       , NA)
                                           , strategy_tol = ifelse(input$light.opt.tol == "by strategy"
                                                                   , input$light.strategy_tol
                                                                   , NA)
                              ))
    if (input$light.opt.tol == "user-defined")
    {
      combi = expand.grid(lifeStage = c("Ge", "Im", "Ma"), resources = c("L", "M", "H"))
      mat.tol = foreach(ls = combi$lifeStage, re = combi$resources, .combine = "rbind") %do%
        {
          eval(parse(text = paste0("tol = as.numeric(input$light.", ls, ".", re, ".tol) / 10")))
          return(data.frame(PFG = input$light.PFG
                            , lifeStage = c("Ge" = "Germinant", "Im" = "Immature", "Ma" = "Mature")[ls]
                            , resources = c("L" = "Low", "M" = "Medium", "H" = "High")[re]
                            , tolerance = tol))
        }
      RV$mat.PFG.light.tol <- rbind(RV$mat.PFG.light.tol, mat.tol)
    }
    RV$compt.light.options = c(input$light.opt.ag == "by type"
                              , input$light.opt.ag == "by strategy"
                              , input$light.opt.ag == "user-defined"
                              , input$light.opt.tol == "by type & light"
                              , input$light.opt.tol == "by strategy"
                              , input$light.opt.ol == "user-defined")
  }
})

observeEvent(input$delete.PFG.light, {
  RV$mat.PFG.light <- data.frame()
  RV$compt.light.options <- rep(FALSE, 6)
})

observeEvent(RV$mat.PFG.light, {
  if (nrow(RV$mat.PFG.light) > 0)
  {
    shinyjs::enable("create.light")
  } else
  {
    shinyjs::disable("create.light")
  }
})


####################################################################

observeEvent(input$create.light, {
  if (input$create.skeleton > 0)
  {
    col.light = c("PFG", switch (input$light.opt.ag
                                 , "by type" = "type"
                                 , "by strategy" = "strategy_ag"
                                 , "user-defined" = c("active_germ_low"
                                                      , "active_germ_medium"
                                                      , "active_germ_high")))
    if (input$light.opt.tol == "by type & light")
    {
      col.light = c(col.light, c("type", "light_need"))
      mat.tol = NULL
    } else if (input$light.opt.tol == "by strategy")
    {
      mat.tol = RV$mat.PFG.light[, c("PFG", "strategy_tol")]
    } else if (input$light.opt.tol == "user-defined")
    {
      mat.tol = RV$mat.PFG.light.tol
    }
    
    get_res = print_messages(as.expression(
      PRE_FATE.params_PFGlight(name.simulation = input$name.simul
                               , mat.PFG.light = RV$mat.PFG.light[, unique(col.light)]
                               , mat.PFG.tol = mat.tol
                               , opt.folder.name = get_opt.folder.name()
      )
    ), cut_pattern = paste0(input$name.simul, "/DATA/PFGS/LIGHT/"))
    
  } else
  {
    shinyalert(type = "warning", text = "You must create a simulation folder first !")
  }
})


####################################################################

get_tab.light = eventReactive(paste(input$name.simul
                                    , input$create.light
                                    , RV$compt.light.no), {
                                      if (!is.null(input$name.simul) && nchar(input$name.simul) > 0)
                                      {
                                        path_folder = paste0(input$name.simul, "/DATA/PFGS/LIGHT/")
                                        tab = get_files(path_folder, skip.no = 2, opt.sub_folder = TRUE)
                                        
                                        if (!is.null(tab) && ncol(tab) > 0)
                                        {
                                          RV$compt.light.no = ncol(tab)
                                          RV$compt.light.files = colnames(tab)
                                          return(tab)
                                        }
                                      }
                                    })

output$UI.files.light = renderUI({
  tab = get_tab.light()
  tab = as.data.frame(tab)
  
  if (!is.null(tab) && ncol(tab) > 0)
  {
    tagList(
      fluidRow(
        column(4
               , checkboxInput(inputId = "check.light.all"
                               , label = HTML("<em>Select all</em>")
                               , value = TRUE
                               , width = "100%"))
        , column(3
                 , actionButton(inputId = "view.light.select"
                                , label = "View selected"
                                , icon = icon("eye")
                                , width = "100%"
                                , style = button.style.action))
        , column(3
                 , actionButton(inputId = "delete.light.select"
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
                          checkboxInput(inputId = paste0("check.light.", colnames(tab)[i])
                                        , label = gsub("__", "/", colnames(tab)[i])
                                        , value = TRUE
                                        , width = "100%")
                        })
        )
        # , column(2
        #          , lapply(1:ncol(tab)
        #                   , function(i) {
        #                     actionButton(inputId = paste0("upload.light.", colnames(tab)[i])
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

# observeEvent(RV$compt.light.no, {
#   for (i in 1:RV$compt.light.no)
#   {
#     observeEvent(input[[paste0("upload.light.", RV$compt.light.files[i])]], {
#       get_update.light(file.lightParam = paste0(input$name.simul
#                                                   , "/DATA/PFGS/LIGHT/"
#                                                   , RV$compt.light.files[i]))
#     })
#   }
# })


observeEvent(input$check.light.all, {
  for (col_tab in RV$compt.light.files)
  {
    updateCheckboxInput(session
                        , inputId = paste0("check.light.", col_tab)
                        , value = input$check.light.all)
  }
})

observeEvent(input$view.light.select, {
  output$created_table.light = renderDataTable({
    req(grep(pattern = "check.light.", x = names(input), value = TRUE))
    
    tab = get_tab.light()
    tab = as.data.frame(tab)
    
    if (!is.null(tab) && ncol(tab) > 0)
    {
      if (input$check.light.all)
      {
        col_toKeep = rep(TRUE, ncol(tab))
      } else
      {
        col_toKeep = foreach(i = 1:ncol(tab), .combine = "c") %do%
        {
          eval(parse(text = paste0("res = input$check.light.", colnames(tab)[i])))
          return(res)
        }
      }
      return(tab[, which(col_toKeep == TRUE), drop = FALSE])
    }
  })
})

observeEvent(input$delete.light.select, {
  if (input$check.light.all)
  {
    col_toKeep = rep(TRUE,RV$compt.light.no)
  } else
  {
    col_toKeep = foreach(i = 1:RV$compt.light.no, .combine = "c") %do%
    {
      eval(parse(text = paste0("res = input$check.light.", RV$compt.light.files[i])))
      return(res)
    }
  }
  
  if (sum(col_toKeep) > 0)
  {
    file.lightParam = RV$compt.light.files[col_toKeep]
    shinyalert(type = "warning"
               , text = paste0("The simulation parameter file(s) "
                               , paste0(input$name.simul, "/DATA/PFGS/LIGHT/ \n")
                               , paste0(gsub("__", "/", file.saveParam), collapse = " , ")
                               , "\n will be removed !\n"
                               , "Make sure this is what you want.")
               , showCancelButton = TRUE
               , showConfirmButton = TRUE
               , callbackR = function(x)
               {
                 if (x)
                 {
                   for (fi in file.lightParam) 
                   {
                     file.remove(paste0(input$name.simul, "/DATA/PFGS/LIGHT/", gsub("__", "/", fi)))
                     if (nchar(dirname(gsub("__", "/", fi))) > 0)
                     {
                       sub_dir = paste0(input$name.simul, "/DATA/PFGS/LIGHT/", dirname(gsub("__", "/", fi)))
                       if (dir.exists(sub_dir) && length(list.files(path = sub_dir)) == 0)
                       {
                         unlink(sub_dir, recursive = TRUE)
                       }
                     }
                     removeUI(selector = paste0("check.light.", file.lightParam)
                              , multiple = FALSE
                              , immediate = TRUE)
                     removeUI(selector = paste0("upload.light.", file.lightParam)
                              , multiple = FALSE
                              , immediate = TRUE)
                   }
                   RV$compt.light.no = min(0, RV$compt.light.no - sum(col_toKeep))
                 }
               })
  }
})


