
####################################################################

output$UI.doRuleAC = renderUI({
  if (input$doRuleA || input$doRuleC)
  {
    tagList(
      numericInput(inputId = "rule.A1"
                   , label = param.style("rule.A1")
                   , min = 0
                   , value = 10
                   , step = 1
                   , width = "100%")
      , sliderInput(inputId = "rule.A2_quantile"
                    , label = param.style("rule.A2_quantile")
                    , min = 0
                    , max = 1
                    , value = 0.9
                    , step = 0.05
                    , width = "100%")
    )
  }
})


output$UI.doRuleB = renderUI({
  if (input$doRuleB)
  {
    tagList(
      numericInput(inputId = "rule.B1_number"
                   , label = param.style("rule.B1_number")
                   , min = 0
                   , value = 5
                   , step = 1
                   , width = "100%")
      , sliderInput(inputId = "rule.B1_percentage"
                    , label = param.style("rule.B1_percentage")
                    , min = 0
                    , max = 1
                    , value = 0.25
                    , step = 0.05
                    , width = "100%")
      , sliderInput(inputId = "rule.B2"
                    , label = param.style("rule.B2")
                    , min = 0
                    , max = 1
                    , value = 0.5
                    , step = 0.05
                    , width = "100%")
    )
  }
})

output$UI.doRobustness = renderUI({
  if (input$doRobustness)
  {
    tagList(
      numericInput(inputId = "robustness_rep"
                   , label = param.style("robustness_rep")
                   , min = 0
                   , value = 10
                   , step = 1
                   , width = "100%")
      , selectInput(inputId = "robustness_percent"
                    , label = param.style("robustness_percent")
                    , choices = seq(0.1, 0.9, 0.1)
                    , selected = seq(0.1, 0.9, 0.1)
                    , multiple = TRUE
                    , width = "100%")
    )
  }
})

####################################################################

get_obs = eventReactive(list(input$species.observations, input$select.dominant), {
  if (is.data.frame(input$species.observations))
  {
    if (extension(input$species.observations$name) %in% c(".txt", ".csv"))
    {
      sp.obs = fread(input$species.observations$datapath)
	  sp.obs = as.data.frame(sp.obs, stringsAsFactors = FALSE)
      
      if (!is.null(sp.obs))
      {
        shinyjs::show("table.observations")
        output$table.observations = renderDataTable({
          sp.obs
        })
        return(sp.obs)
      } else
      {
        shinyalert(type = "warning", text = "The species.observations is not correct !")
        return(NULL)
      }
    } else
    {
      shinyalert(type = "warning", text = "You must provide a text file (.txt or .csv) for the species.observations !")
      return(NULL)
    }
  } else
  {
    shinyalert(type = "warning", text = "You must provide a text file for (.txt or .csv) the species.observations !")
    return(NULL)
  }
})

####################################################################

output$table.observations = renderDataTable({
  sp.obs = get_obs()
  if (!is.null(sp.obs))
  {
    return(sp.obs)
  }
})

####################################################################

observeEvent(input$select.dominant, {
  RV$pfg.graph <- c(RV$pfg.graph, "dom")
})

get_DOM = eventReactive(input$select.dominant, {

  sp.obs = get_obs()
  if (!is.null(sp.obs))
  {
    rule.A1 = rule.A2_quantile = NULL
    if (!is.null(input$rule.A1)){
      rule.A1 = as.numeric(input$rule.A1)
    }
    if (!is.null(input$rule.A2_quantile)){
      rule.A2_quantile = as.numeric(input$rule.A2_quantile)
    }
    rule.B1_percentage = rule.B1_number = rule.B2 = NULL
    if (!is.null(input$rule.B1_percentage)){
      rule.B1_percentage = as.numeric(input$rule.B1_percentage)
    }
    if (!is.null(input$rule.B1_number)){
      rule.B1_number = as.numeric(input$rule.B1_number)
    }
    if (!is.null(input$rule.B2)){
      rule.B2 = as.numeric(input$rule.B2)
    }
    robustness_percent = robustness_rep = NULL
    if (!is.null(input$robustness_percent)){
      robustness_percent = as.numeric(input$robustness_percent)
    }
    if (!is.null(input$robustness_rep)){
      robustness_rep = as.numeric(input$robustness_rep)
    }
    
    showModal(modalDialog(HTML(paste0("Select dominant species with parameters : <ul>"
                                      , ifelse(input$doRuleA || input$doRuleC
                                               , paste0("<li><strong>rule.A1 :</strong> ", rule.A1, "</li>"
                                                        , "<li><strong>rule.A2_quantile :</strong> ", rule.A2_quantile, "</li>")
                                               , "")
                                      , ifelse(input$doRuleB
                                               , paste0("<li><strong>rule.B1_percentage :</strong> ", rule.B1_percentage, "</li>"
                                                        , "<li><strong>rule.B1_number :</strong> ", rule.B1_number, "</li>"
                                                        , "<li><strong>rule.B2 :</strong> ", rule.B2, "</li>")
                                               , "")
                                      , ifelse(input$doRobustness
                                               , paste0("<li><strong>robustness_percent :</strong> "
                                                        , paste0(robustness_percent, collapse = ", "), "</li>"
                                                        , "<li><strong>robustness_rep :</strong> ", robustness_rep, "</li>")
                                               , "")
                                      , "</ul>"))
                          , title = "Selection of dominant species from abundance releves"
                          , footer = NULL))
    Sys.sleep(3)
    get_res = print_messages(as.expression(
      PRE_FATE.selectDominant(mat.observations = sp.obs
                              , doRuleA = input$doRuleA
                              , rule.A1 = rule.A1
                              , rule.A2_quantile = rule.A2_quantile
                              , doRuleB = input$doRuleB
                              , rule.B1_percentage = rule.B1_percentage
                              , rule.B1_number = rule.B1_number
                              , rule.B2 = rule.B2
                              , doRuleC = input$doRuleC
                              , opt.doRobustness = input$doRobustness
                              , opt.robustness_percent = robustness_percent
                              , opt.robustness_rep = robustness_rep
      )
    ))
    removeModal()
    
    return(get_res)
  }
})
