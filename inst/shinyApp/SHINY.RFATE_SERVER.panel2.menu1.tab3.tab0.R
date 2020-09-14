
####################################################################

get_opt.folder.name = eventReactive(input$PFG.folder, {
  opt.folder.name = ifelse(nchar(input$PFG.folder) > 0
                           , gsub(" ", "_", input$PFG.folder)
                           , "")
  if (length(grep(" ", input$PFG.folder)) > 0)
  {
    showNotification("Spaces within opt.folder.name have been replaced by `_` !", type = "warning")
  }
  return(opt.folder.name)
})

####################################################################

output$names.PFG = renderText({
  HTML(paste0("<strong>PFG list :</strong> ", paste0(RV$names.PFG, collapse = " ; ")))
})

####################################################################

observeEvent(RV$names.PFG, {
  if (length(RV$names.PFG) > 0)
  {
    shinyjs::enable("succ.PFG")
    shinyjs::enable("add.PFG.succ")
    shinyjs::enable("light.PFG")
    shinyjs::enable("add.PFG.light")
    shinyjs::enable("soil.PFG")
    shinyjs::enable("add.PFG.soil")
    shinyjs::enable("disp.PFG")
    shinyjs::enable("add.PFG.disp")
  } else
  {
    shinyjs::disable("succ.PFG")
    shinyjs::disable("add.PFG.succ")
    shinyjs::disable("light.PFG")
    shinyjs::disable("add.PFG.light")
    shinyjs::disable("soil.PFG")
    shinyjs::disable("add.PFG.soil")
    shinyjs::disable("disp.PFG")
    shinyjs::disable("add.PFG.disp")
  }
})

observeEvent(input$name.PFG, {
  if (nchar(input$name.PFG) > 0)
  {
    shinyjs::enable("add.PFG.name")
  } else
  {
    shinyjs::disable("add.PFG.name")
  }
})

observeEvent(input$add.PFG.name, {
  if (input$name.PFG %in% RV$names.PFG)
  {
    shinyalert(type = "warning", text = "You must give different PFG names !")
    shinyjs::reset("name.PFG")
  } else
  {
    RV$names.PFG = c(RV$names.PFG, input$name.PFG)
    shinyjs::reset("name.PFG")
  }
})

observeEvent(input$delete.names.PFG, {
  RV$names.PFG = vector()
  
  shinyjs::reset("name.PFG")
  shinyjs::reset("succ.PFG")
  shinyjs::reset("light.PFG")
  shinyjs::reset("soil.PFG")
  shinyjs::reset("disp.PFG")
})

