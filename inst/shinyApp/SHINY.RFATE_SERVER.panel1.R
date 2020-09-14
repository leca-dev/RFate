
####################################################################

observeEvent(input$HELP.panel1, {
  introjs(session = session
          , options = list("nextLabel" = "Next"
                           , "prevLabel" = "Prev"
                           , "skipLabel" = "Close"
                           , steps = data.frame(element = c("#pfg.panel1", paste0("#help1_", 2:3), "#pfg.panel2")
                                                , intro = c("Functional group building is made in 3 steps :
                                                            <ul>
                                                            <li>the selection of dominant species within the studied area
                                                            </li>
                                                            <li>the computation of functional distance (traits, climatic niche...) between each pair of selected species
                                                            </li>
                                                            <li>the division into clusters, and the calculation of traits for each group obtained
                                                            </li>
                                                            </ul>
                                                            "
                                                            , "Species observations must be given by sites. 
                                                            Colnames must be <em>species</em>, <em>sites</em> and <em>abund</em> (presence/absence data are allowed). 
                                                            An optional column <em>habitat</em> can be given if selection of dominant is to be done also by type of habitat."
                                                            , "Traits must be given for as many species (dominant) as possible.
                                                            Grouping information can be given as well to perform the clustering for different species assemblages (e.g. life form)."
                                                            , "Intermediate graphics allow the user to adjust the parameters at each step."))
          )
  )
})

####################################################################


observeEvent(RV$pfg.graph, {

  levels_graph.type = unique(RV$pfg.graph)
  levels_graph.type = factor(levels_graph.type, c("dom", "dist", "clust1", "clust2", "clust3"))
  levels_graph.type = sort(levels_graph.type)
  pp = foreach(graph.type = levels_graph.type, .combine ="c") %do%
    {
      graph.type = as.character(graph.type)
      tab = switch(graph.type
                   , dom = { get_DOM() }
                   , dist = { get_DIST() }
                   , clust1 = { get_CLUST1() }
                   , clust2 = { get_CLUST2() }
                   , clust3 = { get_CLUST3() }
      )
      
      if (!is.null(tab) && length(tab) > 1)
      {
        colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
        
        pp = switch(graph.type
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , dom = {
                      pp = foreach(x = names(tab)) %do%
                        {
                          if (length(grep("plot", x)) > 0)
                          {
                            if (x == "plot.pco" || x == "plot.B" || x == "plot.robustness")
                            {
                              pp_bis = foreach(y = names(tab[[x]])) %do% { return(tab[[x]][[y]]) }
                              names(pp_bis) = names(tab[[x]])
                            } else
                            {
                              pp_bis = list(tab[[x]])
                              names(pp_bis) = x
                            }
                            return(pp_bis)
                          }
                        }
                      return(unlist(pp, recursive = FALSE))
                      }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , dist = {
                      pp = foreach(x = names(tab)) %do%
                        {
                          hc = hclust(tab[[x]])
                          pp = ggdendrogram(hc, rotate = TRUE) +
                            labs(title = paste0("Hierarchical clustering based on species distance "
                                                , ifelse(length(names(tab)) > 1
                                                         , paste0("(group ", x, ")")
                                                         , "")))
                          return(pp)
                        }
                    }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , clust1 = { return(list(tab$plot.clustMethod, tab$plot.clustNo)) }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , clust2 = { return(c(list(tab$plot.distance), tab$plot.PCO)) }
                    ## ---------------------------------------------------------------------------------------------------------- ##
                    , clust3 = { return(tab$plot) }
        )
      }
    }
  
  if (is.ggplot(pp) || (is.list(pp) && length(pp) > 0))
  {
    shinyjs::show("pfg.go.left")
    shinyjs::show("pfg.go.right")
    shinyjs::disable("pfg.go.left")
    shinyjs::disable("pfg.go.right")
    if (length(pp) > 1)
    {
      shinyjs::enable("pfg.go.left")
    }
    
    output$UI.pfg.browser = renderUI({
      lapply(1:length(pp), function(i) {
        if (i == length(pp)){
          plotOutput(outputId = paste0("pfg.browser_", i), width = "100%", height = "600px")
        } else {
          shinyjs::hidden(plotOutput(outputId = paste0("pfg.browser_", i), width = "100%", height = "600px"))
        }
      })
    })
    lapply(1:length(pp), function(i) {
      output[[paste0("pfg.browser_", i)]] = renderPlot({ plot(pp[[i]]) })
    })

    RV$compt.browser.pfg.max <- length(pp)
    RV$compt.browser.pfg <- length(pp)
  }
})

####################################################################

observeEvent(input$pfg.go.left, {
  if (input$pfg.go.left > 0)
  {
    shinyjs::enable("pfg.go.right")
    RV$compt.browser.pfg <- RV$compt.browser.pfg - 1
    if (RV$compt.browser.pfg == 1)
    {
      shinyjs::disable("pfg.go.left")
    }
    shinyjs::toggle(paste0("pfg.browser_", RV$compt.browser.pfg + 1))
    shinyjs::toggle(paste0("pfg.browser_", RV$compt.browser.pfg))
  }
})

observeEvent(input$pfg.go.right, {
  if (input$pfg.go.right > 0)
  {
    shinyjs::enable("pfg.go.left")
    RV$compt.browser.pfg <- RV$compt.browser.pfg + 1
    if (RV$compt.browser.pfg == RV$compt.browser.pfg.max)
    {
      shinyjs::disable("pfg.go.right")
    }
    shinyjs::toggle(paste0("pfg.browser_", RV$compt.browser.pfg - 1))
    shinyjs::toggle(paste0("pfg.browser_", RV$compt.browser.pfg))
  }
})



