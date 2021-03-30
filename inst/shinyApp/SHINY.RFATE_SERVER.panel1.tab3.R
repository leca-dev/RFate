
####################################################################

output$UI.species.distance = renderUI({
  if (input$choice.distance == "from file")
  {
    fileInput(inputId = "species.distance"
              , label = NULL
              , buttonLabel = param.style("species.distance")
              , multiple = TRUE
              , width = "100%")
  }
})

####################################################################

output$UI.no.clusters = renderUI({
  sp.clust = get_CLUST1()
  if (!is.null(sp.clust) && length(grep("shinyalert", sp.clust)) == 0)
  {
    if (length(sp.clust$clust.dendrograms) == 1) {
      group_names = "ALL"
      maxi = max(sp.clust$clust.evaluation$no.clusters)
    } else {
      group_names = names(sp.clust$clust.dendrograms)
      maxi = sapply(group_names, function(i) { 
        max(sp.clust$clust.evaluation$no.clusters[which(sp.clust$clust.evaluation$GROUP == i)])
      })
    }
    names(maxi) = group_names
    lapply(group_names, function(i) {
      fluidRow(
        column(3, param.style(paste0("no.clust_", i)))
        , column(9
                 , sliderInput(inputId = paste0("no.clust_", i)
                               , label = NULL
                               , min = 2
                               , max = maxi[i] #max(sp.clust$clust.evaluation$no.clusters[which(sp.clust$clust.evaluation$GROUP == i)])
                               , value = 2
                               , step = 1
                               , round = TRUE
                               , width = "100%"
                 )
        )
      )
    })
  }
})

####################################################################

get_dist = eventReactive(list(input$choice.distance, input$clustering.step1), {
  end_filename = ""
  if (input$choice.distance == "from file")
  {
    if (!is.null(input$species.distance) && is.data.frame(input$species.distance))
    {
      if (sum(sapply(input$species.distance$name, extension) %in% c(".txt", ".csv")) == nrow(input$species.distance))
      {
        end_filename = input$species.distance$datapath
        names(end_filename) = input$species.distance$name
      } else
      {
        if (length(which(RV$pfg.graph == "clust1")) > 0)
        {
          RV$pfg.graph <- RV$pfg.graph[-which(RV$pfg.graph == "clust1")]
        }
        shinyjs::disable("clustering.step2")
        shinyjs::disable("clustering.step3")
        shinyalert(type = "warning", text = "You must provide a text file (.txt or .csv) for the species.distance !")
      }
    } else
    {
      if (length(which(RV$pfg.graph == "clust1")) > 0)
      {
        RV$pfg.graph <- RV$pfg.graph[-which(RV$pfg.graph == "clust1")]
      }
      shinyjs::disable("clustering.step2")
      shinyjs::disable("clustering.step3")
      shinyalert(type = "warning", text = "You must provide a text file (.txt or .csv) for the species.distance !")
    }
  } else
  {
    end_filename = list.files(pattern = "^PRE_FATE_DOMINANT_speciesDistance")
    names(end_filename) = end_filename
  }
  if (end_filename != "" && length(end_filename) > 0)
  {
    sp.dist = foreach(fi = end_filename) %do%
      {
        sp.di = fread(fi, header = TRUE, drop = 1)
        sp.di = as.data.frame(sp.di, stringsAsFactors = FALSE)
        if (ncol(sp.di) == 0 || nrow(sp.di) == 0 || ncol(sp.di) != nrow(sp.di))
        {
          if (length(which(RV$pfg.graph == "clust1")) > 0)
          {
            RV$pfg.graph <- RV$pfg.graph[-which(RV$pfg.graph == "clust1")]
          }
          shinyjs::disable("clustering.step2")
          shinyjs::disable("clustering.step3")
          shinyalert(type = "warning", text = paste0("The file n°", which(end_filename == fi), " contains ", ncol(sp.di)
                                                     , " columns and "
                                                     , nrow(sp.di), " lines. It should be a square matrix (pairwise distances)."))
          return(NULL)
          
        } else
        {
          sp.di = as.dist(sp.di)
          return(sp.di)
        }
      }
    if (sum(sapply(sp.dist, is.null)) == 0)
    {
      nam = sub("^PRE_FATE_DOMINANT_speciesDistance", "", sub(".csv$", "", names(end_filename)))
      if (length(nam) == 1 && nchar(nam) == 0) nam = "ALL"
      names(sp.dist) = nam
      return(sp.dist)
    }
  } else
  {
    if (length(which(RV$pfg.graph == "clust1")) > 0)
    {
      RV$pfg.graph <- RV$pfg.graph[-which(RV$pfg.graph == "clust1")]
    }
    shinyjs::disable("clustering.step2")
    shinyjs::disable("clustering.step3")
    shinyalert(type = "warning", text = "You must run the selection of dominant species or provide a text file (.txt or .csv) for species.distance !")
    return(NULL)
  }
})

####################################################################

observeEvent(input$clustering.step1, {
  RV$pfg.graph <- c(RV$pfg.graph, "clust1")
})

get_CLUST1 = eventReactive(input$clustering.step1, {
  
  ## GET species distance
  sp.dist = get_dist()
  if (!is.null(sp.dist) && length(grep("shinyalert", sp.dist)) == 0)
  {
    showModal(modalDialog(HTML(paste0("Create hierarchical clusters (dendrograms) from dissimilarity matrix ..."))
                          , title = "PFG clustering : step 1"
                          , footer = NULL))
    Sys.sleep(3)
    get_res = print_messages(as.expression(
      PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.dist)
    ))
    removeModal()
    
    shinyjs::enable("clustering.step2")
    return(get_res)
  }
})

####################################################################

observeEvent(input$clustering.step2, {
  RV$pfg.graph <- c(RV$pfg.graph, "clust2")
})

get_CLUST2 = eventReactive(input$clustering.step2, {
  
  ## GET species distance
  sp.dist = get_dist()
  if (!is.null(sp.dist) && length(grep("shinyalert", sp.dist)) == 0)
  {
    ## GET species clusters
    sp.clust = get_CLUST1()
    if (!is.null(sp.clust) && length(grep("shinyalert", sp.clust)) == 0)
    {
      no.clusters = foreach(i = names(sp.clust$clust.dendrograms), .combine = "c") %do%
        {
          input[[paste0("no.clust_", i)]]
        }
      
      showModal(modalDialog(HTML(paste0("Choose clusters and select determinant species with : <ul>"
                                        , "<li><strong>no.clusters :</strong> ", paste(unlist(no.clusters), collapse = " ")
                                        ,"</li>"
                                        , "</ul>"))
                            , title = "PFG clustering : step 2"
                            , footer = NULL))
      Sys.sleep(3)
      get_res = print_messages(as.expression(
        PRE_FATE.speciesClustering_step2(clust.dendrograms = sp.clust$clust.dendrograms
                                         , no.clusters = no.clusters
                                         , mat.species.DIST = sp.dist)
      ))
      removeModal()
      
      shinyjs::enable("clustering.step3")
      return(get_res)
    }
  }
})

####################################################################

get_sp.pfg.traits = eventReactive(input$clustering.step3, {
  
  ## GET species traits
  sp.traits = get_traits()
  if (!is.null(sp.traits) && length(grep("shinyalert", sp.traits)) == 0)
  {
    ## GET species determ
    sp.determ = get_CLUST2()
    if (!is.null(sp.determ) && length(grep("shinyalert", sp.determ)) == 0)
    {
      sp.traits = as.data.frame(sp.traits)
      sp.traits$species = as.character(sp.traits$species)
      
      sp.determ = as.data.frame(sp.determ$determ.all)
      sp.determ$species = as.character(sp.determ$species)
      
      sp.pfg.traits = merge(sp.traits, sp.determ[, c("species", "PFG")], by = "species")
      if ("GROUP" %in% colnames(sp.pfg.traits)){
        sp.pfg.traits = sp.pfg.traits[, -which(colnames(sp.pfg.traits) == "GROUP"), drop = FALSE]
      }
      
      return(sp.pfg.traits)
    }
  }
})

observeEvent(input$clustering.step3, {
  RV$pfg.graph <- c(RV$pfg.graph, "clust3")
})

get_CLUST3 = eventReactive(input$clustering.step3, {

  ## GET species-PFG traits
  sp.pfg.traits = get_sp.pfg.traits()
  if (!is.null(sp.pfg.traits) && length(grep("shinyalert", sp.pfg.traits)) == 0)
  {
    showModal(modalDialog(HTML(paste0("Compute PFG traits values based on determinant species..."))
                          , title = "PFG clustering : step 3"
                          , footer = NULL))
    Sys.sleep(3)
    get_res = print_messages(as.expression(
      PRE_FATE.speciesClustering_step3(mat.traits = sp.pfg.traits)
    ))
    removeModal()
    
    shinyjs::show("table.traits.pfg")
    output$table.traits.pfg = renderDataTable({
      get_res$tab.PFG.traits
    })
    
    return(get_res)
  }
})


