
####################################################################

output$UI.species.selected = renderUI({
  if (input$choice.dominant == "from file")
  {
    fileInput(inputId = "species.selected"
              , label = NULL
              , buttonLabel = param.style("species.selected")
              , multiple = FALSE
              , width = "100%")
  }
})

####################################################################

get_traits = eventReactive(list(input$species.traits, input$compute.distance), {
  if (!is.null(input$species.traits) && is.data.frame(input$species.traits))
  {
    if (extension(input$species.traits$name) %in% c(".txt", ".csv"))
    {
      sp.traits = fread(input$species.traits$datapath)
	  sp.traits = as.data.frame(sp.traits, stringsAsFactors = FALSE)
      
      if (length(which(colnames(sp.traits) == "species")) == 1)
      {
        shinyjs::show("table.traits.sp")
        output$table.traits.sp = renderDataTable({
          sp.traits
        })
        return(sp.traits)
      } else
      {
        shinyalert(type = "warning", text = "The species.traits must contain a column named 'species' !")
        return(NULL)
      }
    } else
    {
      shinyalert(type = "warning", text = "You must provide a text file for the species.traits !")
    }
  } else
  {
    shinyalert(type = "warning", text = "You must provide a text file for the species.traits !")
  }
})

get_dom_param = eventReactive(list(input$doRuleA
                                   , input$rule.A1
                                   , input$rule.A2_quantile
                                   , input$doRuleB
                                   , input$rule.B1_number
                                   , input$rule.B1_percentage
                                   , input$rule.B2
                                   , input$doRuleC
), {
  end_filename = end_filenameA = end_filenameB = end_filenameC = ""
  if (input$doRuleA) {
    end_filenameA = paste0("_A_", input$rule.A1
                           , "_", input$rule.A2_quantile)
  }
  if (input$doRuleB) {
    end_filenameB = paste0("_B_", input$rule.B1_number
                           , "_", input$rule.B1_percentage
                           , "_", input$rule.B2)
  }
  if (input$doRuleC) {
    end_filenameC = paste0("_C_", input$rule.A1
                           , "_", input$rule.A2_quantile)
  }
  end_filename = paste0(end_filenameA, end_filenameB, end_filenameC)
  return(end_filename)
})

get_dom = eventReactive(list(input$choice.dominant, input$compute.distance), {
  end_filename = ""
  if (input$choice.dominant == "from file")
  {
    if (!is.null(input$species.selected) && is.data.frame(input$species.selected))
    {
      if (extension(input$species.selected$name) %in% c(".txt", ".csv"))
      {
        end_filename = input$species.selected$datapath
      } else
      {
        shinyalert(type = "warning", text = "You must provide a text file (.txt or .csv) for the species.selected !")
      }
    } else
    {
      shinyalert(type = "warning", text = "You must provide a text file (.txt or .csv) for the species.selected !")
    }
  } else
  {
    end_filename = paste0("PRE_FATE_DOMINANT_TABLE_species"
                          , get_dom_param()
                          , ".csv")
  }
  if (file.exists(end_filename))
  {
    sp.select = fread(end_filename)
	
    if (ncol(sp.select) > 1)
    {
      warning(paste0("The file ", input$species.selected$name, " contains ", ncol(sp.select)
                     , " columns. Only the first, which should contain dominant species, will be used."))
    }
    
    sp.dom = as.data.frame(sp.select, stringsAsFactors = FALSE)[, 1]
    return(sp.dom)
  } else
  {
    shinyalert(type = "warning", text = "You must run the selection of dominant species or provide a text file (.txt or .csv) for species.selected !")
    return(NULL)
  }
})

####################################################################

observeEvent(input$compute.distance, {
  RV$pfg.graph <- c(RV$pfg.graph, "dist")
})

get_DIST = eventReactive(input$compute.distance, {
  
  ## GET species traits
  sp.traits = get_traits()
  if (!is.null(sp.traits))
  {
    ## GET selected dominant species
    sp.dom = get_dom()
    if (!is.null(sp.dom))
    {
      if (length(which(sp.traits$species %in% sp.dom)) > 0)
      {
        sp.traits = sp.traits[which(sp.traits$species %in% sp.dom), ]
        
        ## GET species niche distance
        if (!is.null(input$species.niche.distance) && is.data.frame(input$species.niche.distance))
        {
          if (extension(input$species.niche.distance$name) %in% c(".txt", ".csv"))
          {
            sp.niche = fread(input$species.niche.distance$datapath)
			      sp.niche = as.data.frame(sp.niche, stringsAsFactors = FALSE)
          } else if (extension(input$species.niche.distance$name) == ".RData")
          {
            sp.niche = get(load(input$species.niche.distance$datapath))
          } else
          {
            shinyalert(type = "warning", text = "You must provide a text or a RData file for the species.niche.distance !")
          }
          
          if (length(which(colnames(sp.niche) %in% sp.dom)) > 0)
          {
            showModal(modalDialog(HTML(paste0("Compute distances between species based on traits and niche overlap, with parameters : <ul>"
                                              , "<li><strong>opt.maxPercent.NA :</strong> ", as.numeric(input$opt.maxPercent.NA), "</li>"
                                              , "<li><strong>opt.maxPercent.similarSpecies :</strong> ", as.numeric(input$opt.maxPercent.similarSpecies), "</li>"
                                              , "<li><strong>opt.min.sd :</strong> ", as.numeric(input$opt.min.sd), "</li>"
                                              , "</ul>"))
                                  , title = "Distance matrix between selected species"
                                  , footer = NULL))
            Sys.sleep(3)
            get_res = print_messages(as.expression(
                PRE_FATE.speciesDistance(mat.traits = sp.traits
                                         , mat.overlap = sp.niche
                                         , opt.maxPercent.NA = as.numeric(input$opt.maxPercent.NA)
                                         , opt.maxPercent.similarSpecies = as.numeric(input$opt.maxPercent.similarSpecies)
                                         , opt.min.sd = as.numeric(input$opt.min.sd)
                )
              ))
            removeModal()
            
            return(get_res)
          } else
          {
            shinyalert(type = "warning", text = "Species names between species.niche.distance and dominant species do not match !")
          }
        } else
        {
          shinyalert(type = "warning", text = "You must provide a text or a RData file for the species.niche.distance !")
        }
      } else
      {
        shinyalert(type = "warning", text = "Species names between species.traits and dominant species do not match !")
      }
    }
  }
})
