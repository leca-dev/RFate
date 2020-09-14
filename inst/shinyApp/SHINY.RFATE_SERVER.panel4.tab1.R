
####################################################################

get_names.files = eventReactive(input$graph.folder.simul, {
  names.files1 = list.files(path = paste0(get_path.simul(), "/RESULTS")
                            , pattern = ".csv$"
                            , all.files = FALSE
                            , full.names = TRUE)
  names.files1 = basename(names.files1)
  if (length(grep("evolution_abundance", names.files1)) > 0){
    names.files1 = names.files1[-grep("evolution_abundance", names.files1)]
  }
  if (length(grep("evolution_stability", names.files1)) > 0){
    names.files1 = sub("evolution_stability[12]", "evolution_stability", names.files1)
    names.files1 = unique(names.files1)
  }
  
  
  names.files2 = list.files(path = paste0(get_path.simul(), "/RESULTS")
                            , pattern = "map_PFG"
                            , all.files = FALSE
                            , full.names = TRUE)
  dir.files2 = sub(".*SIMUL_V", "SIMUL_V", names.files2)
  dir.files2 = sub(".pdf$", "", dir.files2)
  
  names.files3 = list.files(path = paste0(get_path.simul(), "/RESULTS/", dir.files2)
                            , pattern = "^PFGcover|^PFGrichness|^PFGlight|^PFGsoil"
                            , all.files = FALSE
                            , full.names = TRUE)
  names.files3 = paste0(basename(dirname(names.files3)), "/", basename(names.files3))
  
  names.files = c(names.files1, names.files3)
  names.files = sub(".csv$|.pdf$|.tif$", "", names.files)
  return(names.files)
})

update_browser.files = function()
{
  names.files = get_names.files()
  if (length(names.files) > 0)
  {
    ind.toSuppr = get_browser.evolution()
    ind.toSuppr = c(ind.toSuppr, get_browser.validation())
    ind.toSuppr = c(ind.toSuppr, get_browser.map())
    if (length(ind.toSuppr) > 0)
    {
      names.files = names.files[-ind.toSuppr]
    }
    
    updateSelectInput(session
                      , inputId = "browser.files"
                      , choices = names.files
                      , selected = names.files[1])
    shinyjs::enable("browser.files")
  } else
  {
    shinyjs::disable("browser.files")
  }
}

####################################################################

get_browser.evolution = eventReactive(input$browser.evolution, {
  if (!input$browser.evolution)
  {
    return(grep("evolution_spaceOccupancy|evolution_totalAbundance|evolution_pixels|evolution_stability", get_names.files()))
  } 
})

get_browser.validation = eventReactive(input$browser.validation, {
  if (!input$browser.validation)
  {
    return(grep("validation|PFGvsHS", get_names.files()))
  } 
})

get_browser.map = eventReactive(input$browser.map, {
  if (!input$browser.map)
  {
    return(grep("^PFGcover|^PFGrichness|^PFGlight|^PFGsoil", get_names.files()))
  } 
})


####################################################################

observeEvent(input$browser.evolution, { update_browser.files() })
observeEvent(input$browser.validation, { update_browser.files() })
observeEvent(input$browser.map, { update_browser.files() })

####################################################################

get_graph.type = eventReactive(input$browser.files, {
  if (nchar(input$browser.files) > 0)
  {
    if (length(grep("evolution_spaceOccupancy", input$browser.files)) > 0)
    {
      return("evolution_spaceOccupancy")
    } else if (length(grep("evolution_totalAbundance", input$browser.files)) > 0)
    {
      return("evolution_totalAbundance")
    } else if (length(grep("evolution_pixels", input$browser.files)) > 0)
    {
      return("evolution_pixels")
    } else if (length(grep("evolution_stability", input$browser.files)) > 0)
    {
      return("evolution_stability")
    } else if (length(grep("validation", input$browser.files)) > 0)
    {
      return("validation")
    } else if (length(grep("PFGrichness", input$browser.files)) > 0)
    {
      return("richness")
    } else if (length(grep("PFGcover", input$browser.files)) > 0)
    {
      return("cover")
    } else if (length(grep("PFGlight", input$browser.files)) > 0)
    {
      return("light")
    } else if (length(grep("PFGsoil", input$browser.files)) > 0)
    {
      return("soil")
    } else if (length(grep("PFGvsHS", input$browser.files)) > 0)
    {
      return("PFGvsHS")
    } else
    {
      return(NA)
    }
  }
})

observeEvent(input$browser.files, {
  if (nchar(input$browser.files) > 0)
  {
    graph.type = get_graph.type()
    if (!is.na(graph.type))
    {
      if (graph.type %in% c("richness", "cover", "light", "soil"))
      {
        graph.type.file = paste0(get_path.simul(), "/RESULTS/", input$browser.files, ".tif")
        if (file.exists(graph.type.file)){
          ras = raster(graph.type.file)
        } else {
          shinyalert(type = "warning", text = paste0("The file selected (", graph.type.file, ") does not exist !"))
        }
      } else
      {
        graph.type.file = paste0(get_path.simul(), "/RESULTS/", input$browser.files, ".csv")
        if (graph.type == "evolution_stability")
        {
          file1 = sub("evolution_stability_", "evolution_stability1_", graph.type.file)
          file2 = sub("evolution_stability_", "evolution_stability2_", graph.type.file)
          tab1 = tab2 = NULL
          if (file.exists(file1)){ tab1 = as.data.frame(fread(file1, stringsAsFactors = FALSE)) }
          if (file.exists(file2)){ tab2 = as.data.frame(fread(file2, stringsAsFactors = FALSE)) }
        } else
        {
          if (file.exists(graph.type.file)){
            tab = as.data.frame(fread(graph.type.file, stringsAsFactors = FALSE))
          } else {
            shinyalert(type = "warning", text = paste0("The file selected ("
                                                       , graph.type.file
                                                       , ") does not exist !"))
          }
        }
      }
      
      opt.fixedScale = FALSE
      
      ## Calculer ici le nombre de PFG, nombre de strates, nombre d'habitats ?
      ## MAUVAISE MAJ de la liste des fichiers disponibles...
      
      ## spaceOccupancy, totalAbundance
      col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
      col_fun = colorRampPalette(col_vec)
      
      ## PFG maps
      pp.i = function(tab, i.col, i.axis, i.title, i.subtitle)
      {
        pp.i = ggplot(tab, aes_string(x = "X", y = "Y", fill = "VALUE")) +
          scale_fill_gradientn(i.axis
                               , colors = brewer.pal(9, i.col)
                               , limits = c(0, 1)
                               , breaks = seq(0, 1, 0.2)
                               , labels = seq(0, 100, 20)) +
          coord_equal() +
          geom_raster() +
          labs(x = "", y = ""
               , title = i.title
               , subtitle = i.subtitle) +
          .getGraphics_theme() +
          theme(axis.text = element_blank()
                , legend.key.width = unit(2, "lines"))
        
        return(pp.i)
      }
      
      
      pp = switch(graph.type
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , evolution_spaceOccupancy = {
                    if (length(which(colnames(tab) %in% c("PFG", "HAB", "YEAR", "spaceOccupancy"))) == 4)
                    {
                      list(ggplot(tab, aes_string(x = "YEAR"
                                                  , y = "spaceOccupancy * 100"
                                                  , color = "factor(HAB)")) +
                             geom_line(lwd = 1) +
                             facet_wrap("~ PFG") +
                             scale_color_manual("Habitat", values = col_fun(length(unique(tab$HAB)))) +
                             labs(x = "", y = ""
                                  , title = paste0("GRAPH A : evolution of species' space occupation")
                                  , subtitle = paste0("For each PFG, the line represents the "
                                                      , "evolution through time of its space "
                                                      , "occupancy,\n meaning the percentage of "
                                                      , "pixels in which the abundance of the "
                                                      , "species is greater than 0.\n")) +
                             .getGraphics_theme())
                    } else
                    {
                      shinyalert(type = "warning", text = "The file provided does not contain the required columns (YEAR, Abund, HAB, PFG) !")
                    }
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , evolution_totalAbundance = {
                    if (length(which(colnames(tab) %in% c("PFG", "HAB", "YEAR", "totalAbundance"))) == 4)
                    {
                      list(ggplot(tab, aes_string(x = "YEAR"
                                                  , y = "totalAbundance"
                                                  , color = "HAB")) +
                             geom_line(lwd = 1) +
                             facet_wrap("~ PFG", scales = ifelse(opt.fixedScale, "fixed", "free_y")) +
                             scale_color_manual("Habitat", values = col_fun(length(unique(tab$HAB)))) +
                             labs(x = "", y = ""
                                  , title = paste0("GRAPH A : evolution of species' abundance")
                                  , subtitle = paste0("For each PFG, the line represents the "
                                                      , "evolution through time of its abundance\n"
                                                      , "over the whole studied area, meaning the "
                                                      , "sum of its abundances in every pixel.\n")) +
                             .getGraphics_theme())
                    } else
                    {
                      shinyalert(type = "warning", text = "The file provided does not contain the required columns (YEAR, Abund, HAB, PFG) !")
                    }
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , evolution_pixels = {
                    if (length(which(colnames(tab) %in% c("TYPE", "GROUP", "ID.pixel", "HAB", "YEAR", "value"))) == 6)
                    {
                      PFG = unique(tab$GROUP[which(tab$TYPE == "abundance")])
                      no_PFG = length(PFG)
                      
                      strata = 1:10
                      if (length(which(tab$TYPE == "light")) > 0)
                      {
                        strata = unique(tab$GROUP[which(tab$TYPE == "light")])
                      }
                      no_STRATA = length(strata)
                      
                      tab$YEAR = as.numeric(as.character(tab$YEAR))
                      tab$TYPE = factor(tab$TYPE, c("light", "abundance", "soil"))
                      tab$GROUP = factor(tab$GROUP, c(strata, PFG, "soil"))

                      vec_col1 = c('#0077BB', '#33BBEE', '#009988', '#EE7733', '#CC3311', '#EE3377')
                      val_col1 = c(rep(rgb(1,1,1,1), no_STRATA), colorRampPalette(vec_col1)(no_PFG), "grey30")
                      names(val_col1) = c(strata, PFG, "soil")

                      vec_col2 = c('#FEC44F', '#FB9A29', '#EC7014', '#CC4C02', '#993404', '#662506')
                      val_col2 = colorRampPalette(vec_col2)(no_STRATA)
                      names(val_col2) = strata
                      
                      
                      pp = ggplot(tab, aes_string(x = "YEAR", y = "value")) +
                        geom_line(data = tab[which(tab$TYPE == "soil"),]
                                  , color = "grey30"
                                  , lwd = 0.7) +
                        geom_line(data = tab[which(tab$TYPE == "abundance"),]
                                  , aes_string(color = "GROUP")
                                  , lwd = 0.7) +
                        scale_color_manual("", values = val_col1) +
                        geom_area(data = tab[which(tab$TYPE == "light"),]
                                  , aes_string(fill = "GROUP")
                                  , position = "identity", alpha= 0.4) +
                        scale_fill_manual("", values = val_col2) +
                        facet_grid("TYPE ~ ID.pixel"
                                   , scales = ifelse(opt.fixedScale, "fixed", "free_y")) +
                        labs(x = "", y = ""
                             , title = paste0("GRAPH A : evolution of species' abundance")
                             , subtitle = paste0("For each PFG, the line represents the "
                                                 , "evolution through time of its abundance\n"
                                                 , "(as well as the light and soil resources if available)"
                                                 , "for the selected pixels within the studied area.\n")) +
                        .getGraphics_theme()
                      list(pp)
                    } else
                    {
                      shinyalert(type = "warning", text = "The file provided does not contain the required columns (YEAR, Abund, HAB, PFG) !")
                    }
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , evolution_stability = {
                    if (length(which(colnames(tab1) %in% c("HAB", "year", "totalAbundance"
                                                           , "no.PFG", "evenness"))) == 5)
                    {
                      ## Evolution of abundance and evenness through time -------------------
                      tab.plot1 = suppressWarnings(melt(tab1, id.vars = c("HAB", "year")))
                      colnames(tab.plot1) = c("HAB", "year", "metric", "value")
                      tab.plot1$metric = factor(tab.plot1$metric, c("totalAbundance", "evenness", "no.PFG"))
                      
                      ## plot
                      pp = ggplot(tab.plot1, aes_string(x = "year", y = "value", color = "HAB"))
                      
                      if (!is.null(tab2))
                      {
                        ## Evolution of stability through time --------------------------------
                        tab.plot2 = tab2
                        tab.plot2$year_median = sapply(1:nrow(tab.plot2), function(x) {
                          median(as.numeric(as.character(tab.plot2[x, c("yearStart", "yearEnd")]))) })
                        tab.plot2 = tab.plot2[which(tab.plot2$sd > 0.00001), ]
                        
                        ## plot
                        pp = pp +
                          geom_rect(data = tab.plot2, inherit.aes = FALSE, alpha = 0.5
                                    , aes_string(xmin = "yearStart", xmax = "yearEnd"
                                                 , ymin = "mean - sd"
                                                 , ymax = "mean + sd"
                                                 , fill = "HAB"))
                      }
                      
                      ## plot
                      pp = pp +
                        geom_line(lwd = 1) +
                        geom_point() +
                        facet_grid("metric ~ .", scales = "free_y"
                                   , labeller = as_labeller(c("totalAbundance" = "Total abundance"
                                                              , "evenness" = "Evenness"
                                                              , "no.PFG" = "Number of PFG"))) +
                        scale_color_manual("Habitat", values  = col_fun(length(unique(tab1$HAB)))) +
                        scale_fill_manual(guide = FALSE, values  = col_fun(length(unique(tab1$HAB)))) +
                        labs(x = "", y = ""
                             , title = paste0("GRAPH A : evolution of habitat composition")) +
                        .getGraphics_theme()
                      list(pp)
                    } else
                    {
                      shinyalert(type = "warning", text = "The file provided does not contain the required columns (YEAR, Abund, HAB, PFG) !")
                    }
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , validation = {
                    if (length(which(colnames(tab) %in% c("PFG", "HAB", "AUC.sd", "sensitivity.sd"
                                                          , "specificity.sd", "sensitivity", "TSS"
                                                          , "specificity", "AUC"))) == 9)
                    {
                      tab = as.data.frame(tab[, c("PFG", "HAB", "AUC.sd", "sensitivity.sd"
                                                  , "specificity.sd", "sensitivity", "TSS"
                                                  , "specificity", "AUC")])
                      
                      ## prepare the plot ------------------------------------------------------------
                      tab = melt(tab, id.vars = c("PFG", "HAB", "AUC.sd", "sensitivity.sd", "specificity.sd"))
                      tab$variable = factor(tab$variable, c("sensitivity", "TSS", "specificity", "AUC"))
                      
                      tab$AUC.sd[which(tab$variable != "AUC")] = NA
                      tab$sensitivity.sd[which(tab$variable != "sensitivity")] = NA
                      tab$specificity.sd[which(tab$variable != "specificity")] = NA
                      
                      tab$hline = 0.5
                      tab$hline[which(tab$variable == "AUC")] = 0.8
                      tab$hline[which(tab$variable == "TSS")] = 0.4
                      
                      hab_names = sort(unique(tab$HAB))
                      
                      ## produce the plot ------------------------------------------------------------
                      pp = foreach(habi = hab_names) %do%
                        {
                          cat("\n > Preparing for habitat ", habi)
                          mat.plot = tab[which(tab$HAB == habi), ]
                          
                          ## 1. get the legend
                          pp = ggplot(mat.plot, aes_string(x = "PFG", y = "value", fill = "value")) +
                            scale_fill_gradientn(""
                                                 , colors = brewer.pal(9, "RdYlGn")
                                                 , breaks = seq(0, 1, 0.2)
                                                 , limits = c(0, 1)) +
                            geom_bar(stat = "identity", na.rm = TRUE) +
                            ylim(0, 1) +
                            .getGraphics_theme() +
                            theme(legend.key.width = unit(2, "lines"))
                          
                          pp_leg = suppressWarnings(get_legend(pp))
                          
                          ## 2. get one plot for the title and for each statistic
                          pp_list = foreach(vari = c("all", "sensitivity", "specificity", "TSS", "AUC")) %do%
                            {
                              if (vari == "all"){
                                pp = ggplot(mat.plot, aes_string(x = "PFG"
                                                                 , y = "value"
                                                                 , fill = "value")) +
                                  labs(x = "", y = ""
                                       , title = paste0("GRAPH B : validation statistics"
                                                        , " - Simulation year : "
                                                        , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1]
                                                        , " - Habitat ", habi)
                                       , subtitle = paste0("Sensitivity (or specificity) measures "
                                                           , "the proportion of actual positives "
                                                           , "(or negatives) that are correctly "
                                                           , "identified as such.\n"
                                                           , "True skill statistic (TSS) values "
                                                           , "of -1 indicate predictive abilities "
                                                           , "of not better than a random model,\n"
                                                           , "0 indicates an indiscriminate model "
                                                           , "and +1 a perfect model.\n"
                                                           , "AUC corresponds to the area under "
                                                           , "the ROC curve (Receiver Operating "
                                                           , "Characteristic).\n")) +
                                  .getGraphics_theme() +
                                  theme(panel.grid = element_blank()
                                        , axis.text = element_blank())
                              } else
                              {
                                if (vari == "sensitivity") subti = "Sensitivity - True positive rate"
                                if (vari == "specificity") subti = "Specificity - True negative rate"
                                if (vari == "TSS") subti = "True Skill Statistic (TSS)"
                                if (vari == "AUC") subti = "Area Under Curve (AUC)"
                                pp = ggplot(mat.plot[which(mat.plot$variable == vari), ]
                                            , aes_string(x = "PFG", y = "value", fill = "value")) +
                                  scale_fill_gradientn(guide = F
                                                       , colors = brewer.pal(9, "RdYlGn")
                                                       , breaks = seq(0, 1, 0.2)
                                                       , limits = c(0, 1)) +
                                  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.08)) +
                                  geom_point(alpha = 0) +
                                  geom_bar(stat = "identity", na.rm = TRUE) +
                                  geom_hline(aes_string(yintercept = "hline")
                                             , lty = 2, color = "grey30") +
                                  geom_errorbar(aes_string(ymin = "value - sensitivity.sd"
                                                           , ymax = "value + sensitivity.sd")
                                                , color = "grey30", na.rm = TRUE) +
                                  geom_errorbar(aes_string(ymin = "value - specificity.sd"
                                                           , ymax = "value + specificity.sd")
                                                , color = "grey30", na.rm = TRUE) +
                                  geom_errorbar(aes_string(ymin = "value - AUC.sd"
                                                           , ymax = "value + AUC.sd")
                                                , color = "grey30", na.rm = TRUE) +
                                  annotate(geom = "text"
                                           , x = length(unique(mat.plot$PFG)) / 2
                                           , y = 1.05
                                           , label = subti
                                           , size = 4) +
                                  .getGraphics_theme() +
                                  theme(axis.text.x = element_text(angle = 90))
                                
                                pp = suppressWarnings(ggMarginal(pp, type = "boxplot", margins = "y", size = 7))
                              }
                              return(pp)
                            }
                          
                          ## 3. gather everything
                          pp_list[[6]] = pp_leg
                          pp_final = grid.arrange(grobs = pp_list
                                                  , layout_matrix = matrix(c(1,1,2,3,2,3,4,5,4,5,6,6)
                                                                           , ncol = 2, byrow = TRUE))
                          pp_final = cowplot::ggdraw(pp_final) + 
                            theme(plot.background = element_rect(fill = "transparent", color = NA))
                          return(pp_final)
                        } ## END loop on hab_names
                      pp
                    } else
                    {
                      shinyalert(type = "warning", text = paste0("The file provided does not contain the required columns "
                                                                 , "(PFG, HAB, AUC.sd, sensitivity.sd, specificity.sd, "
                                                                 , "sensitivity, TSS, specificity, AUC)) !"))
                    }
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , richness = {
                    ras.pts = as.data.frame(rasterToPoints(ras))
                    colnames(ras.pts) = c("X", "Y", "VALUE")
                    pp = pp.i(tab = ras.pts
                              , i.col = "Greens"
                              , i.axis = "Number of PFG"
                              , i.title = paste0("GRAPH C : map of PFG richness - Simulation year : "
                                                 , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1])
                              , i.subtitle = paste0("For each pixel and stratum, first relative abundances are calculated, "
                                                    , "then transformed into binary values :\n"
                                                    , "1 if the PFG abundance represents more than 5 % "
                                                    , "of the pixel abundance, 0 otherwise.\n"
                                                    , "If the PFG is present in one stratum, then it is considered present within the pixel.\n"
                                                    , "Finally, simulated PFG occurrences are summed.\n"))
                    list(pp)
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , cover = {
                    ras.pts = as.data.frame(rasterToPoints(ras))
                    colnames(ras.pts) = c("X", "Y", "VALUE")
                    pp = pp.i(tab = ras.pts
                              , i.col = "Greens"
                              , i.axis = "Abundance (%)"
                              , i.title = paste0("GRAPH C : map of PFG cover - Simulation year : "
                                                 , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1])
                              , i.subtitle = paste0("For each pixel, PFG abundances from strata "
                                                    , strsplit(sub(".*STRATA_", "", input$browser.files), "_")[[1]][1]
                                                    , " to "
                                                    , sub(".tif", "", tail(strsplit(input$browser.files, "_")[[1]], 1))
                                                    , " are summed,\n"
                                                    , "then transformed into relative values by dividing "
                                                    , "by the maximum abundance obtained.\n"))
                    list(pp)
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , light = {
                    ras.pts = as.data.frame(rasterToPoints(ras))
                    colnames(ras.pts) = c("X", "Y", "VALUE")
                    pp = pp.i(tab = ras.pts
                              , i.col = "Oranges"
                              , i.axis = "PFG light CWM"
                              , i.title = paste0("GRAPH C : map of light CWM - Simulation year : "
                                                 , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1])
                              , i.subtitle = paste0("For each pixel, PFG abundances from strata "
                                                    , strsplit(sub(".*STRATA_", "", input$browser.files), "_")[[1]][1]
                                                    , " to "
                                                    , sub(".tif", "", tail(strsplit(input$browser.files, "_")[[1]], 1))
                                                    , " are summed,\n"
                                                    , "then transformed into relative values by dividing by the maximum abundance obtained.\n"
                                                    , "Community Weighted Mean is then calculated with observed values of light for each PFG."))
                    list(pp)
                  }
                  ## ---------------------------------------------------------------------------------------------------------- ##
                  , soil = {
                    ras.pts = as.data.frame(rasterToPoints(ras))
                    colnames(ras.pts) = c("X", "Y", "VALUE")
                    pp = pp.i(tab = ras.pts
                              , i.col = "Oranges"
                              , i.axis = "PFG soil CWM"
                              , i.title = paste0("GRAPH C : map of soil CWM - Simulation year : "
                                                 , strsplit(sub(".*YEAR_", "", input$browser.files), "_")[[1]][1])
                              , i.subtitle = paste0("For each pixel, PFG abundances from strata "
                                                    , strsplit(sub(".*STRATA_", "", input$browser.files), "_")[[1]][1]
                                                    , " to "
                                                    , sub(".tif", "", tail(strsplit(input$browser.files, "_")[[1]], 1))
                                                    , " are summed,\n"
                                                    , "then transformed into relative values by dividing by the maximum abundance obtained.\n"
                                                    , "Community Weighted Mean is then calculated with observed values of soil for each PFG."))
                    list(pp)
                  }
      )
      
      if (is.ggplot(pp) || (is.list(pp) && length(pp) > 0))
      {
        shinyjs::show("go.left")
        shinyjs::show("go.right")
        shinyjs::disable("go.left")
        shinyjs::disable("go.right")
        if (length(pp) > 1)
        {
          shinyjs::enable("go.left")
        }
        
        output$UI.plot.browser = renderUI({
          lapply(1:length(pp), function(i) {
            if (i == length(pp)){
              plotOutput(outputId = paste0("plot.browser_", i), width = "100%", height = "600px")
            } else {
              shinyjs::hidden(plotOutput(outputId = paste0("plot.browser_", i), width = "100%", height = "600px"))
            }
          })
        })
        lapply(1:length(pp), function(i) {
          output[[paste0("plot.browser_", i)]] = renderPlot({ plot(pp[[i]]) })
        })
        
        RV$compt.browser.max <- length(pp)
        RV$compt.browser <- length(pp)
      }
    }
  }
})

####################################################################

observeEvent(input$go.left, {
  if (input$go.left > 0)
  {
    shinyjs::enable("go.right")
    RV$compt.browser <- RV$compt.browser - 1
    if (RV$compt.browser == 1)
    {
      shinyjs::disable("go.left")
    }
    shinyjs::toggle(paste0("plot.browser_", RV$compt.browser + 1))
    shinyjs::toggle(paste0("plot.browser_", RV$compt.browser))
  }
})

observeEvent(input$go.right, {
  if (input$go.right > 0)
  {
    shinyjs::enable("go.left")
    RV$compt.browser <- RV$compt.browser + 1
    if (RV$compt.browser == RV$compt.browser.max)
    {
      shinyjs::disable("go.right")
    }
    shinyjs::toggle(paste0("plot.browser_", RV$compt.browser - 1))
    shinyjs::toggle(paste0("plot.browser_", RV$compt.browser))
  }
})

