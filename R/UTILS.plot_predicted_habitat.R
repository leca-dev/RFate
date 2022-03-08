### HEADER #####################################################################
##'
##' @title Create a raster map of habitat prediction for a specific \code{FATE} 
##' simulation at the last simulation year.
##' 
##' @name plot.predicted.habitat
##' 
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description This script is designed to create a raster map of habitat prediction
##' based on a habitat prediction file. For each pixel, the habitat failure or success value
##' is associated to a color and then, the map is built.
##' 
##' @param predicted.habitat a csv file created by the do.habitat.validation function
##' which contain, for each pixel of the studied map, the simulated and observed habitat.
##' @param col.df a data frame with all the colors associated with the failure or 
##' success of each studied habitat prediction.
##' @param simulation.map a raster map of the whole studied area.
##' @param output.path access path to the for the folder where output files
##' will be created.
##' @param sim.version name of the simulation we want to validate.
##' 
##' @details 
##' 
##' The function determine true/false prediction ('failure' if false, 'success' if true)
##' and prepare a dataframe containing color and habitat code. Then, the script merge
##' the prediction dataframe with the color and code habitat dataframe. Finally,
##' the function draw a raster map and a plot of prediction habitat over it thanks
##' to the data prepared before.
##' 
##' @return
##' 
##' a synthetic.prediction.png file which contain the final prediction plot.
##' 
##' @export
##' 
##' @importFrom dplyr all_of rename select
##' @importFrom utils write.csv
##' @importFrom raster raster crs extent res ratify writeRaster levels
##' @importFrom stats complete.cases
##' @importFrom ggplot2 ggplot geom_raster coord_equal scale_fill_manual 
##' ggtitle guides theme ggsave guide_legend
##' @importFrom reshape2 melt
##' @importFrom prettyR Mode
##' 
### END OF HEADER ##############################################################


plot.predicted.habitat<-function(predicted.habitat
                                 , col.df
                                 , simulation.map
                                 , output.path
                                 , sim.version)
{
  
  #auxiliary function to compute the proportion of simulations lead to the modal prediction
  count.habitat<-function(df){
    index<-which(names(df)=="modal.predicted.habitat")
    prop.simu<-sum(df[-index]==as.character(df[index]))/(length(names(df))-1)
    return(prop.simu)
  }
  
  #compute modal predicted habitat and the proportion of simulations predicting this habitat (for each pixel)
  predicted.habitat$modal.predicted.habitat<-apply(dplyr::select(predicted.habitat,sim.version),1,Mode)
  predicted.habitat$modal.predicted.habitat[predicted.habitat$modal.predicted.habitat==">1 mode"]<-"ambiguous"
  predicted.habitat$confidence<-apply(dplyr::select(predicted.habitat,c(all_of(sim.version),modal.predicted.habitat)),1,FUN=function(x) count.habitat(x))
  
  
  #true/false prediction
  predicted.habitat$prediction.code<-"failure"
  predicted.habitat$prediction.code[predicted.habitat$modal.predicted.habitat==predicted.habitat$true.habitat]<-"success"
  
  #prepare a df containing color & habitat code (to facilitate conversion into raster)
  col.df.long<-data.table::melt(data=setDT(col.df),id.vars="habitat",variable.name="prediction.code",value.name="color")
  
  habitat.code.df<-unique(dplyr::select(predicted.habitat,c(modal.predicted.habitat,prediction.code)))
  habitat.code.df$habitat.code<-seq(from=1,to=dim(habitat.code.df)[1],by=1)
  habitat.code.df<-rename(habitat.code.df,"habitat"="modal.predicted.habitat")
  
  habitat.code.df<-merge(habitat.code.df,col.df.long,by=c("habitat","prediction.code"))
  habitat.code.df$label<-paste0(habitat.code.df$habitat," (",habitat.code.df$prediction.code,")")
  
  #deal with out of scope habitat
  out.of.scope<-data.frame(habitat="out.of.scope",prediction.code="",habitat.code=0,color="white",label="out of scope")
  habitat.code.df<-rbind(habitat.code.df,out.of.scope)
  
  habitat.code.df$label<-as.factor(habitat.code.df$label)
  
  #order the df
  habitat.code.df<-habitat.code.df[order(habitat.code.df$label),] #to be sure it's in te right order (useful to ensure the correspondance between label and color in the ggplot function)
  
  
  #merge the prediction df with the df containing color and habitat code
  predicted.habitat<-merge(predicted.habitat,habitat.code.df,by.x=c("modal.predicted.habitat","prediction.code"),by.y=c("habitat","prediction.code"))
  write.csv(x = predicted.habitat, file = paste0(output.path, "/HABITAT/", sim.version, "/hab.pred.csv"))
  
  
  #plot
  
  #prepare raster
  prediction.map<-raster(nrows=nrow(simulation.map),ncols=ncol(simulation.map),crs=crs(simulation.map),ext=extent(simulation.map), resolution=res(simulation.map))
  
  prediction.map[]<-0 #initialization of the raster, corresponding to "out of scope habitats"
  prediction.map[predicted.habitat$pixel]<-predicted.habitat$habitat.code
  
  #ratify
  prediction.map<-ratify(prediction.map)
  prediction.map.rat<-levels(prediction.map)[[1]]
  prediction.map.rat<-merge(prediction.map.rat,habitat.code.df,by.x="ID",by.y="habitat.code")
  levels(prediction.map)<-prediction.map.rat
  
  #save the raster
  writeRaster(prediction.map,filename = paste0(output.path,"/HABITAT/", sim.version, "/synthetic.prediction.grd"),overwrite=T)
  
  
  #plot on R
  #convert into xy
  xy.prediction<-as.data.frame(prediction.map,xy=T)
  names(xy.prediction)<-c("x","y","habitat","prediction.code","color","label")
  xy.prediction<-xy.prediction[complete.cases(xy.prediction),]
  
  #plot
  prediction.plot<-
    ggplot(xy.prediction, aes(x=x, y=y, fill=factor(label)))+
    geom_raster(show.legend = T) +
    coord_equal()+
    scale_fill_manual(values = as.character(habitat.code.df$color))+ #ok only if habitat.code.df has been ordered according to "label" 
    ggtitle(paste0("Modal prediction over ",length(sim.version)," simulations"))+
    guides(fill=guide_legend(nrow=4,byrow=F))+
    theme(
      plot.title = element_text(size = 8),
      legend.text = element_text(size = 8, colour ="black"),
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )
  
  #save the map
  ggsave(filename="synthetic.prediction.png",plot = prediction.plot,path = paste0(output.path, "/HABITAT/", sim.version),scale = 1,dpi = 300,limitsize = F,width = 15,height = 15,units ="cm")
  
  #return the map
  return(prediction.plot)
  
}

