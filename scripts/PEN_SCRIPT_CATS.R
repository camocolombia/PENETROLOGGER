#####################################################################################

#PENETROLOGGER MEANS PER CATEGORIES
#CCSA 2018

#####################################################################################
mean_cat <- function(cat){
  me <- lapply(1:length(files_to_join),function(i){
    
    cat(round(i/length(files_to_join)*100,2)," %","\n")
    
    x <- files_to_join[[i]]
    plots <- unique(x$plot)
    
    #for(j in 1:length(plots)){
    plots_mean <- lapply(1:length(plots),function(j){
      cat("plot: ",round(j/length(plots)*100,2)," %","\n")
      xpl <- subset(x,x$plot==plots[[j]])
      
      
      y <- list(subset = unique(na.omit(xpl$moisture)),
                mean <- mean(subset(xpl$penetrologger, subset = xpl$depth %in% cat),na.rm=T))
      return(y)
      
      #};rm(j)
    })
    
    #do.call(plots_mean,rbind,)
    moisture <- do.call( rbind, plots_mean)[,1]; moisture <- mean(unlist(moisture),na.rm = T)
    peneto <- do.call( rbind, plots_mean)[,2]; peneto <- max(unlist(peneto),na.rm = T)
    
    x2 <- as.data.frame(matrix(ncol=10,nrow=1))
    
    x2[,2] <- x$file_ID[[i]]
    x2[,3] <- x$PlotCode[[i]]
    x2[,4] <- x$CoverGroupID[[i]]
    x2[,5] <- x$cover[[i]]
    x2[,6] <- as.character(paste0(min(cat),"-",max(cat)))
    x2[,7] <- peneto
    x2[,8] <- moisture
    x2[,9] <-x$cono_info[[i]]
    x2[,10] <-x$date[[i]]
    
    colnames(x2) <- c(
      "id",
      "file_ID",
      "FarmID",
      "CoverGroupID",
      "cover",
      "depth",
      "penetrologger",
      "moisture",
      "cono_info",
      "date"
    )
    
    return(x2)
  })
  return(me)
}

