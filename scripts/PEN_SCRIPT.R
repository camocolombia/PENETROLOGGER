#####################################################################################

#PENETROLOGGER RAW_TO_CURRENT_STANDAR_FORMAT
#CCSA 2018

#####################################################################################
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

library(xlsx)
#####################################################################################
#CALLING PATHS
#####################################################################################
# 
# DATA_DIR <-"C:/Users/csosa/Desktop/LINEA BASE PERÚ - PENETROLOGGER/RAW"
# OUT_DIR <-"C:/Users/csosa/Desktop/LINEA BASE PERÚ - PENETROLOGGER/OUTCOMES"
# pattern <- ".txt"
# 
PEN_TO_FORMAT<-function(DATA_DIR,OUT_DIR,OUT_DIR_RAW,INPUT_DIR,pattern){
  
  cat("processing ",as.character(DATA_DIR)," please be patient","\n" )
  cat("                                                       ","\n")
  
  #####################################################################################
  #CALLING LIBRARIES
  #####################################################################################
  
  library(tidyverse)
  
  #####################################################################################
  #LISTING FILES
  ###########  
  EXCEL_FILES<-list.files(DATA_DIR,pattern = pattern,full.names = T)
  EXCEL_FILES_D<-list.files(DATA_DIR,pattern = pattern,full.names = F)
  EXCEL_FILES_D<-sub(pattern,"",EXCEL_FILES_D)
  
  #####################################################################################
  #EXTRACT DATA
  #####################################################################################
  
  cat("Extracting data from ",as.character(DATA_DIR),"\n" )
  
  
  EXC <- lapply(1:length(EXCEL_FILES),function(x){
    
    cat(x," | ",(x/length(EXCEL_FILES)*100)," %","\n" );cat("      ","\n")
    
    if(!file.exists(paste0(OUT_DIR_RAW,"/",EXCEL_FILES_D[[x]],".csv"))){
      
      
      a <- read.csv(file=EXCEL_FILES[[x]],header=F,sep=",",as.is = T)
      
      #Splitting archive in descriptive and variables to be extracted
      a1 <- a[1:which(a=="PENETRATION DATA"),1]
      a2 <- a[((which(a=="PENETRATION DATA")+1):nrow(a)),1]
      
      #####################################################################################
      #GATHERING ID INFORMATION
      #####################################################################################
      
      name <- unlist(strsplit(EXCEL_FILES_D[[x]],c("-")))
      name2 <- strsplit(name[],c("_"))
      #####################################################################################
      #GATHERING INFORMATION FROM FILE
      #####################################################################################
      
      desc_data <- as.data.frame(gsub("\t"," ",a1)); colnames(desc_data) <- "Description"
      
      info <- as.data.frame(do.call(cbind,lapply(1:length(a2),function(i){
        x <- as.matrix(unlist(strsplit(x=a2[i],split=as.character("\t"),fixed = F))) 
        return(x)
      }))
      )
      
      cat_info <- as.data.frame(info[1:3,])
      
      for(i in 1:ncol(cat_info)){
        cat_info[,i] <- as.character(cat_info[,i])
      };rm(i)
      
      info <-  info[-c(1:3),]
      row.names(info) <- 0:(nrow(info)-1)
      
      for(i in 1:ncol(info)){
        
        info[,i] <- as.numeric(as.character(info[,i]))
        info[,i][which(info[,i]==-1)] <-NA
      };rm(i)
      
      
      info$depth <- row.names(info)
      info <- info[,c(ncol(info),1:(ncol(info)-1))]
      colnames(info)[2:ncol(info)] <- cat_info[1,]
      
      #####################################################################################
      #COORDINATES CONVERSION
      #####################################################################################
      coords <- cat_info[2,];for(i in 1:ncol(cat_info)){ coords[,i] <- as.character(coords[,i])}
      
      coords_i <- lapply(1:ncol(coords),function(i){
        x <- as.data.frame(as.matrix(t(unlist(strsplit(coords[,i],"[[:space:]]")))))
        NS <- substr(as.character(x[,1]),1,1)
        EW <- substr(as.character(x[,3]),1,1)
        x[,1] <- gsub(as.character(NS),"",x[,1]); x[,3] <- gsub(as.character(EW),"",x[,3])
        
        for(j in 1:4){ x[,j] <-as.numeric(as.character(x[,j]))};rm(j)
        
        x$lat <- NA; x$lon <- NA
        #LATITUDE
        if (NS=="N") {
          x$lat<-   round((x[,1]+(x[,2]/60))+(0/3600),4)
        } else {
          x$lat<- -(round((x[,1]-(x[,2]/60))-(0/3600),4))
        }
        #LONGITUDE
        if (EW=="E") {
          x$lon<- round((x[,3]+(x[,4]/60))+(0/3600),4)
          
        } else {
          x$lon<- -(round((x[,3]-(x[,4]/60))-(0/3600),4))
        }
        
        x <- cbind(x$lat,x$lon)
        colnames(x) <- c("lat","lon")
        return(x)
      })
      
      #####################################################################################
      #RETURNING COVER
      #####################################################################################
      
      cover <- read.csv(paste0(INPUT_DIR,"/","cover.csv"))
      
      data_info <- as.data.frame(matrix(ncol=15,nrow=(nrow(info)*ncol(cat_info))))
      
      data_info[,1] <- 1:nrow(data_info)
      data_info[,2] <- rep(EXCEL_FILES_D[[x]],nrow(info))
      data_info[,3] <- rep(name2[[1]][1],nrow(info))
      data_info[,4] <- rep(name2[[2]][1],nrow(info))
      data_info[,5] <- rep(name2[[2]][2],nrow(info))
      data_info[,6] <- do.call(rbind,lapply(1:ncol(cat_info),function(i){ x <- as.matrix(rep(cat_info[1,i],nrow(info)));return(x) }))
      
      for(i in 1:nrow(cover)){
        if(length(data_info[which(as.character(data_info[,5])==cover[i,"IMP_ID"]),])>0){
          data_info[,7][which(data_info[,5]==cover[i,"IMP_ID"])] <-as.character(cover[i,"COVER"])
        } else {
          data_info[,7][which(data_info[,5]==cover[i,"IMP_ID"])] <- NA
        }
      };rm(i)
      
      data_info[,8] <- rep(info$depth,ncol(cat_info))
      data_info[,9] <- do.call(rbind,lapply(2:ncol(info),function(i){ x <-  as.matrix(info[,i]);return(x)}))
      data_info[,10] <- do.call(rbind,lapply(1:ncol(cat_info),function(i){ x <- as.matrix(rep(cat_info[3,i],nrow(info)));return(x) }))
      data_info[,11] <- do.call(rbind,lapply(1:length(coords_i),function(i){ x <- as.matrix(rep(coords_i[[i]][,1],nrow(info)));return(x) }))
      data_info[,12] <- do.call(rbind,lapply(1:length(coords_i),function(i){ x <- as.matrix(rep(coords_i[[i]][,2],nrow(info)));return(x) }))
      data_info[,13] <- do.call(rbind,lapply(1:length(coords_i),function(i){ x <- as.matrix(rep(cat_info[2,i],nrow(info)));return(x) }))
      
      
      #TIPO DE CONO
      mylist <- lapply(1:nrow(desc_data),function(i){
        if(is.integer0(grep("Tipo de cono",as.character(desc_data[i,])))){
          x <- NULL
        } else { x <- i}; return(x)
        
      })
      
      data_info[,14] <- do.call(rbind,lapply(1:ncol(cat_info),function(i){ x <- as.matrix(rep(as.character(desc_data[unlist(mylist[-which(sapply(mylist, is.null))]),]),nrow(info)));return(x) }))
      data_info[,14] <- gsub("Tipo de cono       : ","",data_info[,14])
      
      #FECHA
      mylist_date <- lapply(1:nrow(desc_data),function(i){
        if(is.integer0(grep("Fecha parcela",as.character(desc_data[i,])))){
          x <- NULL
        } else { x <- i}; return(x)
        
      })
      
      data_info[,15] <- do.call(rbind,lapply(1:ncol(cat_info),function(i){ x <- as.matrix(rep(as.character(desc_data[unlist(mylist_date[-which(sapply(mylist_date, is.null))]),]),nrow(info)));return(x) }))
      data_info[15] <- gsub("Fecha parcela      : ","",data_info[,15])
      
      colnames(data_info) <- 
        c("id",
          "file_ID",
          "FarmID",
          "PlotCode",
          "CoverGroupID",
          "plot", # 6
          "cover", #new 7
          "depth",
          "penetrologger",
          "moisture",
          "lat",
          "lon",
          "coords",
          "cono_info",
          "date"
        )
      
      #####################################################################################
      #SAVING RESULTS
      #####################################################################################
      
      cat("saving CSV file","\n" )
      
      write.csv(data_info,paste0(OUT_DIR_RAW,"/",EXCEL_FILES_D[[x]],".csv"),quote = F,row.names = F) 
      
    } else {
      data_info <-  read.csv(paste0(OUT_DIR_RAW,"/",EXCEL_FILES_D[[x]],".csv"))
      
    }
    
    return(data_info)
    
  })
  
  return(EXC)
  
  cat("DONE!","\n" );gc()
  
}

