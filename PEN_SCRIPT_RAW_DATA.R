
#####################################################################################
#RUNNING FUNCTION
#####################################################################################
 
 DIR <- "C:/Users/csosa/Desktop/LINEA BASE PERÚ - PENETROLOGGER"
 DATA_DIR <- paste0(DIR,"/","RAW"); if(!file.exists(DATA_DIR)) { dir.create(DATA_DIR)}
 INPUT_DIR <- paste0(DIR,"/","INPUTS"); if(!file.exists(INPUT_DIR)) { dir.create(INPUT_DIR)}
 OUT_DIR <- paste0(DIR,"/","OUTCOMES"); if(!file.exists(OUT_DIR)) { dir.create(OUT_DIR)}
 OUT_DIR_RAW <- paste0(OUT_DIR,"/","CSV_RAW"); if(!file.exists(OUT_DIR_RAW)) { dir.create(OUT_DIR_RAW)}
 SRC_DIR <- paste0(DIR,"/","SCRIPTS"); if(!file.exists(SRC_DIR)) { dir.create(SRC_DIR)}
 pattern <- ".txt"

 source(paste0(SRC_DIR,"/","PEN_SCRIPT.R"))
 source(paste0(SRC_DIR,"/","PEN_SCRIPT_CATS.R"))
 
files_to_join <- PEN_TO_FORMAT(DATA_DIR,OUT_DIR,OUT_DIR_RAW,INPUT_DIR,pattern)
   
files_to_join2 <- do.call(rbind,files_to_join)

#####################################################################################
#SAVING SUMMARY FILE
#####################################################################################

write.csv(files_to_join2,
          paste0(OUT_DIR,"/","PENETROLOGGER.csv"),
          na="",
          row.names = F,
          quote = F)

#####################################################################################
#RUNNING  PENETROLOGGER ANALYSIS PER CATEGORIES
#####################################################################################


#k <- 1

#categories


cats <- list(
  "0-10"=0:10,
  "10-20"=11:20,
  "20-35"=21:35,
  "35-50"=36:50,
  "50-60"=51:60,
  "60-70"=61:70,
  "70-80"=71:80
)


file_means_cat <- lapply(1:length(cats),function(i){
  cat <- cats[[i]]
  file_means <- mean_cat(cat)
  file_means <- do.call(rbind,file_means)
  file_means$depth <- rep(names(cats)[[i]],nrow(file_means))
 # file_means$id <- 1:nrow(file_means) 
  return(file_means)
  
})

file_means_cat <- do.call(rbind,file_means_cat)
file_means_cat$id <- 1:nrow(file_means_cat)

#####################################################################################
#SAVING PER CATEGORIE CSV FILE
#####################################################################################

write.csv(file_means_cat,
          paste0(OUT_DIR,"/","PENETROLOGGER_per_cats.csv"),
          na="",
          row.names = F,
          quote = F)
