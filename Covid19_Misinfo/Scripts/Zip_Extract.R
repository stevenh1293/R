#Load in necessary packages
library(rlang)
library(zip)
library(parallel)


#Set working directory to home directory of project and save as vector
if (getwd() != "/home/steven/R/JSON Flatten"){
  setwd('/home/steven/R/JSON Flatten')
}
dir <- getwd()


#Save the name of all month folders containing zip files to a vector and unzip
for (i in as.list(list.files(paste0(dir,"/CSV"),pattern = ".zip"))){
  setwd(paste0(dir,"/CSV/"))
  print(paste0('Unzipping: ',i))
  lapply(X = i, FUN = unzip)
  unlink(i)
  rm(i)
}


#For loop to unzip all CSV files in each month directory and place 100 CSV files into subdirectories
#   Files are too large to load into R all together, so we have to load in 100 files at a time to extract the hashtags
#   Also checks to see if the month folder has already been unzipped and skips if so (modified to remove due to storage limit)
for (i in list.files(paste0(dir,"/CSV"))){
  Unzipped <- list.files(paste0(dir,"/CSV_Unzip"))
  AlreadyUnzipped <- which(i == Unzipped)
  if (is_empty(AlreadyUnzipped) == F){
    print(paste0("Already unzipped all files in: ",i))
    setwd(paste0(dir,"/CSV/"))
    unlink(i,recursive = T)
  }
  if (is_empty(AlreadyUnzipped) == T){
    setwd(paste0(dir,"/CSV/",i))
    print(paste0('Unzipping all files in: ',i))
    Number_of_Files <- length(list.files(paste0(dir,"/CSV/",i)))
    Number_of_Files_Over <- as.integer(substr(Number_of_Files,2,3))
    Number_of_Subdirs <- seq(1:ceiling(Number_of_Files *.01))
    files <- list.files(paste0(dir,"/CSV/",i))
    
    seq1 <- seq.int(1,Number_of_Files - Number_of_Files_Over,100)
    seq2 <- seq.int(100,Number_of_Files - Number_of_Files_Over,100)
    seq1 <- c(seq1, max(seq2)+1)
    seq2 <- c(seq2,max(seq2)+Number_of_Files_Over)
    
    for (x in Number_of_Subdirs){
      file_set <- as.list(files[seq1[x]:seq2[x]])
      ncores <- as.integer(detectCores() - 2)
      cl <- makeCluster(ncores)
      clusterEvalQ(cl,library(zip))
      clusterExport(cl, varlist = "file_set")
      parLapply(cl, X = file_set, fun = unzip, exdir = paste0(dir,"/CSV_Unzip/",i,"/",Number_of_Subdirs[x]))
      stopCluster(cl)
    }
    rm(file_set,ncores,seq1,seq2,Number_of_Files,Number_of_Files_Over,Number_of_Subdirs,files,cl,x)
  }
  rm(AlreadyUnzipped,Unzipped,i)
}


#Reset WD to project root
if (getwd() != "/home/steven/R/JSON Flatten"){
  setwd('/home/steven/R/JSON Flatten')
}
