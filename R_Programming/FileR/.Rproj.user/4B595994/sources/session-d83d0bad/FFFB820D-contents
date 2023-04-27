#' Extract files based on user input to select the input and output directory, and to select the file type
#'
#' \code{extract.Files} uses R.utils and utils to extract files from .zip and .gz files. It takes user input in order to
#' select the input and output directories. Any other file types will be ignored.
#'
#' @usage extract.Files(path = getwd(),
#'                       exdir = getwd(),
#'                       file.type = c('.zip','.gz'),
#'                       overwrite.files = F)
#'
#' @param path This is the path for the input directory. Leaving this blank will set the current working directory as the
#' input directory. Typing in "select" as the path will allow you to select the directory based on user input.
#' @param exdir This is the directory to be used as the output for extracting the files. Leaving this blank will set the
#' current working directory as the output directory. Typing in "select" as the path will allow you to select the directory
#' based on user input.
#' @param overwrite.files This determines whether or not files with the same name will be overwritten in the output directory.
#'
#' @importFrom R.utils getAbsolutePath gunzip
#' @export
extract.Files <- function (path = getwd(),
                           exdir = getwd(),
                           file.type = c('.zip','.gz'),
                           overwrite.files = F){


  #Select path to compressed files
  if (path == 'select') {
    if (!('.zip' %in% file.type) | !('.gz' %in% file.type)){
      break
    }
    dirtop <- getwd()
    run <- T
    while(run == T){
      list.dirs(recursive = F)
      dirs <- c(list.dirs(recursive = F),substr(getwd(),1,tail(unlist(gregexpr('/',getwd()))-1, n=1)))
      nu_dirs <- 1:(length(dirs))
      dirs.df <- data.frame(nu_dirs,dirs)
      colnames(dirs.df) <- c('Number','Directory')
      print(dirs.df ,row.names = F)
      print(paste(nrow(dirs.df), 'returns to the directory above the current directory'))
      n <- readline(prompt = 'Select number for path of files to be extracted: ')
      n <- as.integer(n)
      if (is.na(n) == T | is.double(n) == T){
        stop('Entered value is not an integer, please type an integer')
      }
      if (!(n %in% 1:length(dirs))){
        stop('Number outside of directory range')
      } else {
        r <- T
        while(r == T){
          print('1.) Set working directory')
          print('2.) Choose Subdirectory')
          continue <- readline(prompt = 'Select option 1 or 2: ')
          continue <- as.integer(continue)
          if (is.na(continue) == T | is.double(continue) == T | continue < 1 | continue > 2){
            stop('Entered value is invalid, please select 1 or 2')
          }
          if (continue == 1){
            inputDir <- getAbsolutePath(dirs.df$Directory[n])
            print(paste0('Extracting files from ', inputDir))

            #Set exit directory
            if (exdir == 'select'){
              setwd(dirtop)
              run <- T
              while(run == T){
                list.dirs(recursive = F)
                dirs <- c(list.dirs(recursive = F),substr(getwd(),1,tail(unlist(gregexpr('/',getwd()))-1, n=1)))
                nu_dirs <- 1:(length(dirs))
                dirs.df <- data.frame(nu_dirs,dirs)
                colnames(dirs.df) <- c('Number','Directory')
                print(dirs.df ,row.names = F)
                print(paste(nrow(dirs.df), 'returns to the directory above the current directory'))
                n <- readline(prompt = 'Select number of directory to extract files to: ')
                n <- as.integer(n)
                if (is.na(n) == T | is.double(n) == T){
                  stop('Entered value is not an integer, please type an integer')
                }
                if (!(n %in% 1:length(dirs))){
                  stop('Number outside of directory range')
                } else {
                  r <- T
                  while(r == T){
                    print('1.) Set working directory')
                    print('2.) Choose Subdirectory')
                    continue <- readline(prompt = 'Select option 1 or 2: ')
                    continue <- as.integer(continue)
                    if (is.na(continue) == T | is.double(continue) == T | continue < 1 | continue > 2){
                      stop('Entered value is invalid, please select 1 or 2')
                    }
                    if (continue == 1){
                      outputDir <- getAbsolutePath(dirs.df$Directory[n])
                      print(paste0('Extracting ', file.type, ' files to ', outputDir))
                      r <- F
                      run <- F
                      break
                    }
                    if (continue == 2) {
                      r <- F
                      setwd(dirs.df$Directory[n])
                    }
                    if (r == F) break()
                  }
                }
                if (run == F) break
              }
            }
          }
          if (continue == 2) {
            r <- F
            setwd(dirs.df$Directory[n])
          }
          rm(continue)
          if (r == F) break()
        }
      }
      if (run == F) break
    }
    path <- inputDir
    exdir <- outputDir
  }

  #File extraction
  setwd(path)
  if ('.zip' %in% file.type == T){
    zipfiles <- list.files(pattern = '.zip$')
    txtgzzipfiles <- list.files(pattern = 'txt.gz.zip$')
    zipindex <- match(txtgzzipfiles,zipfiles)
    if (length(zipindex) > 0){
      zipfiles <- zipfiles[-zipindex]
    }
    for (i in zipfiles){
      unzip(paste0(getwd(),'/',i),
            overwrite = overwrite.files,
            exdir = outputDir)
    }
    for (i in txtgzzipfiles){
      unzip(paste0(getwd(),'/',i),
            overwrite = overwrite.files)
    }
    gzfilestoremove <- list.files(pattern = 'txt.gz$')
    for (i in gzfilestoremove){
      R.utils::gunzip(i,
             overwrite = overwrite.files,
             remove = T,
             destname = paste0(outputDir,'/',gsub('.gz','',i)))
    }
  txtgzzipfiles <- list.files(pattern = 'txt.gz.zip$')


  }

  txtgzzipfiles <- list.files(pattern = 'txt.gz.zip$')
  gzfiles <- list.files(pattern = '.gz')
  gzindex <- match(txtgzzipfiles,gzfiles)
  if (length(gzindex) > 0){
    gzfiles <- gzfiles[-gzindex]
  }

  if ('.gz' %in% file.type == T){
    for (i in gzfiles){
      R.utils::gunzip(i,
             overwrite = overwrite.files,
             remove = F,
             destname = paste0(outputDir,'/',gsub('.gz','',i)))
    }
  }
  setwd(dirtop)
}
