#' Set working directory based on user input.
#'
#' \code{select.WorkingDirectory} allows the user to set the working directory based on their input. The are no arguments
#' to this function. Once you run it, you select the working directory with input placed in the console.
#' @export
select.WorkingDirectory <- function(){
  dir <- getwd()
  run <- T
  while(run == T){
    list.dirs(recursive = F)
    dirs <- c(list.dirs(recursive = F),substr(getwd(),1,tail(unlist(gregexpr('/',getwd()))-1, n=1)))
    nu_dirs <- 1:(length(dirs))
    dirs.df <- data.frame(nu_dirs,dirs)
    colnames(dirs.df) <- c('Number','Directory')
    print(dirs.df ,row.names = F)
    print(paste(nrow(dirs.df), 'returns to the directory above the current directory'))
    n <- readline(prompt = 'Select Working Directory Number: ')
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
          setwd(dirs.df$Directory[n])
          print(paste0('Working directory set to: ',getwd()))
          r <- F
          run <- F
          break
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
}
