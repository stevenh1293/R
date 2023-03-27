#Function for mode
math.mode <- function(v){
  if(is.vector(v)==T){
    if(is.numeric(v) == T | is.integer(v)==T){
      mostFreq <- aggregate(v,list(num=v), length)
      index <- which(mostFreq$x == max(mostFreq$x))
      answer <- mostFreq$num[index]
      return(answer)
    }
    else{
      stop('Object data type is not numeric or integer')
    }
  }
  else{
    stop('Object is not a vector')
  }
}




