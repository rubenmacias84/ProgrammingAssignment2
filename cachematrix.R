## This function calculates the inverse of a square matrix
## and save it in cache

## Function that will create the inverse of a matrix and will store ir in cache

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                                      ## Inicializate m to Null 
  set<-function(y){                            ## Define the set function
    x<<-y                                      ## that store the Matrix in cache
    m<<-NULL                                   ## And make m Null in cache
  }
  get<-function() x                            ## Define the get function
  setmatrix<-function(solve) m<<- solve        ## Creating the funtion setmatrix that 
                                               ## calculate the inverse and store it
                                               ## in cache in m
  getmatrix<-function() m                      ## Define the getmatrix function
  list(set=set, get=get,                       ## Creating a list with the 
       setmatrix=setmatrix,                    ## value of the functions
       getmatrix=getmatrix)
}

## Now we will check if we have the inverse
## matrix in cache or not and we will show it

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()                           ## Assign to m what we have save in getmatrix
  if(!is.null(m)){                           ## Check if is not empty
    message("getting cached data")
    return(m)                                ##Return a message and the inverse
  }
  matrix<-x$get()                            ## Asign to 'matrix' the value of get in x
  m<-solve(matrix, ...)                      ## Asing to m the inverse of 'matrix'
  x$setmatrix(m)                             
  m                                          ## Return m (the inverse)
}