# Pair of functions that caches the inverse of a matrix. 

#Creating a special matrix object that can cache its inverse. 
makeCacheMatrix <- function( m = matrix() ){
  
  #Initializing 
  i<-NULL
  
  #Setting the matrix
  set<-function(matrix){
    m<<-matrix
    i<<-NULL
  }
  
  #To get the matrix
  get<-function(matrix){
    #Returning the matrix
    m
  }
  
  sI <- function(inverse) {
    i <<- inverse
  }
  
  
  gI <- function() {
    ## Return the inverse property
    i
  }
  
  list(set = set, get = get,
       sI = sI,
       gI = gI)
}


cacheSolve <- function(x, ...) {
  
  
  m <- x$gI()
  
  # Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  # Set the inverse to the object
  x$sI(m)
  
  # Return the matrix
  m
}