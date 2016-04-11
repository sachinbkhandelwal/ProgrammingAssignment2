## "<<-" operator is used to assign a value to an object in an environment that is different from the current environment. 
## Writing function to cache a square matrix inverse
## The input matrix is read as x and is assumed that user has given a square matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Assigning default value NULL 
    i <- NULL
   
    set_sqmatrix <- function(y) {
      ## The square matrix needs to be stored outside the current environment 
      x <<- y
      
      ## Assigning Default value NULL
      i <<- NULL
    }
    
    get_sqmatrix <- function() {
      x
    }
    
    set_inverse <- function(inverse){
    ## The inverse of square matrix needs to be stored outside the current environment
      i <<- inverse
    }
     
    get_inverse <- function(){
    ##If the matrix inverse has already been calculated, then it is returned by this function
    ## Otherwise default value NULL is returned
      i
    }

    ## Defining return value of makeCacheMatrix as a list and assigning names to its members    
    list(set_sqmatrix = set_sqmatrix,
         get_sqmatrix = get_sqmatrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}



## CacheSolve function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv_x_cache <- x$get_inverse()
  if(!is.null(inv_x_cache)) {
    message("Getting Cached Data")
    return(inv_x_cache)
  }

  ##Getting the square matrix and Computing inverse of square matrix if not available in cache  
  inv_x<-solve(x$get_sqmatrix())
  x$set_inverse(inv_x)
  message("Computed inverse of square matrix")
  inv_x
  
}
