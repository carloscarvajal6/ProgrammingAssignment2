## makeCacheMatrix provides the special "matrix" that is able to cache
## its inverse to save time and resources. cacheSolve provides the 
## computation needed to first check whether this inverse has been 
## calculated, and if not, calculate it and store it in cache

## This function creates a special "matrix", which is really a list 
## that contains functions to set/get value of the matrix, and its inverse
## Here, I assume the matrix is always square invertible

makeCacheMatrix <- function(x = matrix()) {
      ## container
      m<-NULL
      ## set the matrix
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      ## get the matrix
      get<-function() x
      ## to store the inverse matrix in cache
      setinverse<-function(solve) m <<- solve
      ## to retrieve inverse matrix from cached value
      getinverse<-function() m
      ## create the special "matrix" (list with functions)
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function calculates the inverse matrix of the special "matrix" 
## created with the makeCacheMatrix function. It first checks whether 
## its inverse matrix has already been calculated. If so, gets it from the
## cache and skips the computation. Otherwise, it calculates it and sets 
## the value into cache via the setinverse function

cacheSolve <- function(x, ...) {
      ## check if the inverse has been calculated
      m<-x$getinverse()
      ## if not, calculate it
      if(!is.null(m)){
            message("getting cached inverse matrix")
            return(m)
      }
      ## get value of the matrix for computation
      data<-x$get()
      ## actually compute the inverse matrix
      m<-solve(data,...)
      ## store it in cache for later use
      x$setinverse(m)
      ## return the value of the inverse
      m
}
