## A pair of functions that cache the inverse of a matrix
##
## "makeCacheMatrix" is a function factory that returns a list of 4 functions that are
## enclosed in the environment of the original function call to "makeCacheMatrix". 
## Therefore all of them reference the same matrix with wich "makeCacheMatrix"
## was originaly loaded. This simulates a mutable object.
## The 4 functions are ways to set the matrix to a new value, to retrieve the matrix, 
## to compute an inverse and retrieve it. The object keeps track of any change to the matrix and stores
## a cached value for its inverse, if it has been computed, for as long as the underlying matrix is not 
## changed. 
## The "cacheSolve" functions checks if an inverse for a "makeCacheMatrix" matrix is in the cache and
## returns it. Otherwise it will compute and then cache it.



## This function makes a list of 4 enclose functions to set and retrieve a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
        inverse_exists <- NULL
        set <- function(y) {                               #function to change the matrix
        x <<- y                                            #in the parent.frame (<<- operator)  
        inverse_exits <<- NULL
  }
  get <- function() x                                           #function to retrieve the matrix
  setinverse <- function(solve) inverse_exists <<- solve        #function to compute the inverse
  getinverse <- function() inverse_exists                       #function to retrieve the inverse
  list(set=set, get=get,setinverse=setinverse,
       getinverse=getinverse)
}


## This function checks if an inverse of the matrix attached to the list already exists and returns it.
## Otherwise it computesm caches and returns the newly computed inverse.

cacheSolve <- function(x, ...) {
  
  inverse_exists <- x$getinverse()                    #retrieve cached inverse
  if(!is.null(inverse_exists)) {                      #check if cached inverse exists
          message("loadindg cached inverse")
          return(inverse_exists)                      #return cached inverse
  }
  temp <- x$get()                                     #no cached inverse exists, retrieve matrix
  inverse_exists <- solve(temp, ...)                  #calculate inverse
  x$setinverse(inverse_exists)                        #cache inverse,
  return(inverse_exists)                              #return it.
}


