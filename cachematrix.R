## This script contains two functions, makeCacheMatrix and cacheSolve
## makeCacheMatrix: is used for creating a matrix object with functions that make it possible to set and get it's inverse (if it has already been calculated)
## cacheSolve: accepts an objects created via the makeCacheMatrix function, it calculates the inverse of the matrix if the matrix does not 
##             already have an inverse calcualted. If the object's inverse has already been calculated that calculated inverse will be returned
## 
## Assumptions: the usage of a invertible square matrix 
## Example: foo<-makeCacheMatrix(matrix(1:4, ncol=2, nrow=2))
##          cacheSolve(foo)
##          cacheSolve(foo) - a message will be displayed stating the inverse has already been calculated

## makeCacheMatrix, accepts an matrix object, default is an empty matrix. Adds setInverse and getInverse functions for setting and getting the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  
  list(   set = set
          , get = get
          , setInverse = setInverse
          , getInverse = getInverse )
  
}


## cacheSolve: accepts an object created via makeCacheMatrix and calculates the inverse of the matrix, if the inverse has already been calculated
##             for the giving object, that calculated inverse is returned, hence the caching
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  
  if(!is.null(inv)) {
    message("Found cached inverse, returning...")
    return(inv)
  }
  
  matrix<-x$get()
  inverse<-solve(matrix)
  x$setInverse(inverse)
  inverse
  
}
