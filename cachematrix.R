## These two functions cache the inverse of a matrix.
## 
## The first function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL

set<-function(y){
  x<<-y
  inv<<-NULL
}

get<-function()x
setmatrix<-function(solve)inv<<-solve
getmatrix<-function()inv
list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## The below function computes the inverse of the special "matrix" returned
## by the first function, tests to see if the inverse has already been 
## calculated, and then retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Returns the inverse matrix of x   
  inv<-x$getmatrix()
  
  ## Determines if inverse is already calculated; if so, returns comment with 
  ## cached data
  if(!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  }
  ## If inverse is not cached, calculates the inverse and sets it in memory.
  matrix<-x$get()
  inv<-solve(matrix)
  x$setmatrix(inv)
  ## Returns the inverse of the matrix.
  inv
}
