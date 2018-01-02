## Put comments here that give an overall description of what your
## functions do
## These two functions help cache the inverse of a matrix

## Write a short comment describing this function
## The function makeCacheMatrix sets and gets the matrix and sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, 	getinverse=getinverse)
}

## the cacheSolve function will compute inverse and cache the result of the matrix we just created

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}       

## Return a matrix that is the inverse of 'x'

##> x = rbind(c(2, 4), c(1, 4/3))
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1]     [,2]
##[1,]    2 4.000000
##[2,]    1 1.333333
##> cacheSolve(m)
##      [,1] [,2]
##[1,] -1.00  3.0
##[2,]  0.75 -1.5
##> cacheSolve(m)
##getting cached data.
##      [,1] [,2]
##[1,] -1.00  3.0
##[2,]  0.75 -1.5

