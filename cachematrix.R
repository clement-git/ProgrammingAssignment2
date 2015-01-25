## Below are two functions that cache and compute the inverse of
## a matrix

## The first function, makeCacheMatrix, creates a special "matrix", which is a 
## list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(mat = matrix()) {
  matinverse<- NULL
  set <- function(b) {
    mat <<-b
    matinverse <<-NULL
  }
  get <- function() mat
  setsolve <- function(solve) matinverse <<- solve
  getsolve <- function () matinverse
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## The second function, cachesolve, computes the inverse of the matrix created by 
## the first function. If the inverse has already been calculated, the function 
## gets the inverse from the cache. Otherwise, it calculates the inverse and sets
## the value of the inverse in the cache using the setsolve function.

cachesolve <- function (mat, ...) {
  matinverse <-mat$getsolve()
  if(!is.null(mat)) {
    message("getting cached data")
    return(matinverse)
  }
  data <- mat$get()
  matinverse <- solve(data, ...)
  mat$setsolve(matinverse)
  matinverse
}
