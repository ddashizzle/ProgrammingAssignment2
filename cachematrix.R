# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
#of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we 
#will not discuss here).

# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function: 

#  setmatrix will store the value of the matrix
#  getmatrix will return the value of the matrix
#  setinv will calculate and store the value of the inverse matrix
#  getinv will return the value of the inverse matrix

#note: it is necessary to run makeCacheMatrix before running cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
}


# The following function calculates the inverse of the special "vector" created with the above function. 
#However, it first checks to see if it has already been calculated. If so, it gets the inverse from 
#the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value
#in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


'
#Test sample matrix data

# samplematrix data and comparative results pulled from first example from 
#https://www.mathsisfun.com/algebra/matrix-inverse.html

samplematrix = rbind(c(4,7),c(2,6))

sample = makeCacheMatrix(samplematrix)
sample$getmatrix()

#Calculate 
cacheSolve(sample)

#Pull results from cache
cacheSolve(sample)

'
