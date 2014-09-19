# The fuctions makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix.

# makeCacheMatrix creates a special matrix containing a function to
# 1. set and get the value of the matrix
# 2. set and get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve 
    getinverse <- function() i 
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse) 
}

# cacheSolve returns the inverse of the matrix. 
# 1. Check if the inverse has already been computed. 
# 1.1. If yes, return it.
# 2. If not, compute the inverse, setinverse function and return it.

# Works only with invertible matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
  x$setinverse(i)
  i
}

#example
## Create a 3x3 invertible matrix
#x <-matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3)
## Call makeCacheMatrix with matrix x
#m = makeCacheMatrix(x)
## Get matrix in cache
#m$get()
#[,1] [,2] [,3]
#[1,]    1    0    5
#[2,]    2    1    6
#[3,]    3    4    0

## First run: There isn't cache
#cacheSolve(m)
#[,1] [,2] [,3]
#[1,]  -24   20   -5
#[2,]   18  -15    4
#[3,]    5   -4    1

#Second run: Get inverse matrix from cache
#cacheSolve(m)
#getting cached data
#[,1] [,2] [,3]
#[1,]  -24   20   -5
#[2,]   18  -15    4
#[3,]    5   -4    1
