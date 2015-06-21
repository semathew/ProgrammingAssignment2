## Programming Assignment-2
# Programming Assignment 2 -
# This consists of 2 functions
# "makeCacheMatrix" :
## This function creates a "matrix" object that can cache its inverse.
# "cacheSolve" :
##It computes the inverse of the "matrix" returned by makeCacheMatrix
## above. if already capured , value is retreived from cache
#
# 1.Set the initial matrix
makeCacheMatrix <- function(x = matrix())
{
    m <- NULL # Initialize the field to hold the cached value
    set <- function(y)  # set the value of the matrix
    {
        x <<- y # Assign value
        m <<- NULL
    }
## 2.Get the value of the matrix
    get <- function() x
## 3.Set Inverse of the matrix
    setInverse <- function(inverse) m <<- inverse
## 4.Get Inverse of the matrix
    getInverse <- function() m
# 5.Return functions
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}
cachesolve <- function(x)
{
# Gets cached value, if available and return the value
m <- x$getInverse()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
# If no cached value , cacluate Inverse
    data <- x$get()  # Get the matrix
    m <- solve(data) # Get the Inverse
    x$setInverse(m)  # cache calculated inverse 
    return(m) # Return matrix
}
