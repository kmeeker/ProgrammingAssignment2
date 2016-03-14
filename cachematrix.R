## Put comments here that give an overall description of what your
## functions do

## example use:
## A <- matrix(c(1,2,3, 0,1,4, 5,6,0),nrow=3,ncol=3,byrow=TRUE)
## B<-makeCacheMatrix(A)
## cacheSolve(B)
## cacheSolve(B)
## > cacheSolve(B)
## getting cached data
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1


## Write a short comment describing this function
## Create a CacheMatrix which stores its inverse in cache
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        
        list(set = set, get = get,
             setinv=setinv,
             getinv=getinv)
}


## Write a short comment describing this function
## Compute the inverse of the CacheMatrix and store it in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
