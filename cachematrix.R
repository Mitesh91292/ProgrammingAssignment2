## we are caching the inverse of a matrix, just as the practise example for mean
## we can use this for inverting large matrices from some example data
## makeCacheMatrix & cacheSolve are the two functions for this job


## here we create a "special matrix" which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function()x
    set.inverse <- function(inverse) m <<- inverse
    get.inverse <- function() m
    list(set = set, get = get, set.inverse = set.inverse, 
         get.inverse = get.inverse)
}
## this function creates inverse of matrix which is created by above function
## it checks it inverse is already computed
## if not, it solves the matrix inverse
## if yes, it just returns the value from "get.inverse" above

cacheSolve <- function(x, ...) {
        m <- x$get.inverse()
        if(!is.null(m)){
            message("getting cached inverse")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set.inverse(m)
        m
}
