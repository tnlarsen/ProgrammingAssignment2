## These two functions together allows for caching of the inverse of a matrix.
## The function 'makeCacheMatrix' creates the special 'matrix' that caches its inverse. 
## To get the inverse use the 'cacheSolve' function. See the usage example for details.
## Note that the inverse is calculated lazily.
##
##   Usage example:
##    
##       data<-replicate(1000, rnorm(1000))     # the real data matrix
##       cacheMatrix<-makeCacheMatrix(data)     # create the caching 'matrix'
##       cms<-cacheSolve(cacheMatrix)           # solve it
##       <snip>
##       cms1<-cacheSolve(cacheMatrix)          # get the cached value
## 
##   The first call to 'cacheSolve' will calculate the 'inverse' of 'cacheMatrix'
##   The second call will return the cached value

## This function creates a special 'matrix' that caches its inverse. The inverse
## should be obtained using the function "cacheSolve" below.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    getInverse <- function() {
        inverse
    }
    
    setInverse <- function(inverseToSet) {
        inverse <<- inverseToSet
    }
    
    getInner <- function() {
        inner <- x
    }
    
    list(getInverse = getInverse, setInverse = setInverse, getInner = getInner)
}


## This function returns the inverse of the special 'matrix' argument. Note that the argument 
## must be created using the 'makeCacheMatrix' function above.
## The logical 'debug' argument alows turning on debug "logging" to enable
## verification of caching.
cacheSolve <- function(x, ..., debug = FALSE) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        if(debug) message("returning cached data")
        inverse
    } else {
        if(debug) message("calculating...")
        inverse <- solve(x$getInner(), ...)
        x$setInverse(inverse)
        inverse
    }
}
