## this function is creating a special matrix, containing a function to: 
## set the matrix value
## get the matrix value
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv<< NULL
        }
        get <- function () x
        setInverse <- function(inverse) inv <<-inverse
        getInverse<- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## this function calculates the inverse of the matrix created in the makeCacheMatrix function
## if the inverse has already been calculated, it gets the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <-x$get()
        inv<- solve(data, ...)
        x$setInverse(inv)
        inv
}
