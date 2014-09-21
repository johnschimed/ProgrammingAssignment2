## Cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( mat = matrix() ) {
    inverse <- NULL
    ##Setters and getters
    set <- function( matrix ) {
            mat <<- matrix
            inverse <<- NULL
    }

    getInverse <- function() {
        inverse
    }
    setInverse <- function(inverseSetter) {
        inverse <<- inverseSetter
    }

    get <- function() {
    	mat
    }

    ## Methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## Return cachesolve if the matrix has not been changed and the matrix has already been computed
cacheSolve <- function(x, ...) {
    ## The return value is an inverse of x
    mat <- x$getInverse()

    ## Return inverse if already set
    if( !is.null(mat) ) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()

    ## Matrix multiplication
    mat <- solve(data) %*% data

    
    x$setInverse(mat)
    ## Return the matrix
    mat
}
