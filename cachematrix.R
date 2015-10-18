## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        setMatrix <- function(mtx) {
                x <<- mtx
                s <<- NULL
        }
        getMatrix <- function() {x}
        setSolve <- function(slv) {
                s <<- slv
        }
        getSolve <- function() {s}
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setSolve = setSolve, getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function. If the inverse has already been calculated,
## it retrieves the cached inverse.

cacheSolve <- function(x, ...) {
        slv <- x$getSolve()
        if (!is.null(slv)) {
                message("getting cached data")
                return(slv)
        }
        slv <- solve(x$getMatrix(), ...)
        x$setSolve(slv)
        slv
}
