## Create a special object that stores a matrix and computes and caches its inverse


## Creates a special "matrix" that can store the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list (set = set, get = get, setInv = setInv, getInv = getInv)

}


## Takes in a "cache matrix", and returns value of its in inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
