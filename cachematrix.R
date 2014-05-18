# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL


    set <- function(y) {
        x <<- y
        s <<- NULL
    }

    get <- function() x

    setter <- function(solve) s <<- solve
    # Getter for the inverse
    getter <- function() s

    # Return the matrix with newly defined functions
    list(set = set, get = get, setter = setter, getter = getter)
}


# cacheSolve: Compute the inverse of the matrix.

cacheSolve <- function(x, ...) {

    inv <- x$getter()
    if (!is.null(s)) {


        return(s)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setter(s)
     s
}
