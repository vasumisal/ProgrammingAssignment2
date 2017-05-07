## makeCacheMatrix function creates a special matrix
makeCacheMatrix <- function(x = matrix()) {
        inversem <- NULL
        set <- function(y) {
        x <<- y
        inversem <<- NULL
        }
        get <- function() x
        set_inverse<- function(inverse) inversem <<-inverse
        get_inverse <- function() inversem
        list(set = set, get = get, set_inverse = set_inverse, 
             get_inverse = get_inverse)
}


## cacheSolve function computes the inverse of the special matrix and returns inverted matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversem <- x$get_inverse()
        if (!is.null(inversem)) {
                message("getting cached data")
                return(inversem)
        } else {
                inversem <- solve(x$get())
                x$set_inverse(inversem)
                return(inversem)
        }
}
