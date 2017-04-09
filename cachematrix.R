#computes the inverse of a matrix, but first it checks if any cached solution exists

makeCacheMatrix <- function(x = matrix()) {
        #this function creates a special vector for keeping calculated inverse of a matrix
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
        #this function checks if the solution for the input matrix exists in cache
        #if not, it completes the calculation and stores the inverse of the current input matrix in cache and print the result
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        my_mat <- x$get()
        s <- solve(my_mat, ...)
        x$setsolve(s)
        s
}
