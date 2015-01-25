## makeCacheMatrix creates a special matrix object to cache the inverse
## cacheSolve solves the inverse matrix of object from makeCacheMatrix


makeCacheMatrix <- function(x = matrix() ){
        m <- NULL
        set <- function(y) { # set the value of the matrix
                x <<- y 
                m <<- NULL
        }
        get <- function() x # gets the value of the matrix
        set.inverse <- function(solve) m <<- solve # set the value of the inverse matrix
        get.inverse <- function() m # gets the value of the inverse matrix
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}

## cacheSolve solves the inverse matrix of object from makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$get.inverse() # setting m to value of previous inverse matrix
        if(!is.null(m)) {  #if m is NOT a null object, prints "getting cached data")
                message("getting cached data")
                return(m)
        }
        data <- x$get()  # gets the value of the previous matrix
        m <- solve (data, ... ) # solves the inverse of the matrix
        x$set.inverse(m) # sets the solved inverse matrix in makeCacheMatrix
        m
}
