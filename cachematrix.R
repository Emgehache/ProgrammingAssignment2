## The following functions are useful to calculate the inverse of a matrix and set this inverse into
## cache, in order to retrieve it faster if needed.

## makeCacheMatrix associates a list of functions to an object. It is then possible to associate a matrix
## to this objet using the function set, to retrieve this matrix using get, to store the inverse of the
## matrix using setinv and to retrieve the inverse using getinv (if this inverse has been stored).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve allows to obtain the inverse matrix stored in an object created with makeCacheMatrix.
## If this inverse matrix has never been calculated, it gets calculated and stored in the object, and
## later calls will then get it directly.
## The message "getting cached data" is returned if the inverse matrix is obtained from storage rather
## than from calculation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}