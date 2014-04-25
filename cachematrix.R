## The makeCacheMatrix function creates a cache matrix object to  
## cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        mtx <- NULL
        set <- function(y) {
                x <<- y
                mtx <<- NULL
        }
        get <- function() x  ## it's a function without arguments that gets x
        setmtx <- function(theMean) mtx <<- theMean ## assigns the 'theMean' value to mtx  
        getmtx <- function() mtx
        list(set = set, get = get,
             setmtx = setmtx,
             getmtx = getmtx)
}


## The  cacheSolve function computes the inverse of the matrix
## returned by makeCacheMatrix. In case the inverse is calculated
## cacheSolve will retrieve the cached matrix (if it is not changed).
If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## x is an object created with makeCacheMatrix
        mtx <- x$getmtx()  ## matrix 'mtx' is either NULL or is indeed cached
        if(!is.null(mtx)) {
                message("getting cached data")
                return(mtx)
        }
        data <- x$get()
        mtx <- solve(data, ...)
        x$setmtx(mtx)
        mtx
}
