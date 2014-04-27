## The makeCacheMatrix function creates a cache matrix object to  
## cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        mtx <- NULL
        set <- function(y) { ## function set is not actually used but for the sake of the initial example 
                x <<- y      ## we don't delete this function
                mtx <<- NULL
        }
        get <- function() x 
        setmtx <- function(matrix) mtx <<- matrix 
        getmtx <- function() mtx
        list(set = set, get = get,
             setmtx = setmtx,
             getmtx = getmtx)
}


## The  cacheSolve function computes the inverse of the matrix
## returned by makeCacheMatrix. In case the inverse is calculated
## cacheSolve will retrieve the cached matrix (if it is not changed).

cacheSolve <- function(x, ...) { 
        mtx <- x$getmtx() 
        if(!is.null(mtx)) {
                message("getting cached data")
                return(mtx)
        }
        data <- x$get()
        if( ncol(data)==nrow(data)){
        mtx <- solve(data, ...)
        x$setmtx(mtx)
        mtx}
		else message("The cached matrix is not square.\nPlease use again function makeCacheMatrix to cache a square matrix 'NxN'!")
}