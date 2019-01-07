## this function is supposed to create a special "matrix" and 
## cache the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## in this function the inverse of the special "matrix" cached 
##by the above function is computed

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)## return a matrix that is the inverse of the special "matrix"
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m 
       
}
