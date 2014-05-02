## Coching the inversion of a Matrix

## set the cachevalue of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        #initial inverse matrix
        m <- NULL
        #initial matrix and inverse matrix in cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get matrix from cache
        get <- function() x
        #set inverse matrix in cache
        setinverse <- function(inverseMatrix) m <<-inverseMatrix
        #get inverse matrix from cache
        getinverse <- function() m
        #return the function list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## get the inversion of the matrix
cacheSolve <- function(x, ...) {
        #check whether the inverse matrix exist in cache
        m <- x$getinverse()
        #if exsit, return the cached inverse matrix
        if(!is.null(m)) {
                message("getting cached inverse matrix data")
                return(m)
        }
        #if not exist, calculate the inverse of the matrix
        matrix <- x$get()
        m <- solve(matrix)
        x$setinverse(m)
        m
}
