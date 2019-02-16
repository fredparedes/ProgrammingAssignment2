## These function provide the capability to cache a matrix and the inverse matrix
## for later use. That way it will only have to run solve() once on the matrix if it
## has already been done.

## Function: makeCacheMatrix 
## Description: take in a matrix and cache the value for later. It provides a list of functions to use.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Set m in current scope
        m <- NULL
        
        ## set the value of the matrix in parent scope
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the matrix from the parent scope
        get <- function() x
        
        ## set the inverse of the matrix in the parent scope
        setinverse <- function(inverse) m <<- inverse
        
        ## get the inverse of the matrix from the parent scope
        getinverse <- function() m
        
        ## list of the functions
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function: cacheSolve
## Description: this function will use makeCacheMatrix to see if there is a cached inverse matrix
## if not then it will go ahead and solve the inverse matrix and cache the value.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## if the value is not null then we have a cashed value
        if (!is.null(m)) {
                message("Return cached matrix")
                return(m)
        }
        
        ## get the cached matrix
        data <- x$get()
        
        ## get the inverse of the matrix
        m <- solve(data, ...)
        
        ## cache the value of the matrix inverse
        x$setinverse(m)
        
        ## return the matrix
        m
}
