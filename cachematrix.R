## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "vector", which is a list containing functions to
# 1. set the value of the matrix, if the matrix is not a square matrix, report a message
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                if(nrow(y)!=ncol(y)) {
                        message("input matrix must be a square matrix")
                        return(matrix())
                } else {
                        x <<- y
                        inv <<- NULL
                }
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# solve the inverse matrix for the input matrix. it first checks whether
# the inverse matrix has already existed. If so, it gets the inverse
# from the cache. Otherwise, it solves the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
