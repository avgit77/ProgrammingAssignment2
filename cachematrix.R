## The following two functions will take a invertible matrix and then compute and cache its inverse.
## This is done in the case of large computationally heavy matrices where it would take a long
## time to compute the inverse over and over again (such as in a for loop). By caching the matrix,
## computation time is significantly sped up.

## makeCacheMatrix creates an object that holds a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse.matrix <<- NULL
    }
    get <- function() x
    SetInverse <- function(inverse) inverse.matrix <<- inverse
    GetInverse <- function() inverse.matrix
    list(set = set, get = get,
         SetInverse = SetInverse,
         GetInverse = GetInverse)
}


## cacheSolve will either retrieve a pre-computed inverse of a matrix, 
## or solve the inverse of the matrix and cache it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse.matrix <- x$GetInverse()
    if(!is.null(inverse.matrix)) {
        message("getting cached data")
        return(inverse.matrix)
    }
    data <- x$get()
    inverse.matrix <- solve(data)
    x$SetInverse(inverse.matrix)
    inverse.matrix
}

# ## Some Test Cases, uncomment to run them
# 
# ## 3 square matrices derived from normal distribution
# test.matrix1 <- matrix(rnorm(25), 5, 5)
# test.matrix2 <- matrix(rnorm(100), 10, 10)
# test.matrix3 <- matrix(rnorm(10000), 100, 100)
# 
# ## Creating the cached matrix
# test.cache.matrix1 <- makeCacheMatrix(test.matrix1)
# test.cache.matrix2 <- makeCacheMatrix(test.matrix2)
# test.cache.matrix3 <- makeCacheMatrix(test.matrix3)
# 
# ## Solving the inverse
# cacheSolve(test.cache.matrix1)
# cacheSolve(test.cache.matrix2)
# cacheSolve(test.cache.matrix3)
# 
# ## Check is the inverse of the test matrix is identical to the solved inverse
# identical(solve(test.matrix1), test.cache.matrix1$GetInverse())
# identical(solve(test.matrix2), test.cache.matrix2$GetInverse())
# identical(solve(test.matrix3), test.cache.matrix3$GetInverse())
