## Matrix inversion is usually a costly computation and there may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly. Below are two functions that are used 
## to create a special object that stores a matrix and caches its inverse.


## Creates a special "matrix" which is a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of the special "matrix" created with the above function. 
## If the inverse has already been calculated, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse and sets the value 
## of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}


#-------------------------------------------------
# Testing

# > mat <- makeCacheMatrix(matrix(1:4, 2, 2))
# > mat$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > mat$getInverse()
# NULL
# > cacheSolve(mat)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mat)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > mat$getInverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > mat$set(matrix(5:8, 2, 2))
# > mat$get()
# [,1] [,2]
# [1,]    5    7
# [2,]    6    8
# > mat$getInverse()
# NULL
# > cacheSolve(mat)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(mat)
# getting cached data
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > mat$getInverse()
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5