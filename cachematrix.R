# FP: to avoid additional computational time when working in R, when we need recall a function and its values
# many times, it is recommended to cache the values so that when they are required again they can be directly
# called from the cache

# this function makeCacheMatrix creates a list that contains a functions to 
# 1. take a matrix
# 2. return the matrix
# 3. calculate the inverse of such matrix
# 4. return the inverse calculated at step 3

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# the second function here below calculates the inverse of a function (using the first function above)
# but first it checks if the inverse has already been calculated, and if it has been already calculated
# it takes the object from the cache instead of recalculating it again

cachesolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")  #to know you are getting the inverse matrix from the cache
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
