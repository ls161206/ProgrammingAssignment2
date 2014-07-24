## In this file, we have two functions that together can calculate, cache, and 
## retrieve the inverse of an invertible matrix.  As mentioned in the 
## instructions, this could be useful since it can take a significant amount
## of time to calculate the inverse of a large matrix.

## The first function is very similar to the function cacheVector in the 
## example code. The input is an invertible matrix, and makeCacheMatrix returns
## a list of four functions which do the following:
## 1. Set the value of a matrix (setmat)
## 2. Retrieve the value of matrix (getmat)
## 3. Set the value of the inverse matrix (setinv)
## 4. Retrieve the value of the inverse matrix (getinv)

## Note that this function does not actually calculate the inverse! That is done
## with the second function below.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmat <- function() x
        setinv <- function(dummy.variable) inv <<- dummy.variable 
        getinv <- function() inv
        list(setmat = setmat, getmat = getmat,
             setinv = setinv,
             getinv = getinv)
}


## This function is similar to the function cachemean in the example code.
## The input is a list of the form created by the makeCacheMatrix function above.
## If the inverse matrix has already been cached, this function displays the 
## message "getting cached data" and then returns the inverse.
## If the inverse has not already been cached, it calculates and caches the 
## inverse, and then returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getmat()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
