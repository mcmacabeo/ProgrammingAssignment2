## R Programming Assignment 2: lexical scoping
## The purpose of this assignment is to learn how to write an executable program 
## that will be able to store a function that is useful when dealing with 
## large amount of dataset.   

## In this case, the function will create a matrix that can cache its inverse.
## 1. set the matrix; 2. get the matrix
## 3. set the inverse; 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                set.inverse <- function(solve.matrix) i <<- solve.matrix
                get.inverse <- function() i
                list(set = set, get = get,
                     set.inverse = set.inverse, get.inverse = get.inverse)
}


## This is a "closure" function that is usually paired with "<<-". 
## The purpose to learn how to program the function to access the defined 
## free variable/parameter in the enclosed enviroment prior to 
## looking into the global environment. It's like a check and balance.
## In this case, it returns the inverse of the matrix that is 
## returned from makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        i <- x$get.inverse()
        if(!is.null(i)) {
                message("getting cache data")
                return(i)
        }
        ## Return a matrix that is the inverse of 'x'
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$set.inverse(i)
        return(i)
}