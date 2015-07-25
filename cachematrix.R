# Coursera R Programming Course
# Week 3 Assignment: Caching the Inverse of a Matrix

#--------------------------------------------------------------------

# Given that matrix inversion is usually a costly computation, there 
# may be some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly. In this assignment, I will write a pair of 
# functions that cache the inverse of a matrix.

# For this assignment, I have assumed that the matrix supplied is
# always invertible.

#--------------------------------------------------------------------

# Function 1 of 2: makeCacheMatrix
# This function creates a special "matrix" object that can cache its 
# inverse. It is really a list containing a function to:

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#--------------------------------------------------------------------

# Function 2 of 2: cacheSolve
# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cacheSolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

#--------------------------------------------------------------------

# Here is some code to test the functions (commented out)

# First, create a square invertible matrix
# x <- matrix(1:4, 2, 2)

# Use it to create the special matrix of get/set functions
# a <- makeCacheMatrix(x)

# Compute the inverse
# cacheSolve(a)

# If we run this function again, now we'll get the inverse from cache
# cacheSolve(a)