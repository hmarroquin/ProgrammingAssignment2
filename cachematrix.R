## R Programming - Assignment 2
## Author: hmarroquin
## Date  : 2014-11-19


## Function to create a "matrix" list object.
## Object will store both the matrix and cache its inverse value for future reference

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize empty inverse
        inverse <- NULL
        
        
        ## Matrix Constructor
        setMatrix <- function(newMatrixParameter){
                x <<- newMatrix   ## New Matrix values
                inverse <<- NULL  ## Reset inverse value
        }
        
        ## Stores Inverse of Matrix
        setInverse <- function(inverseParameter){
                inverse <<- inverseParameter
        }
        
                
        ## Code to pull values of matrix and its inverse
        getMatrix <- function() x
        getInverse <- function() inverse
        
        
        ## Return built "Matrix" list object
        return(list(  getMatrix  = getMatrix
                    , getInverse = getInverse
                    , setMatrix  = setMatrix 
                    , setInverse = setInverse
                   )
               )
}


## Function to solve for the inverse value of a square matrix.
## Work together with makeCacheMatrix in order to avoid redundant computation of same inverse.

cacheSolve <- function(x, ...) {
        
        ## Fetch possible cached inverse.
        cachedInverse <- x$getInverse()
        
        
        if(!is.null(cachedInverse)){
                message("Retrieving previously cached inverse")
                return(cachedInverse)   ## return cached value and exit function
        }
        
        ## Fetch Matrix
        cachedMatrix <- x$getMatrix()
        
        
        ## Calculate inverse
        inverseMatrix <- solve(cachedMatrix, ...)
        
        ## Cache Inversed Matrix for future reference
        x$setInverse(inverseMatrix)
        
        
        
        ## Return a matrix that is the inverse of 'x'
        return(inverseMatrix)
        
}


## UAT:
## Inversible matrix sample : matrix(c(2,2,3,2),2,2)
# > source('C:/Users/hmar/COURSERA/R/ProgrammingAssignment2/cachematrix.R')
# > temp <- makeCacheMatrix(matrix(c(2,2,3,2),2,2))
# > cacheSolve(temp)
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve(temp)
# Retrieving previously cached inverse
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > temp$getMatrix()
# [,1] [,2]
# [1,]    2    3
# [2,]    2    2





