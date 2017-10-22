## R Programming
## Week 3
## Programming Assignment 2
## ************************

## Matrix inversion is a costly computation in general. Providing that the content 
## of a matrix is not changing, it is worth caching its inverse.
## The two functions below, on the one hand, cache the inverse of a matrix,
## and, on the other hand, when it is needed again looked up it in the cache 
## rather than recomputed.

## The function makeCacheMatrix below defines 4 different functions:
## 	- setMatrix(): set a matrix;
##	- getMatrix(): get the matrix;
##	- setInverse(): determine the inverse of a matrix;
##	- getInverse(): get the inverse of the given matrix. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      setMatrix <- function(y) {
      	x <<- y
      	inv <<- NULL
      }
	getMatrix <- function() { x }
	setInverse <- function(inverse) { inv <<- inverse }
	getInverse <- function() { inv }
	list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}

## The function cacheSolve below determines the matrix returned by the function 
## makeCacheMatrix.
## If the inverse exists, the function cacheSolve retrieves the inverse from the cache, 
## otherwise this function computes it.
## Throughout the function, it is assumed that the actual matrix is invertible.

cacheSolve <- function(x) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if ( !is.null(inv) ) {
		message("Getting cached inverse.")
		return(inv)
	}
	data <- x$getMatrix()
	inv <- solve(data)
	x$setInverse(inv)
	message("Inverse was computed.")
	inv
}
