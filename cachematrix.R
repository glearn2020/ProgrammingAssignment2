## This file contains two functions to find inverse of matrix
## function makeCacheMatrix returns a list with original matrix and its inverse (stored in cache)
## function cacheSolve returns the inverse of a matrix, if the inverse is already computed and available in cache (makeCacheMatrix),
## this function will not re compute, if the inverse is not available in cache or the supplied matrix is different, then inverse will be computed

## function to cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		setMatrix <- function(mat = matrix()){
			newMatrix <<- mat
			inv <<- NULL		
		}
		getMatrix <- function() newMatrix
		setInverse <- function(newinv) inv <<- newinv
		getInverse <- function() inv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)		
}


## function to check if inverse exists in cache, if so return it, else compute inverse
## this function takes two arguments, one a list out of makeCacheMatrix and second a matrix
## if the matrix supplied is the same as the one supplied to makeCacheMatrix then will return inverse of 
## matrix (from cache)

cacheSolve <- function(x, newMatrix = matrix(), ...){
		## Return a matrix that is the inverse of 'x'
		invMatrix <- x$getInverse()
		currMatrix <- x$getMatrix()
		

		if (all(is.na(newMatrix)) & all(is.na(currMatrix))) {
				message("Matrices are empty. Returning Null")
				return(invMatrix)
		}
		else if (all(is.na(newMatrix))){
				message("New Matrix supplied is empty")
				newMatrix <- currMatrix
		}
		else {
			message("")
		}

		if(!is.null(invMatrix) & identical(currMatrix, newMatrix)){
				message("Getting cached Inverse")
				return(invMatrix)
		}
		else if(!identical(currMatrix, newMatrix)){
				message("Matrices are different. Calculating Inverse")
				x$setMatrix(newMatrix)
				invmtrx <- solve(newMatrix)
				x$setInvers(invmtrx)
				return(invmtrx)
		}
		else {
				if (is.null(invMatrix)) {
					message("Inverse is null. Calculating..")
					invmtrx <- solve(currMatrix)
					x$setInvers(invmtrx)
					return(invmtrx)
				}
				else {
					message("Getting cached Inverse")
					return(invMatrix)
				}
		}

}
