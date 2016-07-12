makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


testMatrix <- function(mtrx = matrix()){
getm <- function() mtrx
list(getm = getm)
}



makeCacheMatrix <- function(newMatrix = matrix()){
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

cacheSolve <- function(x, newMatrix = matrix(), ...){
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

x <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
newMatrix <- matrix(1:4, nrow=2, ncol=2)
nullMatrix <- matrix()
diffMatrix <- matrix(4:7, nrow=2, ncol=2)
nullx <- makeCacheMatrix()



cacheSolve(x)
cacheSolve(x,newMatrix)
cacheSolve(x,nullMatrix)
cacheSolve(x,diffMatrix)
cacheSolve(nullx)

