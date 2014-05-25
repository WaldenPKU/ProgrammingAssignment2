## This pair of functions are used to calculate and cache the inverse of a matrix.
## They are used to create a matrix, and cache its inverse.

## The first funciton "makeCacheMatrix" creates a special "matrix".
## Its real class is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function (x = matrix()) {
		inv <- NULL
		set <- function (y) {
				x <<- y
				inv <<- NULL
		}
		get <- function () x
		setinverse <- function (solve) inv <<- solve
		getinverse <- function() inv
		list (set = set, get =get,
				setinverse = setinverse,
				getinverse = getinverse)
}

## The function below calculates the inverse of the matrix, which was created
## with the above funcion. If the inverse has already been calculated, it will 
## get the value from the cache and skips the computation. Otherwise, it will
## calculate the inverse of the data and store the value in the cache by 
## "setinverse" function.

cacheSolve <- function (x, ...) {
		inv <- x$getinverse()
		if(!is.null(inv)) {
		## test if the inverse exist
				message ("getting  cached data")
				return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
		inv
}
