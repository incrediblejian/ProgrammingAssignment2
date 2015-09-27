## R Programming HW2: Caching the Inverse of a Matrix
## Date: 20150925

## makeCacheMatrix creates a special "vector" contains below functions
## 1: set value of matrix
## 2: get value of matrix
## 3: set value of its inverse
## 4: get value of its inverse
## only update result when matrix value changed
makeCacheMatrix <- function(cachedMatrix = matrix()) {
	cachedInverse <- NULL
	set <- function(input) {
		if(!identical(input,cachedMatrix)) {
			message("Update cached matrix value")			
			cachedMatrix <<- input
			cachedInverse <<- NULL		
		}				
	}
	get <- function() cachedMatrix
	setInverse <- function(inverse) cachedInverse <<- inverse
	getInverse <- function() cachedInverse 
	
	list(set = set, get = get, setInverse = setInverse , getInverse = getInverse)
}

## cacheSolve uses lazy calculation to find the inverse
## Input is a vector returned by makeCachedMatrix function
## It checks cached inverse first and update this value only if needed
cacheSolve <- function(x, ...) {
	myInverse <- x$getInverse()	
	if(!is.null(myInverse)) {
		message("Getting cashed inverse value")
		return(myInverse)
	}
	
	data <- x$get()
	myInverse <- solve(data, ...)	
	x$setInverse(myInverse)		
	myInverse
}

##
## Test
##
## m1 = rbind(c(1, -1/4), c(-1/4, 1))
## m2 = rbind(c(1, -1/8), c(-1/8, 1))
## m3 = rbind(c(1, -1/4), c(-1/4, 1))

#### calculate inverse
## obj = makeCacheMatrix(m1)
## invm1 = cacheSolve(obj)
## m1%*%invm1

#### use cached value for same matrix value
## obj$set(m3)
## invm3 = cacheSolve(obj)
## m3%*%invm3

#### update result due to differnet matrix value
## obj$set(m2)
## invm2 = cacheSolve(obj)
## m2%*%invm2

