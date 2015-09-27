## The functions together provide computing and caching of inverse of a square matrix.
## A special matrix object is created (which is actually a list). This object caches
## the inverse of the square matrix, and returns the cached value when queried for.


## The function makeCacheMatrix takes matrix as an input
## It returns a list of functions on the square matrix
## The functions in the list are:
##         set the value of a matrix
##         get the value of the matrix
##         set the inverse of the matrix
##         get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

## when creating a new list object using the makeCacheMatrix funtion,
## initialize the inverse to NULL

        inv <- NULL


## Assign the matrix to the internal object
## reset the inverse to NULL (clear the cached value)

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }


## Fetch the matrix value

        get <- function() x


## Cache the inverse of the matrix

        setinverse <- function(inverse) inv <<- inverse


## Return the cached inverse of the matrix

        getinverse <- function() inv


## Return the list of functions on the matrix

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## The function cacheSolve takes the list object created
## in function makeCacheMatrix as input
## It returns the inverse of the matrix
## If the inverse is cached, the cached value is returned
## Else the matrix inverse is calculated and set, and the same is returned
 
cacheSolve <- function(x, ...) {

## Query the inverse of matrix, using the function defined in makeCacheMatrix

        inverse <- x$getinverse()

## If the inverse is cached, return the cached value

        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

## If the inverse is not cached (cache value is NULL), calculate the inverse

        data <- x$get()
        inverse <- solve(data, ...)

## Set the calculated inverse

        x$setinverse(inverse)

## Return the inverse

        inverse
}
