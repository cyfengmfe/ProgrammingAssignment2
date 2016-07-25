## The function below cached the inverse of a matrix to save time for matrix inversion calculation 

## description of my function

## Cachematrix.R function contain two functions, makeCacheMatrix() and cacheSolve()
## makeCacheMatrix() creates an R object that stores a matrix and its inverse.



makeCacheMatrix <- function(x = matrix()) {
	## initialization inverse matrix X
	invmatx <- NULL
	
	## Assign the input matrix x in the parent environment
	## Assign the value of NULL to inverse matrix x in the parent environment
        set <- function(maty) {
                x <<- maty
                invmatx <<- NULL
        }
        
        ## get the matrix of x
        get <- function() x
        
        ## assign the inverse matrix inv in the parent environment
        setinv <- function(inv) invmatx <<- inv
        
        ## get the inverse matrix 
        getinv <- function() invmatx
        
        ## assigns each of these functions as element within a list(), and returns it to the parent environment
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve() requires inputting a matrix from makeCacheMatrix() either retrieve the inverse from 
## the cached value, or calculate the cached value then ...

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invmatx <- x$getinv()
	
	## if inverse is already calculated, get cached data directly and return
        if(!is.null(invmatx)) {
                message("getting cached data")
                return(invmatx)
        }
        
        ## if inverse matrix hasn't been calculated, calculate and save
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}