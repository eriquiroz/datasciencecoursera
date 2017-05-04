## Programming Assignment 2:
## Lexical Scoping
## Caching the inverse matrix

## This program takes two functions that cache the inverse of a matrix
## makeCacheMatrix : creates a "matrix" object than can cache its inverse
## cacheSolve: computes the inverse of the "matrix" returned by makeCacheMatrix



## makeCacheMatrix creates a list of functions
## then returns those lists to be used by cacheSolve (inverted matrix)

makeCacheMatrix <- function(x = matrix()) { 
    
    ## initialize the inverse property to NULL and
    ## stores the cached value
    inv_matrix <- NULL
    
    ## create a matrix in the working environment
    set <- function(y){
        x <<- y
        inv_matrix <<- NULL
    }
    
    ## get the matrix and return the matrix 'x'
    get <- function() x
    
    ## set the inverse of the matrix and store in cache
    setinv <- function(inv) inv_matrix <<- inv
    
    ## get the inverse of the matrix from cache
    getinv <- function() inv_matrix
    
    ## return a list of functions to the working environment 
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve will calculate the inverse of the unique matrix
## returned by 'makeCacheMatrix'
## If the matrix is not in cache, it will be created in the working environment
## and it's inverted value will bo stored in cache via the setinv function

cacheSolve <- function(x, ...) {
    
        ## get the inverse of the matrix stored in cache
    inv_matrix <- x$getinv()
    
    ## evaluate if the inverse matrix is on cache
    ## if it exists on cache, it will return it's value
    ## otherwise, it will create the matrix in the working environment
    if(!is.null(inv_matrix)){
        message("getting cached data")
        
        ## show matrix
        return(inv_matrix)
    }
    
    ## calculate the inverse of the matrix
    matrix <- x$get()
    inv_matrix <- solve(matrix, ...)
    
    ## store the inverse of the matrix in cache
    x$setinv(inv_matrix)
    
    ## return inverse of matrix
    inv_matrix
}
