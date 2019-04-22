## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                ## Used <<- to assign the value that is
                ## not from current environment
                ## x represents an invertible matrix
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) inv <<- Inverse
        getInverse <- function() inv
        
                ## 4 list includes...
                ## 1. setting the matrix 2. getting the matrix
                ## 3. setting the inverse of matrix
                ## 4. getting the inverse of matrix
        
        list (set = set, get = get, 
              setInverse = setInverse,
              getInverse = getInverse)
              
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv = x$getInverse()
        
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
                ## if there is an invserse 
                ## then skip and get from the cache
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
                ## calculate the inverse
                
        x$setInverse(inv)
                ## setting the value of inverse by using setInvserse function
        return(inv)
}
