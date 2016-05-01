#################################################################################
## Week 3 assignment
##     makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##     cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#################################################################################

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        #initializing
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #returning whatever matrix passed in
        get <- function() x       
                
        #for caching
        setInverse <- function (inv) {
            inv <<- inv
        }
        
        #returning inversed matrix
        getInverse <- function() {
            inv
        }        
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        #attemping to fetch inverse matrix of x
        #if anything availble getting from cache        
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        #creating inversed matrix
        mat <- x$get()
        inv <- solve(mat, ...)
        
        #assigning inverse matrix
        x$setInverse(inv)
        inv
}

