## This functions will take advantage of the scoping rules of the R and how they can be manipulated to preserve state inside of an R object.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        invrs <- NULL
        
        set <- function(y) {    # 1
                
                x <<- y
                invrs <<- NULL
        }
        
        get <- function() x  # 2
        
        set.inverse <- function(inverse) {invrs <<- inverse}       # 3
        
        get.inverse <- function() invrs  # 4
        
        list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        invrs <- x$get.inverse()
        
        if(!is.null(invrs)) {
                
                message("getting cached data")
                return(invrs)
        }
        
        data <- x$get()
        invrs <- solve(data, ...)
        
        x$set.inverse(invrs)
        invrs
}
