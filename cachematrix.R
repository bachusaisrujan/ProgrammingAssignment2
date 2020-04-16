## We are trying to fetch the inverse of the matrix with the two functions
## One function is responsible for caching the data and the other responsible for fetching the cache or calculating the value.

## This function will create an object that can cache its inverse
    
    makeCacheMatrix <- function(dat = matrix()) { ## Since the default argument type is default we define it that way
           temp <- NULL                             
           set <- function(temp1) {                    ## define the set function to assign new 
            dat <<- temp1                             ## assigning to the value in the parent environment
            temp <<- NULL                        ## resetting back the temporary to NULL value
        }
        get <- function() dat                     
        
        settheinverse <- function(inverse) temp <<- inverse  ## assigning the value of inverse to the parent environment value
        
        gettheinverse <- function() temp                     ## gets the value of inv where called
        
        list(set = set, get = get, settheinverse = settheinverse, gettheinverse = gettheinverse)  ## setting the order for $operator
    }
    
    
  
    ## The return from the makeCacheMatrix is sent here to calculate the inverse of the special "matrix".
    ## If the inverse was previously calculated then, the cache value if fetched
    ## or else we call the solve function to fetch the inverse
    
    cacheSolve <- function(dat1, ...) {
     
        temp <- dat1$gettheinverse()
        
        if(!is.null(temp)) {
            message("getting the cached value")
            return(temp)
        }
        
        value <- dat1$get()
        
        temp <- solve(value, ...)
        dat1$settheinverse(temp)
        temp
        
    }
