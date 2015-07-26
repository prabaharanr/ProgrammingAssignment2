## 
##

## Returns a special matrix which has capability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y = matrix()){
              x <<- y
            inv <<- NULL
        }
        get <- function() x
        
        setInv <- function(newInv){
            inv <<- newInv
        }
        getInv <- function() inv
        
        list(set = set,get = get,
             setInv = setInv,getInv = getInv)
}


##returns the inverse of input matrix
##Gets the inverse from cache if its already calculated 
##otherwise calculates the inverse and update the cache 


cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        
        if(!is.null(inv)){
            message("Getting cached data")
            return(inv)
        }
        
        data <- x$get()
        inv  <- solve(data)
        x$setInv(inv)
        
        inv
}
