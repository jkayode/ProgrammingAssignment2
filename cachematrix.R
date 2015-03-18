## This function makes x the value of a square invertible matrix  
## that returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##  This list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
                
        inv = NULL
        set = function(y) {
                
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## To Return a matrix that is the inverse of 'x'
## This function returns the inverse of the original matrix input 
## to makeCacheMatrix()

cacheSolve <- function(x, ...) {
       
        inv = x$getinv()
        
        # if the inverse has already been computed
        # get it from the cache and skip the computation
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculate the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # set the value of the inverse in the cache using the setinv function.
        x$setinv(inv)
        
        # return the value of the inverse 
        return(inv)
}
