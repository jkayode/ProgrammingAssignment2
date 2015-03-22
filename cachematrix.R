## This function makes x the value of a square invertible matrix that returns a list 
## containing functions to set the matrix, get the inverse, set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## function to set the matrix                
        inv = NULL
        set = function(y) {
                
                x <<- y
                inv <<- NULL
        }
        
        ## function to get the matrix
        get = function() x
        
        ## function to set the inverse
        setinv = function(inverse) inv <<- inverse 
        
        ## function to get the inverse
        getinv = function() inv
        
        ##  Return the list of functions to be used as the input to cacheSolve()
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## To Return a matrix that is the inverse of 'x'
## This function returns the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
        ## get the inverse
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
