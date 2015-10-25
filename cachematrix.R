## This R function chaches the inverse of a Matrix
## We will use the function solve() to calculate the inverse
## of a matrix

## We will use the R laguage's scoping rules for chaching the results

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
  
        ### Let us set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
  
        ### Now let us get the value of the matrix
        get <- function() x
  
        ### Now let us set the value of the matrix in the inverse order
        setsolve <- function(solve) m <<- solve
  
        ### Now let us get the inverse of the matrix
        getsolve <- function() m
  
        ### Now let us return the values
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)

}


## The below function first checks whether the oinverse is already cached
## If the inverse is already cahched then it reuses the same result
## If the result is not already cached, then it will calculate and then cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() # here we are retrieving from the cache
  
        if(!is.null(m)) #this means we have some value in the cache
                {      
                message("getting the cached data")
                return(m)
        }
    
        # The cache is not filled with any value
        data <- x$get()         # get the value
        m <- solve(data, ...)  # inverse it
        x$setsolve(m)          # set the value
        m                       # return the value
  
  
}
