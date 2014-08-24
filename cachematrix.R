## The two functions collectively invert a matrix with caching
## First funciton creates a special list that contains functions 
## to 1. set a matrix 2. get a matrix 3. set matrix inverse 4. get matrix inverse 
## the second function looks for the passed matrix in its cache and returns
## its inverse if it has laready been computed. Otherwise, it computes the inverse
## and returns it 

## This function creates a list of four objects that set or get a matrix 
## and set or get its inverse
## This function can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function inverses a matrix using its cache
## If the inverse of a passed amtrix has already been calculated and 
## the passed matrix has not changed, this function looks up its inverse
## from its cache and returns it rather than calculating the inverse again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
