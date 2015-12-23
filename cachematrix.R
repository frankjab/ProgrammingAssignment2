# Overall description of makeCacheMatrix and cacheSolve
# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

# makeCacheMatrix creates an enclosed environment with variables, y = matrix and m = an empty variable
# makeCacheMatrix returns the 4 functions into the global environment as a list
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # Initialize m
        # define the 4 functions (set, get, setinverse, getinverse)
        set <- function(y){
                x <<- y  # sets y equal to the matrix x
                m <<- NULL  # m, an empty variable
        }
        get <- function() x  # when called the matrix x is returned
        set.solve <- function(solve) m <<- solve
        get.solve <- function() m  # when called m is returned (e.g. NULL until cacheSolve is called)
        list(set = set, get = get,
             set.solve = set.solve,
             get.solve = get.solve)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
        # On the  first run, cacheSolve finds m empty and goes to the else instruction, calculates m and returns it
        # On subsequent runs, m is no longer Null, so m is returned via m <- x$getinverse())
        m <- x$get.solve()
        # If m is not empty return m, its been calculated already
        if(!is.null(m)){
                message("")
                return(m)
        }
        # else inverse the matrix and return
        data <- x$get()  # get the matrix
        m <- solve(data, ...)  # inverse the matrix
        x$set.solve(m)  #set 
        m # return inverse matrix
}