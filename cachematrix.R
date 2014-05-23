## Programming Assignment 2 : Lexical Scoping

# makeCacheMatrix create a matrix and store it

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) {    
                x <<- y         # stores the matrix into a variable called x
                m <<- NULL      # clean the cache
        }
        get <- function() x     # return the matrix stored in x
        setmatrix <- function(matrix) m <<- matrix      # save something into the cache m
        getmatrix <- function() m       # return the cache
        list(set = set, get = get,      # the makeCacheMatrix is a list returning the different functions
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


# cacheSolve compute the inverse of a matrix and store it

cacheSolve <- function(x, ...){
        m <- x$getmatrix()      
        if(!is.null(m)) {                      # if the cache is non-empty
                message("getting cached data") # show this message then ...
                return(m)                      # ... return the cache
        }
        data <- x$get()                        # take the matrix from makeCacheMatrix
        m <- solve(data, ...)                  # return its inverse
        x$setmatrix(m)                         # save it into the cache
        m                                      # return the cache
}



