## when makeCacheMatrix is assigned to an object, it copies its environment
## so m and x can be stored in memory due to lexical scoping
## m is initially assigned as NULL every time mackeCacheMatrix is called
## if cacheSolve is called on an object created by makeCacheMatrix for the
## first time, m will be NULL and cacheSolve will calculate the inverse
##of x. Then it will call x$setinverse to assign the inverse to m in the
## parent environment. So when the same object (created by makeCacheMatrix)
##is called by cacheSolve again, m will not be NULL and the inverse
## will not be calculated again


## makeCacheMatrix creates a list of functions, and when assigned to
## an object, copies it's environment which stores the values of m and
## x in memory. 
##

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes an object from makeCacheMatrix as an argument
## and checks to see if the value of m is NULL, if it is not,
## cacheSolve will return the value of m that is stored in memory.
## If m is null, cacheSolve uses get to store the matrix in the data,
## then stores the inverse in m. Afterwords, setinverse is used to store
## the new value of m in the parent environment. Finally,
## cacheSolve returns m with the calculated inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
