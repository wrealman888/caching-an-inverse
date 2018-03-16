# Function to create a list from matrix x
makeMat <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # set and get inverse of matrix x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list( set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

# Function to look for inverse of matrix x in cache
cacheinv <- function(x, ...) {
    m <- x$getinv()
    # If inverse already in cache, get it, if not calcuate it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}