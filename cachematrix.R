## These two functions reducing the recalculations of inverse and getting the actual matrix

## functions do

## makeCacheMatrix function is used to store the matrix data which is given by user
##whenever the matrix wants simply calling the get function of that vector

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverse) m <<- inverse
    getInverseMatrix <- function() m
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve function is used to find the inverse of matrix
## And also set this inverse to make cache matrix function vector
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverseMatrix(m)
    m
}
## example
## v=makeCacheMatrix(matrix(c(5,2,6,2),nrow=2,ncol=2,byrow=TRUE))
## v$get()
##     [,1] [,2]
## [1,]    5    2
## [2,]    6    2
## cacheSolve(v)
##      [,1] [,2]
## [1,]   -1  1.0
## [2,]    3 -2.5
## v$getInverseMatrix()
##       [,1] [,2]
## [1,]   -1  1.0
## [2,]    3 -2.5
