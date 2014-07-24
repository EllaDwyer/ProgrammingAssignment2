## The following functions speed up calculations by cacheing a matrix's inverse, then retrieving it,
## rather than calculating it again.

## makeCacheMatrix takes in a matrix and returns a list containing functions to:
## 1) set the value of the matrix, 2) get the value of the matrix,
## 3) set the value of the matrix's inverse, 4) get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL    
    setmatrix <- function(y){
        x <<- y
        I <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inv){
        I <<- inv
    }
    getinverse <- function() I
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse=getinverse)
}


## The following function returns the inverse of the matrix in the special list returned by
## the preceeding `makeCacheMatrix` function, whether or not that inverse has been cached. If the 
##inverse has not yet been cached, it will cache the newly calculated inverse in the special list.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)){
            message("getting cached inverse")
            return(I)
        }
        mx <- x$getmatrix()
        I <- solve(mx, ...)
        x$setinverse(I)
        I
}
