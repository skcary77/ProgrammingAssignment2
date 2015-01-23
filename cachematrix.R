## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(iMatrix = matrix()) {
        inverse <- NULL
        set <- function(y) {
                iMatrix <<- y
                inverse <<- NULL
        }
        get <- function() iMatrix
        setinverse <-function(i) inverse <<- i
        getinverse <-function() inverse
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(iMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        solved <- iMatrix$getinverse()
        if(!is.null(solved)){
                #message("getting cached data")
                #return(solved)
                return()
        }
        data <- iMatrix$get()
        solved <- solve(data)
        iMatrix$setinverse(solved)
        #solved       
}
