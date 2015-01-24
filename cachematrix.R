## The following two functions work together to allow the user to create a matrix,
## store the inverse of that matrix in the cache, and then retrieve the inverse from the cache
## when necessary.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(iMatrix = matrix()) {
        inverse <- NULL
        #calling makeCacheMatrix$set(y) allows you to change the matrix without excuting
        #the entire function. It also clears the inverse from the cache
        set <- function(y) {
                iMatrix <<- y
                inverse <<- NULL
        }
        get <- function() iMatrix #returns the matrix
        setinverse <-function(inv) inverse <<- inv #takes the inverse and stores to cache
        getinverse <-function() inverse #returns the inverse
        #returns a list of the functions described above
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated, it is retrieved from the cache

cacheSolve <- function(iMatrix, ...) {
        ## pulls the inverse from makeCacheMartix
        solved <- iMatrix$getinverse()
        ## if it is not null, then return it
        if(!is.null(solved)){
                #message("getting cached data")
                #return(solved)
                return()
        }
        #otherwise, comput the inverse, store it to the cache, then return it
        data <- iMatrix$get()
        solved <- solve(data)
        iMatrix$setinverse(solved)
        #solved       
}
