## [Put comments here that describe what your functions do]
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function takes a matrix as an argument to makeCacheMatrix function and stores it into memory,
#cacheSolve takes as an argument variable that was created using makeCacheMatrix function and 
#either calculates inverse of it if it hasn't been done before and stores it, or returns 
#previously calculated inverse value stored with help of makeCacheMatrix function. The goal is 
#to reduce time and memory for calculating inverse of a matrix object. 

makeCacheMatrix <- function(x = matrix()) {
    inversM <- NULL
    set <- function(y){
        x <<- y
        inversM <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inversM <<- solve
    getinverse <- function() inversM
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversM <- x$getinverse()
    if(!is.null(inversM)){
        message('getting cached data')
        return(inversM)
    }
    data <- x$get()
    inversM <- solve(data, ...)
    x$setinverse(inversM)
    inversM
}


