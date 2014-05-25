## Put comments here that give an overall description of what your
## functions do
##These functions takes a matrix and inverse it, 
##store it in another environment and then call it if the  
##inverse is asked for again, rather than slving it again.
##It then stores the inverse in another environment as s.
## Write a short comment describing this function

##The makeCacheMatrix function makes a matrix 
##and stores its inverse in another environment.

makeCacheMatrix<-function(x = matrix()){
        
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function
##checks if the inverse has already been stored as
##s before it carries out the cachesolve function.

cachesolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                print("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)      
        print("getting new data")
        s
}
