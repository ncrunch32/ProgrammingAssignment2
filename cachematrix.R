## Coursera R Programming 
## Week 3 Assignment; 12/19/2020

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL                         
        set <- function(y) {               
                x <<- y                             
                inv <<- NULL                            }
        get <- function() {x}                      
        setinverse <- function(inverse) {inv <<- inverse}  
        getinverse <- function() {inv}            
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Testing

mat<-matrix(c(1,4,5,6,7,8,2,3,5),nrow=3,ncol=3)
wmat<-makeCacheMatrix(mat)
wmat$get()
wmat$getinverse()
cacheSolve(wmat)

wmat$getinverse()
cacheSolve(wmat)

mat<-matrix(c(20,10,15,17,13,18,20,35,30),nrow=3,ncol=3)
wmat<-makeCacheMatrix(mat)
wmat$get()
wmat$getinverse()
cacheSolve(wmat)

wmat$getinverse()
cacheSolve(wmat)


