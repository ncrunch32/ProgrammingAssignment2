## Coursera R Programming 
## Week 3 Assignment; 12/19/2020

##  The following 2 functions work together to take the inverse of a square
##  non singular matrix repetitively.  Following the two functions are a sequence of 
##  commands that test the functions to ensure that they work.

## This first makeCacheMatrix function that takes a matrix 
## and then can cache the inverse

makeCacheMatrix <- function(x = matrix()) { ## read in the matrix
        inv <- NULL                         ## initially set the inverse to null
        set <- function(y) {                ## create the set function to assign new value of 
                x <<- y                     ## matrix in parent environment 
                inv <<- NULL                ## if a new matrix then reset inv to null            
                }
        get <- function() {x}               ## function to get the matrix       
        setinverse <- function(inverse) {inv <<- inverse}  ## function to set the inverse
        getinverse <- function() {inv}      ## function to get the inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## create list of
                                            ## of the set matrix, get matrix, set inverse, and get inverse
}

##  The cacheSolve function takes matrix defined in the above function, checks to see
##  if the inverse has already been computed and, if not, computes the inverse.  If the inverse
##  is not null, then it returns the cached inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()     ## execute the getinverse function defined in the
                                  ## above function to get the previously computed inverse
                            
        if(!is.null(inv)) {       ## if the inverse is not null then return the inverse from cache
                message("getting cached data")
                return(inv)
        }
        
        ## Otherwise do
        data <- x$get()           ## get the matrix
        inv <- solve(data, ...)   ## compute the inverse
        x$setinverse(inv)         ## set the inverse to inv
        inv                       ## return the inverse
}

## The following statements test to see if the functions defined above compute
## a new inverse or return a cached inverse using two 3x3 matrices

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


