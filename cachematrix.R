## Caching implementation for the inverse of a square matrix.
##  These functions allow for the caching of the inverse of a square 
##  square matrix. 
##
##   makeCacheMatrix : Creates an object that supports caching off the
##                     inverse of a square matrix.
##
##   cacheSolve: Solves for the inverse of a square matrix. If the
##               matrix inverse has previously been found and the
##               matrix has not been changed, this function returns 
##               the cached value.
##
## Usage
##     Create a 2x2 matrix with a cacheable inverse
##          a<-makeCacheMatrix(matrix(1:4,2,2))
##
##     Calculate the cacheable inverse of the matrix
##          aInverse<-cacheSolve(a)
##
##     Set cache matrix to new value
##          a$set(matrix(c(1,2,2,1),2,2))
##
##     return cached matrix as class "matrix"
##          b<-a$get()
##
##
## Create a cacheable inverse matrix object
##   x - matrix to be cached
##
##  returns: a cacheable inverse matrix object
makeCacheMatrix <- function(x = matrix()) {
    #initialize inverse variable
    inverse <- NULL
    
    #define set,get,getinverse,setinverse functions
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(matrixInverse) inverse <<- matrixInverse
    
    getinverse <- function() inverse
    
    #return cached matrix function list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Solve for matrix inverse and cache inverse for later use
##   x - cacheable matrix object created with makeCacheMatrix
##   ... - additional parameters to be passed to R solve().
##
## returns: the inverse of the matrix as class "matrix"
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse<- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
