## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

m <- NULL ## sets the value of m to NULL in case cacheSolve hasn't been used yet
set = function (y) {
x <<- y
m <<- NULL
}

setmatrix <- function(y) { ## set value of the matrix
getmatrix <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function () m

list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse=setinverse, getinverse = getinverse) ## creats list of the 4 functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse() #return if inverse has already been calculated
        
        if(!isnull(m)) { ## check to see if cacheSolve already calculated inverse
        message (getting cached data")
        return(m)
        }
        
#if not already calculated
y <- x$getmatrix() ## get the value of the input matrix

x$setmatrix(y) ## run setmatrix on input matrix to cache it

m <- solve(y, ...) ## compute the value of the inverse matrix

x$setinverse(m) ## run the setinverse function on the inverse to cache the inverse

m ## return the inverse

}
