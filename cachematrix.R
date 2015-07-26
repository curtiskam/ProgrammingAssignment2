## initialize the sorage matrix and create functions

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y)
        
    {
        x <<- y   ## assign y to input matrix
        
        m <<- NULL  ## initialize storage matrix
        
    }
    get <- function() x ## get inout matrix
    
    setinverse <- function(inverse) m <<- inverse  ## store inverse
    
    getinverse <- function() m ## calculate inverse
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## function list
}


## Use solve function to invert matrix if not already done
## if already cached return cache 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()  ## set m to cached matrix
    
    if(!is.null(m)) {  #check if cache is empty 
        
        message("getting cached data")  ## let caller know we are using cache
        
        return(m)  ## return cache
        
    }
    
    ## otherwise calculate inverse
    
    temp <- x$get()  ## otherwise calculate inverse
    
    m <- solve(temp, ...)  ## solve function inverts a square matrix
    
    x$setinverse(m)   ## store inverse matrix in cache 
    
    m  ## return inverse matrix
}

## test calls
## mat <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
## mat2 <- makeCacheMatrix(mat)
## cacheSolve(mat2)

## help found at http://stackoverflow.com/questions/25374803/returning-the-inverse-matrix-from-a-cached-object-in-r-checking-that-input-matri
