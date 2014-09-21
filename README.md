## For R programming assignment 2

##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
    
    iv <- matrix()       
    
    ## set matrix by y if a y is defined in environment
   
    set <- function(y) {
        x <<- y
        iv <<- matrix()
     }
    
    get <- function() x
    setinverse <- function() {
        iv <<- solve(x)
    }
    
    getinverse <- function() iv
    
    ## output
    list(set = set(), get = get(),
         setinverse = setinverse(),
         getinverse = getinverse())
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x = list()){
   
    iv <-x$getinverse()
    
    if(!is.na(iv)){
        message("getting cashed data")
        return(iv)
    }
    
    data <- x$get()
    iv <- solve(data)
}
