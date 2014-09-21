makeCacheMatrix <- function(x = matrix()){
    iv <- matrix()       
    
    set <- function(y) {
        x <<- y
        iv <<- matrix()
     }
    
    get <- function() x
    setinverse <- function() {
        iv <<- solve(x)
    }
    
    getinverse <- function() iv
    
    list(set = set(), get = get(),
         setinverse = setinverse(),
         getinverse = getinverse())
}


cacheSolve <- function(x = list()){
    iv <-x$getinverse()
    if(!is.na(iv)){
        message("getting cashed data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data)
}
