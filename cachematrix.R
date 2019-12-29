## initialize an object of type makeCacheMatrix to create a matrix

makeCacheMatrix <- function(x = matrix()) {
        #create variable for inverse
        inv <- NULL
        #function for setting new matrix in the parent environmemnt
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #function for getting the matrix
        get <- function() x
        
        #functo to set inverse in parent environemnt
        setinv <- function(inverse) inv <<- inverse

        #functon to get the inverse
        getinv <- function() inv
        
        #list containing functions to return to be able to use $
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## function that cache the inverse of the matrix
## and takes object of only type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # returns inverse of matrix as inv
        inv <- x$getinv()
        
        #checks if there is cache or if its NULL
        if(!is.null(inv)) {
                message("getting cashed data")
                return(inv)
        }
        
        # getter function to get thr original matrix
        matrix <- x$get()
        
        # solving the inverse of the matrix if cache is NULL
        inv <- solve(matrix,...)
        
        # sets the inverse
        x$setinv(inv)
        inv
}
