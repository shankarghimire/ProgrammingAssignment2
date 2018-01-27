## Put comments here that give an overall description of what your
## functions do
## here are two functions i.e. 'makeCacheMatrix(x=matrix())' and 'cacheSolve(x,...)
## In the first function, the given matrix is assigned to 'x' object and 'inversmatrix' object is assigned to 'NULL'
## due to the uses of '<<-' operator, both the objects 'x' and 'inversmatrix' are placed in parent environment.
## so that even the second function 'cacheSolve()' can check the values of the objects 'x' and 'invrsmatrix'
## which are initialized to their respective values in first function 'makeCacheMatrix()'
## 
## Write a short comment describing this function
## this function takes 'x' as matrix objecct and in set() function, assigns it to 'x' object and 'invrsmatrix' object to NULL.
## when the first function ends, it return a 'list' object
makeCacheMatrix <- function(x = matrix()) {
    invrsmatrix <- NULL
    set <- function(y) {
        x <<- y
        invrsmatrix <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse){
        invrsmatrix <<- inverse
    }
    getinverse <- function() {
        invrsmatrix
    }
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
## 'invrsmatrix' object gets its value by the function 'x$getinverse()' 
## if the 'invrsmatrix' is  NULL, then the 'if' condition will be false and will use 'solve()' function
## to find the inverse of the matrix
## if the 'invrsmatrix' is not NULL, then the 'if' condition will be true and 
## 'getting cached data' message will be printed and the cached inverse matrix is returned 
## from the parent environment.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrsmatrix <- x$getinverse()
    if(!is.null(invrsmatrix)) {
        message("getting cached data")
        return(invrsmatrix)
    }
    givenmatrix <- x$get()
    invrsmatrix <- solve(givenmatrix, ...)
    x$setinverse(invrsmatrix)
    invrsmatrix
}

