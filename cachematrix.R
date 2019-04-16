## Put comments here that give an overall description of what your
## functions do

## The "makeCacheMatrix" function contains 4 functions in it

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## The "set" function allows us to assign matrix into x.
        ## Variable y is supposed to be the matrix we input.
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        ## The "get" function just print out the matrix, stored in "x".
        get <- function()x
        ## The "setInverse" function stores the inverse of the matrix to "i". 
        setInverse <- function(inverse) i<<- inverse
        ## The "getInverse" function just print the inversed matrix "i" out.
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function 1) ask if the matrix has been solved before, and 
## 2) if not, solve the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse() 
        ## If there has been an "i" for the matrix, find the "i" instead of calc again.
        if (!is.null(i)){
                message("Getting cache data...")
                return(i)
        }
        ## Otherwise, assign the matrix "x" into "data".
        data <- x$get()
        ## Inverse the matrix, and assign the inversed into "i".
        i <- solve(data,...)
        ## Assign the new "i" into the previous "i". 
        x$setInverse(i)
        i
}
my_matrix <- makeCacheMatrix((matrix(1:4,2,2)))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
