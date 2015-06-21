## Potentially time-consuming computations can be omitted by cashing the
## results and therefore performing the costly calculation only once.

##MakeCacheMatrix creates a special "vector", which is really a list containing
##a function to:
##set the value of the matrix,
##get the value of the matrix,
##set the value of the invers matrix,
##get the value of the invers matrix.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinvers <- function(invers) i <<- invers
        getinvers <- function() i
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)

}


##CashSolve first checks to see if the mean has already been calculated.
##If so, it gets the invers matrix from the cache and skips the computation.
##Otherwise, it calculates the invers of the matrix and sets the value of the
##invers in the cache via the setinvers function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinvers()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(x)
        x$setinvers(i)
        i
}


