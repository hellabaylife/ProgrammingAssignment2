## The first function, makecachematrix, takes an argument of a matrix
## Then it stores functions that will be used later
## The second function, cahcematrix, takes the matrix created from the first function
## and sees if the inverse has already been stored, if not it solves for the inverse 
## of the matrix, and then stores the solution to memory.


## the function below takes the argument of a matrix, and builds a set on functions that will be
## stored within the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  ##allows you to change argument x without calling a new matrix.
                x <<- y
                m <<- NULL
        }
        get <- function() x  ##retreives argument of x.
        setsolve <- function(solve) m <<- solve  ##sets m to the inverse matrix when function is called.
        getsolve <- function() m ##retrieves value of m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## takes the argument of the matrix you create in makecachematrix
## then it attempts to retrieve the inverese of the matrix if already stored
## if not, it takes the inverse of the matrix you created and stores it to memory

cacheSolve <- function(x, ...) {
                m <- x$getsolve()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
                m
        ## Return a matrix that is the inverse of 'x'
}

##example below

matrix1 <- makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2,ncol = 2))
cacheSolve(matrix1)
cacheSolve(matrix1)
