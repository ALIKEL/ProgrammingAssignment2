#The following function allows the user to set and get the matrix
# It stores the matrix in temporary storage to allow for caching later on
makeCachematrix <- function(x = matrix()) {  
        m <- NULL							 
        set <- function(y) {                 
                x <<- y					     
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
# The following function checks whether the matrix has changed. 
# If it hasn't then it prints the message "getting cached data" 
# and prints its inverse. If it has changed then it calculates the inverse
# and prints it.
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {                         
		message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)            
        x$setmatrix(m)
        m
}
