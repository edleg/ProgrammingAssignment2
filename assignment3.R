makeCacheMatrix <- function(x = matrix()) {
        ## A matrix is invertible if it is square with determinant !=0
        # Set inv to NULL
        inv <- NULL
        ## Check if the matrix is squared with determinant not zero
        # Case 1: square matrix with det != 0
        if(ncol(x)==nrow(x) && det(x)!=0){
                set<-function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(solve) inv <<- solve
                getinv <- function() inv
                list(set = set, 
                     get = get,
                     setinv = setinv,
                     getinv = getinv)
        }
        # Case 2: square matrix with det = 0 --< exit with error
        else if(ncol(x)==nrow(x) && det(x)==0){
                message("ERROR: singular matrix")
        }
        # Case 3: not square matrix --> exit with error
        else if(ncol(x)!=nrow(x)){
                message("ERROR: not square matrix")
        }
}

cacheSolve <- function(x) {
        # Getting the inverse matrix
        inv <- x$getinv()
        # Check if the inverse matrix is NOT null and getting it from cache
        if(!is.null(inv)) {
                message("getting cached inverse...")
                return(inv)
        }
        # If the inverse matrix is null, calculate the inverse
        else{
                message("calculating inverse matrix...")
                # Getting matrix
                data <- x$get()
                # Calculating inverse matrix
                inv <- solve(data)
                # Caching inverse matrix for future use
                x$setinv(inv)
                inv
        }
        # Check if the returned matrix is the inverse
        
}