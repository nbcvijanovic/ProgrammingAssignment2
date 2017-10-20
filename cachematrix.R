## This function creates the special matrix object (actually a list) containing functions to 
## set and get its values as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
        # initilize the inverse of the special matrix object x - if it's NULL we compute it,
        # otherwise we retrieve it from memory
        inv <- NULL
        # function for setting the special matrix object as in example
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # gets the special matric object like in the example
        get <- function() x
        # set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        # get the inverse of the matrix
        getinverse <- function() inv
        # create (return) list containing all functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function returns the inverse of our special matrix object.
## However, it only calculates the inverse in case it has not been
## calculated before. Otherwise, it retrieves the result from the cache

cacheSolve <- function(x, ...) {
        # uses the get function to retrieve the inverse
        inv <- x$getinverse()
        # if the returned value is NULL we need to compute the inverse.
        # if it's not, the inverse has been computed before and we retrieve it from cache
        if(!is.null(inv)) {
                message("Getting cached inverse value")
                return(inv)
        }
        # inverse not in cache so we calculate it
        # get the matrix values (remember, x is a list)
        data <- x$get()
        # calculate
        inv <- solve(data)
        # set it so we don't have top repeat the calculation
        x$setinverse(inv)
        inv
}
