# create a caching vector function that accepts a matrix as its only argument and returns a list of caching functions.
makeCacheMatrix <- function(x = matrix()) {
		# initialize local variable 'inv' to NULL
        inv <- NULL
        		# check if the argument passed is a matrix or not. 
        		if (!is.matrix(x))
        		{
        		  # if argument entered is not matrix then display message asking user to enter a matrix argument
        		  print ("Enter Matrix Object as argument")
        		  # return null and end execution here since argument entered is not matrix
        		  return(NULL)
        		}
        # create a function called set that allows to set/accept the matrix value that the user passes to function and cache/store in 'x'. 
        set <- function(y) {
        		# check if the argument passed while trying to 'set' is a matrix or not. 
        		if (!is.matrix(y))
        		{
        		  # if argument entered is not matrix then display message asking user to enter a matrix argument	
        		  print ("Enter Matrix Object as argument")
        		  # return null and end execution here since argument entered is not matrix
        		  return(NULL)
        		}
          # assign the matrix that user entered to the object 'x'.
          x <<- y
          # reset the vector 'inv' as NULL since a new matrix has been set.
          inv <<- NULL
        }
        # will print the cached matrix in 'x'.
        get <- function() x
        # create a function to assign an inverse matrix to the object 'inv'.
        setinverse <- function(inverse) inv <<- inverse
        # will print the cached inversed matrix
        getinverse <- function() inv
        # return (since its the last block of code) a list of the functions set, get, setinverse, getinverse 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



# create a function to compute and return the inverse of the cached matrix
cacheSolve <- function(x, ...) {
		# read if there is already a cached inverse matrix in object x.
        inv <- x$getinverse()
        # if there is a cached inverse matrix in x then return the same.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # read the cached matrix from object x and assign to object data.
        data <- x$get()
        # compute the inverse of matrix stored in object 'data' and assign to object 'inv'.
        inv <- solve(data, ...)
        # set the inverse matrix 'inv' in to the object x.
        x$setinverse(inv)
        # return the object 'inv'
        inv
}
