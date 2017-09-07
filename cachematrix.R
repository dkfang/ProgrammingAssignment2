## My solution
#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i_m <- NULL
      set <- function(y){
            x <<- y
            i_m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i_m <<- inverse
      getInverse <- function() i_m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse
      )
}

##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse value has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. Otherwise,
##it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i_m <- x$getInverse()
      if(!is.null(i_m)){
            message("getting cached data")
            return(i_m)
      }
      data <- x$get()
      i_m <- solve(data, ...)
      x$setInverse(i_m)
}

## Testing my functions
a<- matrix(c(2,7,3,8), ncol = 2, nrow = 2, byrow = T)
#solve(a)
my_matrix <- makeCacheMatrix(a)
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
my_matrix$getInverse()
## 
a<- matrix(c(2,6,2,9), ncol = 2, nrow = 2, byrow = T)
my_matrix <- makeCacheMatrix(a)
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
my_matrix$getInverse()
