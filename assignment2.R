# Function to create a special "matrix" object that can cache its inverse.

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    inver <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) inver <<- inverse
  
  getinverse <- function() inver
  
  list(set=set,                      # set the value of the martix
       
       get=get,                      # get the value of the matrix
       
       setinverse=setinverse,        # set the value of the inverse
       
       getinverse=getinverse)        # get the value of the inverse
}

 
# function to compute the inverse of the special "matrix" returned by makeCacheMatrix above. 

# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.   



cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
  
      message("getting cached data")
    
    return(m)
  }
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  
  m
}

