makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # define set function：set matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # define get function：return matrix
  get <- function() x
  
  # define setinverse function：
  setinverse <- function(inverse) inv <<- inverse
  
  # define getinverse function
  getinverse <- function() inv
  
  # return result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # return cache inverse matrix
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()  # get inverse matrix
  inv <- solve(data, ...)  # calculate inverse matrix
  x$setinverse(inv)  # cache inverse matrix
  
  # return inverse matrix
  inv
}

# use case
mat <- makeCacheMatrix(matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2))

# calculate and make a cache
inverse1 <- cacheSolve(mat)
print(inverse1)

# get result from cache
inverse2 <- cacheSolve(mat)
print(inverse2)  # output "getting cached inverse"