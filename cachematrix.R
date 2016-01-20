## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This set of functions create the inverse of a matrix if it doesn't already exist, if it exists it doesn't create it.
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## testing that it works
testCache <- function(s)
{
  # Create a s x s invertible matrix
  m <- matrix(rnorm(s * s), s, s)
  
  # Create the cacheable matrix object
  a <- makeCacheMatrix(m)

  # Calculate the matrix inverse
  ptm <- proc.time()
  u <- cacheSolve(a)
  print(u)
  t1 <- proc.time() - ptm
  cat("Time to calculate the inverse: ", t1[1], "\n")
  
  # Fetch the matrix inverse 
  ptm <- proc.time()
  cacheSolve(a)
  t2 <- proc.time() - ptm
  cat("Time to fetch matrix inverse from cache: ", t2[1], "\n")
}
testCache(200)
testCache(600)