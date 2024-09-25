## Module 3 Peer Review Assignment

## This function creates a matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize two objects, x and m1
  m1 <- NULL 
  #assign input argument to x object in the parent environment and value of NULL to m1, thereby resetting both values
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  #Retrieve x value from parent environment
  get <- function() x
  #setinverse defined as solve function then assign input to m1
  setinverse <- function(solve) m1 <<- solve
  #use lexical scoping to find m1 and retrieve its value
  getinverse <- function() m1
  #create a list, naming each element so that they can be extracted in later function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  #calls getinverse from the list
  m1 <- x$getinverse()
  #check if m1 is null
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  #retrieve the value of x and assign get element to data
  data <- x$get()
  #run inverse function on data and storing as m1
  m1 <- solve(data, ...)
  #uses setmean function on the m1 
  x$setinverse(m1)
  #return value of m1
  m1
}
#create test matrix
m_test <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#run test matrix through first function and store list as m_test1
m_test1 <- makeCacheMatrix(m_test)
#run list through function
n1 <- cacheSolve(m_test1)
#check if n1 is inverse of m_test
m_test %*% n1

