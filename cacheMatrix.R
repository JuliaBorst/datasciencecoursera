##The following function creates makeCacheMatrix special "matrix" object that can cache its inverse.
## The first function, makeVector creates a special "vector", which is really a list containing a function to
##1. set the value of the vector, 2. get the value of the vector, 3. set the value of the mean 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  ## creates the function makeCacheMatrix that takes the matrix as input
    
    #Set the value of a vector and get the value of the vector
    m <- NULL
    set <- function(y) {
      x <<-y 
      m <<-NULL
    }
    
    get <- function() x
    #set the inverse of the matrix and get the inverse of the matrix
    setinv <-function(solve) m<<-solve
    
    #get the matrix
    getinv <-function() m
    
    #create and return a list with the 4 functions set, get, setinv and getinv
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
    
    
  }
  
  
  #This function computes the inverse of the special "matrix" returned by 
  # makeCacheMatrix above. If the inverse has already been calculated (and the matrix
  #has not changed), then the cachesolve should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv
    
    #If inverse is already calculated and thus in cache return it
    if(!is.null(m)){
      message("getting cached data")
      return(m)    
    }
    
    
    #If data isn't there calculate the inverse
    
    data <-x$get()
    m<-solve(data, ...)
    x$setinv(m)
    m #return result
    
  }
  



