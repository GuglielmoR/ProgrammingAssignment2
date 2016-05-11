##Coursera R programming. Assignment 3.
#The two functions required for the assignment are below.
#The two functions can simplify time consuming computations.

#The first function (makeCahceMatrix) creates a matrix object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<-solve
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#The second function computes the inverse of the matrix created by makeCacheMatrix.
#If the inverse has already been calculated and the matrix has not changed, 
#then the cachesolve function retrieves the inverse from the cache.
cacheSolve<-function(x,...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}

