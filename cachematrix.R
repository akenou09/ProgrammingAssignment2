## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function is a cache for the matrix. It:
#sets the value of the matrix
#gets the value of the matrix
#sets the value of the inverse of the matrix
#gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invmat<-NULL
  set<-function(y) {
  x<<-y
  invmat<<-NULL
  }
  get<-function() x
  setinv<-function(inversmat) invmat<<-inversmat
  getinv<-function() invmat
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function
# This function first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverese of the matrix and sets the value of 
#the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat<-x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  thematrix<-x$get()
  invmat<-solve(thematrix,...)
  x$setinv(invmat)
  invmat
}
