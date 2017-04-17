## This function caches the inverse of a invertible matrix


makeCacheMatrix <- function(x = matrix()) {
  
  mInverse<-NULL
  set <-function(y) {
        x<<-y
        mInverse<<-NULL
  }
  
  get <-function() x
  setInverse<-function(inverse) mInverse <<-inverse
  getInverse-function() mInverse
  list(set=set,get=get,setInverse=setInverse,
      getInverse=getInverse)

}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  mInverse<-x$getInverse()
  if(!is.null(mInverse)) {
      message("getting cached data")
      return(mInverse)
  }
  data<-x$get()
  mInverse<-solve(data)
  x$setInverse(mInverse)
  mInverse
}
