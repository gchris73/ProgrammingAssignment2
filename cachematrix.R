## Put comments here that give an overall description of what your
## functions do
## one function makeCacheMatrix returns a list of functions
## to that return or set objects that store the matrix and the inverse
## one function CacheSolve that either calculates the inverse and stores
## it in the object i of the return of the first function
## or returns i if that value is not NULL



## return a list of functions to get/set the matrix and get/set 
## the inverse and 2 objects that store the matrix and the inverse that
## are accessed by these functions

makeCacheMatrix <- function(x = matrix()) {
  #initalize the variable i which will contain the inverse with NULL
  #important to create i in this environment otherwise the functions below
  #would write to the global environment 
  i <- NULL
  
  #function to set matrix
  set <- function(y) {
    #assign the argument y to x of the parent environment of set function
    #and inverse will be set to NULL as when set function is used we have
    #might have a new inverse
    x <<- y
    i <<- NULL
  }
  
  #return x of the parent environment of get function
  get <- function() {
    x 
  }
  
  #set i of parent environment to argument inv
  #this function will be called by cacheSolve
  setinv <- function(inv) {
    i <<- inv
  }
  
  #return i of the parent environment of function getinv
  getinv <- function() i
  
  #create a list of the functions defined above
  list(set = set, get = get, setinv = setinv,getinv = getinv)
  

}


## calculate the inverse of the matrix stored in object that
## holds the return value from makeCacheMatrix
## only if value stored there is NULL , otherwise return 
## value stored there

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      #get the value of the inverse stored in the object created by makeMatrix
      i <- x$getinv()
      #if it is not NULL we use the value stored there as we then 
      # do not have new data and can use the inverse already stored there
      #and don'T have to recalculate
      if(!is.null(i)) {
          message("getting cached data")
          return(i)
      }
      #get the matrix from object that holds return value from makeCaheMatrix
      data <- x$get()
      i <- solve(data, ...)
      #call setinv to set i in object holds return value from makeCaheMatrix
      x$setinv(i)
      i
}
