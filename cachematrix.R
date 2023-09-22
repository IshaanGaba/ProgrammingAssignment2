## Put comments here that give an overall description of what your
## functions do

##There are two functions makeCacheMatrix,cacheSolve
##makeCacheMatrix consists of set,get,setinv, getinv,etc.
##library(MASS) is used to calculate inverse.
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL           
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x             #function to get matrix x
  setinv<-function(inverse)i<-inverse
  getinv<-function(){ 
    inver<-ginv(x)
    inver%*%x           #function to obtain inverse of the matrix
  }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function

## get the cache data
cacheSolve <- function(x, ...)       
{
  i<-x$getinv()                  
  if(!is.null(i)){                  
    message("getting cached data!")     #checking whether inverse is NUll
    return(i)                       #returns inverse value
  }
  data<-x$get()
  i<-solve(data,...)              
  x$setinv(i)
  i   ## Return a matrix that is the inverse of 'x'
}

