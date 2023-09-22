## Put comments here that give an overall description of what your
## functions do

##There are two functions makeCacheMatrix,cacheSolve
##makeCacheMatrix consists of set,get,setinv, getinv,etc.
##library(MASS) is used to calculate inverse.
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL           
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x             #function to get matrix x
  setinv<-function(inverse)inv<-inverse
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
  inv<-x$getinv()                  
  if(!is.null(inv)){                  
    message("getting cached data!")     #checking whether inverse is NUll
    return(inv)                       #returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)              
  x$setinv(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}

