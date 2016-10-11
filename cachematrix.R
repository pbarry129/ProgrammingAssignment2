## Put comments here that give an overall description of what your
## functions do
## The constructor function makeCacheMatrix() allows us to contruct 
## a matrix and either assign NULL to its inverse (the default) or to 
## assign it a specific value through the setInverse() method. It 
## also allows us to inspect the stored value of the inverse. 
## The solveCacheMatrix() function uses this inspection method to see 
## if the inverse value is stored with the matrix (ie is not NULL). 
## It it is already stored, then solveCacheMatrix() returns this value 
## for the inverse, otherwise it use the solve() function to work it out



## Write a short comment describing this function

## makeCacheMatrix is a constructor function for class matrix that 
## associates methods get, set, setInverse and getInverse to it.
## By default, the inverse is set to NULL 

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y) {
         x<<-y
         i<<-NULL
         }
    get<-function() x
    setInverse<-function(inverse) i <<- inverse 
    getInverse<-function() i
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}




## Write a short comment describing this function

## cacheSolve is a function that works in conjunction with the 
## matrix class constructor makeCacheMatrix to efficiently calculate  
## the inverse of a matrix: if the inverse of the matrix has 
## already been calculated (and stored in cache), then this value 
## is retrieved, otherwise the inverse is calculate using the solve() 
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getInverse()
      if(!is.null(inverse)){
           message("getting cached inverse")
           return(inverse)
       }
       data<-x$get()
       inverse<-solve(data,...)
       x$setInverse(inverse)
       inverse
}

