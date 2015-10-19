## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##setmat and getmat sets and retrieves the matrix respectively 

#setinv and getinv ses and retrieves the matrix respectively

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(Y){
    x <<- Y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(Inverse) inv <<- Inverse
  getinv <- function() inv
  list(setmat=setmat,getmat=getmat,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##check if MASS library ispresent
  if(require("MASS")){
    print("MASS is loaded correctly")
    } 
  else {
    print("trying to install MASS package")
    install.packages("MASS")
    if(require("MASS")){
      print("MASS installed and loaded");
      library("MASS")
    } else {
      stop("could not install MASS babes")
    }
  }
  
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Matrix is already  in memory..why so serious!!")
    return(inv)
  }
  message("Its not in memory so for the inverse so lets do it babes!!")
  matx <- x$getmat()
  inv <- ginv(matx,tol=sqrt(.Machine$double.eps))
  x$setinv(inv)
  inverse
}
