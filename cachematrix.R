## Put comments here that give an overall description of what your
## functions do
##  makeCacheMatrix is a function that takes a vector or matrix and checks to see if it is a square matrix
##  If the input is a square matrix or can be converted to a square matrix and cached then the fundtion
##  calls the cacheSolve function which will inverse the matrix if it's inverse is not already cached or if 
##  an error is thrown because of collinearity it will return that it cannot invert the matrix.
## makeCacheMatrix prints a message when successful that your input is an invertable matrix and returns the inverse of the input matrix


## makes the matrix

makeCacheMatrix <- function(x = matrix()) {
  ##  The is.wholenumber is defined to check 
  ## for whole numbers to be used 
  ## to check the result of the square root of the input to determine if equal rows
  ## and equal columns can be generated to make a square matrix
  
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  #x<- c(1:2,22,33, 44, 218, 111, 89, 99, 99, 2, 22:33,23, 44)
  a<-x
  px<<-matrix(1:4, nrow= 2, ncol=2)
  if (is.wholenumber(sqrt(length(a)))==FALSE){
    print("Input cannot be transformed into a square invertable matrix")
  }else {
    dimensions<- sqrt(length(a))
    y<<- matrix(a, nrow=dimensions, ncol=dimensions)
    print("your input is an invertible matrix")
    inversematrix<<-cacheSolve(y)
    return(inversematrix)
  }
  
}


## cacheSolve is will return the inverse of a square matrix or use the cached inverse or return an error 
## if errors occur.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if(is.matrix(x) == TRUE & sum(px) == sum(y) & exists("inversematrix")==TRUE){
   # print("cached inverse returned")
    return(inversematrix)
    
  }  else {
    
    result = tryCatch({
      return(solve(x))
    }, warning = function(w) {
     return ("warning generated")
    }, error = function(e) {
      return("Error: Cannot Invert Matrix, please ensure it is a valid invertible matrix")
    }, finally = {
    
      px <<- y
    })
  
  
  } 
}
