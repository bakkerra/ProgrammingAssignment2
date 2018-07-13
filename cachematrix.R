##This system of functions stores a matrix and its inverse in an environment
##in order to access them quickly


## makeCacheMatrix makes a list of functions that gets and stores the 
##inverse of a matrix object,x

makeCacheMatrix<-function(x=matrix()){
  ##set inverse matrix object S to zero
  S<-NULL
  ##define set function that stores input matrix in parent environment and resets inverse to zero
  set<-function(y){
    x<<-y
    S<<-NULL
  }
  ##define get function in order to access input matrix
  get<-function() x
  ##define setsolve to store inverse matrix in parent environment
  setsolve<-function(solvedmatrix) S<<-solvedmatrix
  ##define getsolve to access inverse matrix
  getsolve<-function() S
  list(set= set, get=get, setsolve=setsolve, getsolve=getsolve)
}


##cacheSolve finds the inverse of the matrix stored in the makeCachematrix environment. 
##It either gets it from the cache or calculates and stores it

cacheSolve<- function(x,...){
  ##access inverse matrix stored in makecachematrix environment
  S<-x$getsolve()
  ##if it is not null, access inverse matrix from the cache and print inverse matrix
  if(!is.null(S)){
    message("getting cached data")
    return(S)
  }
  ##access input matrix stored in makecachematrix environment
  data<-x$get()
  ##make and store the inverse matrix in makecachematrix environment, print inverse matrix
  S<-solve(data,...)
  x$setsolve(S)
  S
}
