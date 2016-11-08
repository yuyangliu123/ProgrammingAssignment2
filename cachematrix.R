## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions 
##that cache the inverse of a matrix.
 

## The first function: "makeCacheMatrix". Creates a special "matrix" object that can cache its inverse
## set the value of matrix
## get the value of matrix
## set the value of matrix inversion
## get the value of matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinver<-function(inverse) inv<<-inverse
  getinver<-function() inv
  list(set=set,
       get=get,
       setinver=setinver,
       getinver=getinver)
}


## The second function: "cacheSolve". Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getinver()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  a<-x$get()
  inv<-solve(a,...)
  x$setinver(inv)
  inv
}


## The example of functions above

##>my_matrix<-matrix(c(1,0.5,-0.25,3),nrow = 2)
##>my_matrix
##     [,1]  [,2]
##[1,]  1.0 -0.25
##[2,]  0.5  3.00

##> input<-makeCacheMatrix(my_matrix)
##> input$get()
##     [,1]  [,2]
##[1,]  1.0 -0.25
##[2,]  0.5  3.00

##> cacheSolve(input)
##      [,1] [,2]
##[1,]  0.96 0.08
##[2,] -0.16 0.32
##> cacheSolve(input)
##getting cached data
##      [,1] [,2]
##[1,]  0.96 0.08
##[2,] -0.16 0.32

##If I want to see a new matrixs' inversion but don't want to make a new data

##Example
##> input$set(matrix(c(1,2,3,4),nrow=2))
##> input$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(input)
##getting cached data
##      [,1] [,2]
##[1,]  0.96 0.08
##[2,] -0.16 0.32




