## These two functions calculate and cache the 
## inverse of a square matrix
## use: 
## 1. create a squarematrix m, e.g.: 
## m<-matrix(1:4,2,2)
## 2. create a 'cacheMatrix' object: 
## cacheMatrix<-makeCacheMatrix(m)
## 3. calculate and store the inverse matrix:
## cachedInverseMatrix<-cacheSolve(cacheMatrix)

#This function creates two matrix objects to store 
#the original and the calculated inverse matrix
#The values of the matricis can be set and get by 
# four functions: (set, get, setinverse, getinverse)
# and the functions returns a list which contains them 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setinverse<-function(inverse){
    inv<<-inverse
    
  }
  getinverse<-function(){
    inv
  }
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

# This function returns a matrix that is the inverse of 'x'
# First, it checks if the inverse has already been calulated
# if so, then return it
# if not, it calculates and caches the inverse
cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    mesagge("getting cached data")
    return(inv)
  }else{
    data<-x$get()
    inv<-solve(data)
    x$setinverse(inv)
    inv
  }
}