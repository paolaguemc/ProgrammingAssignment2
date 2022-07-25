## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inverse_matrix <-NULL
   set<-function(y){
     x<<-y
     inverse_matrix<<-NULL
   }
   get<-function()x                    #gets matrix "x"
   set_inverse<-function(i)inverse_matrix<<-i
   get_inverse<-function() inverse_matrix     #gets the inverse of matrix "x"
   list(set=set, get=get, set_inverse=set_inverse,get_inverse=get_inverse)
}
 
 
## Write a short comment describing this function
 
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {       
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix<-x$get_inverse()
  if (!is.null(inverse_matrix)){      #verifies if the inverse of matrix "x" has already been calculated
    message("getting cached data")
    return(inverse_matrix)
  }
  modify_matrix<-x$get()
  inverse_matrix<-solve(modify_matrix,...)  #calculates the inverse of matrix "x"
  x$set_inverse(inverse_matrix)
  inverse_matrix
}
