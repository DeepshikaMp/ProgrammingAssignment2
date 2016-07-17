## This function is used to create a special object that stores a matrix
makeCacheMatrix<-function(x=matrix())
{
   i<<-NULL  ## invertible matrix is assigned to null, placeholder purpose.
 
   # Set function used to set matrix 
  set<-function(y)
  {
    x<<-y
    i<<-NULL
  }
  
   # Get function used to get the matrix 
  get<-function()
    x
    
  # setInverse function to set inverse matrix
  setInverse<-function(inverse)
    i<<-inverse
    
    # getInverse function used to get inverse matrix  
  getInverse<-function()
    i
  
    # input to cacheSolve
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

  ## This function is used to compute inverse of matrix returned from above function, if the inverse is already computed,
  ## cacheSolve should return already cached inverse.
cacheSolve <- function(x, ...) {
  # x is makeCacheMatrix output and this function returns the inverse of x matrix
  
  
  inv = x$getInverse()   #inv stores inverse of x if already computed else stores NULL
  
  
  if (!is.null(inv))  #checks if inv is not null
  {     
    
    message("getting cached data")
    return(inv)        #returns cached data
  }
  
  
   mat.data = x$get()        #compute inverse otherwise
  inv = solve(mat.data, ...)
  
 
  x$setInverse(inv)      #set value of inverse in cache
  
  return(inv)
}

