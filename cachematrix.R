## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{

  ##Initially setting the value of Inverse to NULL
  inverse <- NULL
  
  ##The get function of the makeCacheMatrixList looks up the matrix to be inverted
  ## So when the cache is being looked up to see if the inverse readily exists this
  ## this is the function that is gets called to check if the matrix has an inverse readily available
  get <- function()
        {
          x
        }
  
  ##The set function of the makeCacheMatrixList sets up the list for the original matrix values for which inverses are not already
  ##present in the list.
  ## So when the cache is being looked up to see if the inverse readily exists this
  ##  is the function that  gets called to insert the matrix values for which the inverse was calculated from scratch
  ## Once this function sets the value for such a matrix, then that matrix is available in the cache, and next time its inverse is requested
  ## it can be readily looked up from the cache instead of being calculated from scratch
  set <- function(matrixValue) 
        {
          x <<- matrixValue
          
          ## Making sure the corrosponding inverse is still set to NULL. The actual inverse value will get set by the setInverse function
          inverse <<- NULL
        }
  
  ##The setInverse function of the makeCacheMatrixList actually inserts the inverse up the list for values that are not already
  ##present in the list.
  ## So when the cache is being looked up to see if the inverse readily exists this
  ## this is the function that is gets called to insert those values that are not available in the list 
  ## for which the inverse was calculated from scratch
  setinverse <- function(invertedMatrix) 
        {
          inverse <<- invertedMatrix
        }
  
  getinverse <- function() 
        {
          inverse
        }
  
  list(set = set, get = get,
       setInverse = setInverse,getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) 
  {
    message("getting cached data")
    return(inverse)
  }
  
  invertedMatrixdata <- x$get()
  inverse <- solve(invertedMatrixdata, ...)
  x$setInverse(inverse)
  inverse
}
