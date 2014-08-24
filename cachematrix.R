

## The File has 3 functions namely
## 1. makeCacheMatrix : This function stores the cached information of a matrix and its inverse in a seperate environment 
## 2. cacheSolve: This function does the lookup in the Cache list to check if for the given matrix, inverse readily exists in the cache
## 3. Initiator: This function is to demonstrate how the above 2 functions interact and can be seen in action

## This function is responsible to construct the cache for each matrix, 
## and needs to be called for each matrix at least once to ensure that they are inserted into the cache

makeCacheMatrix <- function(x = matrix()) 
{

  ##Seting the inverse to NULL when the function is inititally called
  inverse <- NULL
  
  ##The get function of the makeCacheMatrixList looks up for the original matrix value that needs to be inverted
  ## So when the cache is being looked up to see if the inverse readily exists and the inverse is not found it
  ##  is this function that  gets called to get the original value to calculate the inverse from scratch
  get <- function()
        {
          x
        }
  
  ## The set function of the makeCacheMatrixList sets up the list for the original matrix values for which inverse needs to be calculated
  ## Since the set function gets called only the first time, it inserts a NULL by default ensuring that the inverse has not yet been calculated
  set <- function(matrixValue) 
        {
          x <<- matrixValue
          
          ## Making sure the corrosponding inverse is still set to NULL. The actual inverse value will get be returned/set by the setInverse function
          inverse <<- NULL
        }
  
  ## The setInverse function of the makeCacheMatrixList actually updates the inverse after the inverse is calculated from scratch
  setInverse <- function(invertedMatrix) 
        {
          inverse <<- invertedMatrix
        }
  
  ## The getInverse function is the one that is executed first to check if for a given matrix, the inverse readily exists. If the SetInverse has 
  ## gotten executed earlier, then the inverse holds a proper inverted matrix, else it holds a NULL value
  getInverse <- function() 
        {
          inverse
        }
  
  ##The makeCacheMatrix returns a list of 4 functions that operate on the matrix x and the other variable in the same environment
  list("set" = set, "get" = get,
       "setInverse" = setInverse,"getInverse" = getInverse)
  
}


## The cacheSolve function looksup the matrix in the Cache for an inverse, if not found, it calculates from scratch and updates cache

cacheSolve <- function(x, ...) 
{
  ## Check the cache for the given special vector to see if an inverse is readily available
  inverse <- x$getInverse()
  
  ## If Inverse is available (Non NUll Value) then print that the Inverse was used from the cache and exit the function
  if(!is.null(inverse )) 
  {
    print("getting cached data")
    return(inverse)
  }
  
  ## If the inverse was not found in the cache, then get the original matrix value from the cache
  invertedMatrixdata <- x$get()
  
  ##For the original value, calculate the inverse (assuming that the matrix is inversible)
  inverse <- solve(invertedMatrixdata, ...)
  
  ## Once the inverse is calculated, set the inverse in the cache, so that next time, the inverse can be used from the cache
  x$setInverse(inverse)
  
  ##return and print the inverse value to the caller
  inverse
}

## Initiator function creates a set of matrices to test the caching functionality
Initiator <- function()
{
  ## Lets make 3 sample matrices for testing
  m1<-matrix(c(1,-2,3,0,-1,4,0,0,1),nrow=3,ncol=3)
  m2<-matrix(c(3,1,0,3),nrow=2,ncol=2)
  m3<-matrix(c(0,15,1,-3,15,-12,-3,2,-32,96,9,-12,96,-64,-12,-9),nrow=4,ncol=4)
  ##m4 is a singular matrix whose inverse cannot be calculated
  ##m4<-matrix(c(0,4,1,0,5,1,0,0,2),nrow=3,ncol=3)
  
  ## Creating a list of cachematrices 
  z<-list(makeCacheMatrix(m1),makeCacheMatrix(m2),makeCacheMatrix(m3))##,makeCacheMatrix(m4))
  
  ##Now lets try and see what happens when we try to get the inverse
  
  ##First time accessing m1, so solving inverse from scratch
  print(cacheSolve(z[[1]]))
  
  ##First time accessing m3, so solving inverse from scratch
  print(cacheSolve(z[[3]]))
  
  ## Inverse for m1 was evaluated previously, so it should be used from Cache now
  print(cacheSolve(z[[1]]))
  
  ##First time accessing m2, so solving inverse from scratch
  print(cacheSolve(z[[2]]))
  
  ## Inverse for m1 was evaluated previously, so it should be used from Cache now
  print(cacheSolve(z[[1]]))
  
  ## Inverse for m3 was evaluated previously, so it should be used from Cache now
  print(cacheSolve(z[[3]]))
  
}



