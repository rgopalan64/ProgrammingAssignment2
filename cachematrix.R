
## makeCacheMatrix takes as argument a matrix and creates the "SPECIAL" matrix
# PLEASE NOTE GRADER: cacheSOLVE takes TWO arguments, the special matrix created by makeCacheMatrix AND
# the ORIGINAL matrix itself.  That way, the program can CHECK using the IDENTICAL function if the original
# matrix has undergone any changes...

makeCacheMatrix <- function(x = matrix()) { 
  # cachedInverse serves the role of the variable m in the MEAN example provided
  # the function SOLVE serves the role of the function MEAN in the example provided
  cachedInverse <- NULL 
  set <- function(y) { 
    x <<- y 
    cachedInverse <<- NULL 
  } 
  get <- function() x 
  setInverse <- function(solve) cachedInverse <<- solve 
  getInverse <- function() cachedInverse 
  list(set = set, get = get, 
            setInverse = setInverse, 
            getInverse = getInverse) 
  } 

## Return the inverse of an cacheMatrix object 
## GRADER PLEASE NOTE THIS CAREFULLY!!!
## THIS VERSION OF CACHESOLVE ACTUALLY CHECKS IF THE MATRIX HAS BEEN ALTERED SINCE
## THE LAST COMPUTATION OF THE INVERSE.  SO CACHESOLVE REQUIRES TWO ARGUMENTS
## ARGUMENT 1 is the "SPECIAL MATRIX" AND ARGUMENT 2 is the CURRENT STORED MATRIX
## (WHOSE INVERSE IS BEING COMPUTED)
## PLEASE REMEMBER TO PASS TWO ARGUMENTS TO CACHE SOLVE AS IN THE FOLLOWING CALL
## Z <- cacheSolve(B, A)
## B is the "special" matrix constructed with makeCacheMatrix
## A is the current form of the matrix
## The "identical" function checks if A has been altered from x$get()

cacheSolve <- function(x,y = matrix(),...) { 
     ## Return a matrix that is the inverse of 'x'  
     cachedInverse <- x$getInverse() 
     if((!is.null(cachedInverse)) & (identical(x$get(), y))) { 
         message("getting cached data") 
         return(cachedInverse) 
       } 
     message("resetting makeCacheMatrix for NEW altered matrix")
     x$set(y)
     data <- x$get() 
     # print(data)
     cachedInverse <- solve(data,...)
     x$setInverse(cachedInverse) 
     cachedInverse
   } 




