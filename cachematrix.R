## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix will creates special "matrix" object that can cache its inverse.
## makeCacheMatrix has set, setMatrix, get, getMatrix, setInverse , getInverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inversedMatrix <- NULL          #Initializing
    setMatrix <- function(y) {      #Setter for passed Matrix
        x <<- y
        inversedMatrix <<- NULL
    }
    
    getMatrix  <- function() x      #Getter for passed Matrix
    
    setInverse <- function(SolvedMatrix) inversedMatrix <<- SolvedMatrix    #Setter for Inversed Matrix
    
    getInverse <- function() inversedMatrix               #Getter for Inversed Matrix
    # providing names gor various combinations just in case we forget setter/getter
    list(set = setMatrix, setMatrix = setMatrix, get = getMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


##  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cSinversedMatrix <- x$getInverse()
    # See if stored already 
    if(!is.null(cSinversedMatrix)) {
        message("getting cached data")
        return(cSinversedMatrix)
    }
    
    cSMatrix         <- x$getMatrix()
    cSinversedMatrix <- solve(cSMatrix)
    x$setInverse(cSinversedMatrix)  #Set inverse in Cache
    cSinversedMatrix
}
