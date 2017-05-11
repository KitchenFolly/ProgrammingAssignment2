## Functions are "over-documented" for my own aging memory...

## This function does the following:
## 1) takes a matrix as input and caches it for re-use
## 2) takes the inverse of the same matrix as input from the solveMatrix
## function below and caches it for re-use
## Goal of the function is to prevent unecessary recalculation of matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ##declare empty locally to hold inverse later
        
        ## SetMatrix - caches the matrix for reuse
        ## called from cacheSolve with the value of y
        set_matrix <- function(y)  {     
                x <<- y  ##change environment of x and stuff y into it         
                inv <<- NULL  ##change env of inv
        }
        
        ## GetMatrix - returns cached matrix
        get_matrix <- function() x  
        
        ## SetInverse - caches the inverse of the matrix for reuse
        ## called with cacheSolve with the value of inverse
        set_inverse <- function(inverse) inv <<- inverse 
        
        ## GetInverse - return cached inverse of matrix
        get_inverse <- function() inv 
        
        ## display options for user
        list (set_matrix = set_matrix, get_matrix = get_matrix, 
              set_inverse = set_inverse,
              get_inverse = get_inverse)
        
}

## This function is dependent on the makeCacheMatrix function above.
## 1) checks to see if the inverse of matrix has been calculated and stored, and
## if so, returns it using the get_inverse function in makeCacheMatrix.
## 2) if not stored, this function uses get_matrix from makeCacheMatrix to retrieve
## the matrix, calculates the inverse inside this function, and both stores it 
## using using set_matrix from makeCacheMatrix, and returns it

cacheSolve <- function(x, ...) {
        ##Check the cache for previously stored inverse 
        inv <- x$get_inverse()  ##call get_inverse from above
        if(!is.null(inv)) {     ##if it is not null return it, else move on
                message("getting cached data")  
                return(inv)
        }
        
        ## Get the matrix from above, solve for inverse, cache it, display it
        mat <- x$get_matrix()   ##since not cached, call get_matrix from above
        ## there is no null check for the matrix
        inv <- solve(mat, ...)  ##solve for the inverse
        x$set_inverse(inv)      ##call set_inverse from above to cache the inverse
        inv                     ##return the inverse to console
}

