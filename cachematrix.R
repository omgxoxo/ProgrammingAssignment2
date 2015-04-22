## Create a pair of functions to cache a matrix and
## compute and return the inverse of the matrix.

## makeCacheMatrix function creates a special matrix that can
## cache its inverse.  It stores the matrix and its inverse in some 
## variables as a placeholder and access them through 4 functions. 

makeCacheMatrix <- function(mx = matrix()) {      ## creates a special "matrix" object
       
    sv <- NULL                                    ## clears any cached inverse
    set <- function(new_mx) {                     ## function to save new_mx into the matrix object 
        mx <<- new_mx                             ## copies new_mx into a variable mx in parent scope 
        sv <<- NULL                               ## clears any previously stored inverse in parent scope 
    }
    get <- function() mx                          ## get() function returns mx in its lexical scope
    setSolve <- function(solve) sv <<- solve      ## stores new incoming inverse
    getSolve <- function() sv                     ## returns the stored matrix inverse 
    list(set = set, get = get,                    ## methods that allow access to cached matrix and its inverse
         setSolve = setSolve,                     ## where functions are defined (parent environment)
         getSolve = getSolve)
        
}


## cacheSolve function computes the inverse of the special 
## martix from makeCacheMatrix.  If the inverse has been calculated,
## it retrieves the inverse of the unchanged matrix from the cache.

cacheSolve <- function(mx, ...) {                 ## returns a matrix that is the inverse of 'mx'
        
    sv <- mx$getSolve()                           ## use method to get inverse from object
    if(!is.null(sv)) {                            ## if matrix object already has an inverse, return it with message
        message("getting inversed matrix")
        return(sv)
    }
    data <- mx$get()                              ## otherwise get matrix from the object 
    sv <- solve(data, ...)                        ## calculates its inverse
    mx$setSolve(sv)                               ## stores the inverse in the object sv
    sv                                            ## return sv to the parent scope 
}




