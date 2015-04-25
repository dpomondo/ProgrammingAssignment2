## Put comments here that give an overall description of what your
## functions do

## Takes an invertible matrix object and stores it in a list, along with
## functions to set & get its inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse_storage variable with NULL
    inverse_storage <- NULL
    # method set: move the matrix data and inverse_storage into varaibles
    # that exist in the parent environment
    set <- function(y) {
        y <<- x
        inverse_storage <<- NULL
    }
    # method get: (anonymous?) function which just returns the matrix data
    get <- function() x
    # method setSolve: this should NOT be called from the command line as it 
    # is incomplete as it is; it is just a way to move the data in the variable
    # into the inverse_storage var living in the parent environment. This 
    # method only exists so cacheSolve can store the resulting inverse matrix
    # with the parent matrix.
    setSolve <- function(solve_data) inverse_storage <<- solve_data
    # method getSolve: returns the data living in the inverse_storage var
    getSolve <- function() inverse_storage
    # store all the methods in a list!
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Takes as input a list object created by makeCacheMatrix, returns the inverse
## of the original matrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # so long as x is list with the appropriate named functions, this returns
    # the value of the getSolve method
    inverse_of_original <- x$getSolve()
    # if calling the getSolve method returns a value, we return that value.
    # In other words, we have cached a value and need only return it.
    if(!is.null(inverse_of_original)) {
            message("Returning previously cached data")
            return(inverse_of_original)
    }
    # If calling getSolve returns nothing, we have to actually call the solve() 
    # function and do the math.
    data <- x$get()
    inverse_of_original <- solve(data, ...)
    # But here we can save the result (cache it, in other words) by calling
    # the setSolve method.
    x$setSolve(inverse_of_original)
    # BUt we still WANT the inverse matrix so we sould return it huh?
    inverse_of_original
}
