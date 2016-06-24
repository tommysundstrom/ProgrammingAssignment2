## makeCacheMatrix creates a special version of matrix, that has the ability to
## cache inversion.
## cacheSolve is used to apply solve to an object created by makeCacheMatrix


## Creates a version of matrix, that has the ability to
## cache inversion (an expensive operation).

makeCacheMatrix <- function(the.matrix = matrix()) {
        # Useful things
        empty.cache <- list(mtrx = NULL, argmnts = list()) # mtrx stores the matrix
        # argmts stores a list of arguments to solve, if any
        
        # Create the cache, and set it to empty
        cached.inverse <- empty.cache
        
        
        # Returns the matrix, as is
        get <- function() the.matrix   
        
        # Returns the inverse of the matrix, by solving it. If already cached, returns 
        # the cached value. 
        inverse <- function(...){
                # Tactics: Will allways return the value from the cache. (Note that 
                # unlike in the vector example of the assignment, I've choosen to place
                # the responsibility to set the cache in this function, not in the 
                # cacheSolve function. )  
                
                # Start by checking the arguments list (i.e. arguments that has been provided
                # by ... ) is the same as when the cache was created.
                if ( length(setdiff(list(...), cached.inverse$argmnts)) != 0 ){
                        # The lists are different, ergo the arguments to sort is not the same 
                        # as when the cache was created, so I invalidate the cache.
                        message('Arguments differs from when cache was created, so recalculating')
                        cached.inverse <- empty.cache
                }
                
                # Make sure there is a value in the cache
                if (is.null(cached.inverse$mtrx)) {
                        # Cache is empty, so needs to calculate the inverted matrix
                        solved.matrix <- solve(the.matrix, ...)
                        # Place result in cache
                        cached.inverse <<- list(mtrx = solved.matrix, argmnts = list(...))
                } else {
                        # There is already a value in the cache, so no need to do anything
                        message('Returning the cached inverse')
                }
                # Return the matrix in the cache
                return(cached.inverse$mtrx)
        }
        
        # Sets a new matrix, and invalidates the cache
        set <- function(new.matrix){
                cached.inverse <<- empty.cache # Invalidate cache
                the.matrix <<- new.matrix
        }
        
        list(set = set, get = get, inverse = inverse)
}



## Returns the inverse of x.
##
## x must be a matrix created by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x$inverse(...)
}

