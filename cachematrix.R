## The first function here returns a list of functions to the parent environment and 
## also includes objects x and s, x being a matrix and s being the inverse of the matrix.
## The second function, cacheSolve, then checks to see if there's a cached version of s.
## If there is, it returns it with a helpful message. If not, it returns a new value for the inverse of 
## the matrix


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL 
        set <- function(y) { #sets both data objects x and s within the parent enviroments
                x <<- y #assigns y to x in parent enviro
                s <<- NULL #assigns NULL to s in parent environment so when x is reset, s is cleared
        }
        get <- function() x #retrieves the matrix x
        setsolve <- function(solve) s <<- solve #assigns solve fcn to s object in parent environment
        getsolve <- function() s # retrieves the solve function
        list(set = set, get = get, #names the functions for use with the handy $ operator
             setsolve = setsolve,
             getsolve = getsolve)

}



cacheSolve <- function(x) {
        
        
        s <- x$getsolve() #assigns s the value of x$getsolve(), which may be null
        if(!is.null(s)) { 
                message("Get ALL the cached things!")
                return(s) #if x$getsolve() is not null, then lreturn the cached item
        }
        
        
        data <- x$get() #otherwise, get the value of x$get(), which retrieves the matrix x
        s <- solve(data) #execute solve on data and assign it to s
        x$setsolve(s) #execute the x$setsolve function on s
        s #return s
        
}

# Use the following to test the functions. 
# source("cachematrix.R")
# x <- matrix(1:4,2,2)
# z <- makeCacheMatrix(x)
# cacheSolve(z)
