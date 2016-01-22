## Following functions take advantage of the scoping rules of
##the R language and how they can be manipulated to preserve state inside
##of an R object.
##If the contents of a matrix are not changing, it may make
##sense to cache the value of the inverse of a matrix so that when we need it again, it
##can be looked up in the cache rather than recomputed.


## 'makeCavheMatrix creates a special "matrix" which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the values of the inverse (solve) of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y){
              x <<- y
              inv <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) inv <<-solve
            getmean <- function() inv
            list(set = set, get =get,
                 setinv =setinv,
                 getinv = getinv)
}


## This function computes the inverse of the special matrix returned by `makeCachematrix`above.
## If the inverse has already been calculated (and the matrix has not changed) then `cacheSolve
## should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix (inv) that is the inverse of 'x'
}
