## It is too complicated to calculate the inverse of a matrix sometimes
## because of the repeated computations. The following function can 
## help simplify the inverse computations.

## The function caches the inverse of a matrix by creating a special 
## "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
        inv<<-NULL
        set<-function(y){
              x<<-y
              inv<<-NULL
        }
        get<-function()x
        setInverse<-function(inverse) inv<<-inverse
        getInverse<-function() inv
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}

## This function is used to compute the inverse of 
## a special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated, it 
## should be retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
               message("getting cached data")
               return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}
