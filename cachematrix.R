## R±à³Ì×÷Òµ git push  -f origin master

## Write a  short comment describing this function
## I make the inverse matrix contains all NA

makeCacheMatrix <- function(x = matrix()) {
    inverse <- matrix(NA,nrow(x),ncol(x))
    set <- function(y){
        x <<- y
        inverse <- matrix(NA,nrow(x),ncol(x))
    }
    get <- function() x
    setInverse <- function(ans) inverse <<- ans
    getInverse <- function() inverse
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## judge the inverse matrix cached or not by counting the number of NA 
## the count is larger than zero when the inverse is NULL

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    is_cached <- (sum(!is.na(inverse))>0)
    if(is_cached){
        message("getting cached data")
        return(inverse)
    }
    tmp <- x$get()
    inverse <- solve(tmp)
    x$setInverse(inverse)
    inverse
}
