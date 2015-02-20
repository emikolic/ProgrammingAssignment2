## Functions for calculating inverted matrices and caching
## results of the calculations

## Function takes a matrix as a argument and if given matrix
## is invertible, the function will create functions for
## getting/setting inverse matrix 'inv' and original matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
	    if(!is.matrix(x)) 
                stop("x must be a matrix")
        else if(det(x)==0)
                stop("x is not invertible")
        else{
           inv<-NULL
           
           set<-function(y){
                   x<<-y
                   inv<<-NULL
           }
           
           get<-function() x
           
           setinv<-function(inverse) 
                   inv<<-inverse
           
           getinv<-function() inv
           
           list(set=set,get=get,setinv=setinv,getinv=getinv)
        }
}


## Function takes as an argument list of functions from the
## makeCacheMatrix function. It checks if there is a cached
## inverted matrix available to return. If there is no cached
## inverted matrix, it will try to make one and then it will
## save inverted matrix in makeCacheMatrix environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
