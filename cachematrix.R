## These functions compute and cache the inverse of a given matrix.  The matrix is always 
## assumed to be invertible.
  

## makeCacheMatrix takes a matrix as input and returns a list of functions to (1) set the matrix, (2) get the matrix, (3) set the inverse of the matrix, and (4) get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	#Set inverse to Null
	inv <- NULL
	#Define function to set the matrix
	set <- function(mat){
		x<<-mat
		#If matrix changes set inverse to NULL
		inv <<-NULL
		}
	
	#Define function to return the matrix
	get <- function () x	
	#Define function to set the inverse
	setinv<-function(inverse) inv <<- inverse
	#Define function to return the inverse
	getinv<-function() inv
	
	#Create and return list of functions
	list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve checks to see if the inverse of the matrix is cached.  If not it computes the
## inverse and caches it in the appropriate list element.  The inverse is then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #Check if inverse cached.  If so, return it
    inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	} 
	
	#If inverse not cached retrieve the matrix, compute its inverse, and cache the inverse
	data<-x$get()
	inv=solve(data, ...)
	x$setinv(inv)
	
	#return the inverse
	inv
}
