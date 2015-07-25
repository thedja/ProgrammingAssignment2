## This object stores a matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
	## Verifying that x is a true square matrix or stops the execution if not
	if(nrow(x) != ncol(x)){
		stop("x must be a square matrix !")
	}
	myInverse <- NULL
	
	## Changing the matrix implies changing the cached inverse, we must also verify if the new matrix is a square matrix
	set <- function(y){
		if(nrow(x) != ncol(x)){
			stop("x must be a square matrix !")
		}
		x <<- y
		myInverse <<- NULL
	}
	
	get <- function(){
		x
	}
	
	## We must verify two points before setting the new value of the cached inverse 
	setInverse <- function(inverse){
		## Verify if the two matrix are identical in number of rows and columns
		if(nrow(x) == nrow(inverse) && ncol(x) == ncol(inverse)){
			## Verify if X * Inverse_of_X == IDENTITY_MATRIX
			if(all(x * inverse == diag(nrow(x)))){				
				myInverse <<- inverse
			}else{				
				stop("this isn't a correct inverse for x !")
			}
		}else{
			stop("this isn't a correct inverse for x ! (the two matrix aren't identical in number of rows and columns)")
		}
		
	}
	
	getInverse <- function(){
		myInverse
	}
	
	list(
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse	
	)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()
		if(!is.null(inverse)){
			message("Getting cached inverse of x !")
			return(inverse)
		}
		message("There is no cached result !")
		inverse <- solve(x$get())
		x$setInverse(inverse)
		inverse
}
