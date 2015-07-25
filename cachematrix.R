## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	if(nrow(x) != ncol(x)){
		stop("x must be a square matrix !")
	}
	
	myInverse <- NULL
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
	
	setInverse <- function(inverse){
		if(nrow(x) == nrow(inverse) && ncol(x) == ncol(inverse)){
			if(all(x * inverse == diag(nrow(x)))){				
				myInverse <<- inverse
			}else{				
				stop("this isn't a correct inverse for x !")
			}
		}else{
			stop("this isn't a correct inverse for x ! (the two matrix aren't compatible in number of rows and columns)")
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


## Write a short comment describing this function

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
