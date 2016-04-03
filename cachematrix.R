# This function creates a special a list
# containing functions to :
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse  matrix
# 4. get the value of the inverse  matrix
#
# The argument is a square invertible matrix


makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function(){ 
                x 
        } 
        setinverse <- function(inv){ 
                m_inv <<- inv
        }
        getinverse <- function(){
                m_inv
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function calculates the inverse of the matrix in the list 
# created with the above function. It first checks to see if the 
# inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the
# value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        m_inv <- x$getinverse()
        if(!is.null(m_inv)) {
                message("getting cached inverse")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...) 
        x$setinverse(m_inv)
        m_inv
}
