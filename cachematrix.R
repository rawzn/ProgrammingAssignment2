## The functions here 'maekCacheMatrix' and 'cacheSolve' inverse any invertible matrix that is given as an input. 
## The functions will use the laxical scoping principle of R and will first look for the inverse matrix in the global environment. If the required matrix exists in the environment the function will simply cache the data without additional computing. However, if the function does not find the required inverse matrix in the environment it will proceed anf calculate the desired inverse matrix.

## makeCacheMatrix function will take a matrix as an input, thus I put an empty matrix as an input so that I can chnage to any matrix as I want later. the function has set the function for the input- that is , to inverse the given matrix. Value of x and inv is assigned in the function accordingly in the set fucntion also. Then later the setinverse function is used to get the inverse of the input matrix and then assign it to the object inv. At last, the getinverse function is used to get the value of the operation in the inv. Later, a list is made with the name given to the each function. This is done so that the function can subset the input with $ sign. 

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
        
        set <-  function(y) {
          x <<- y
        inv <<- NULL
       
        }
      
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## the cacheSolve function takes a matrix as an input. It will then attempt to find the inverse of the matrix in the environment. If the environment has the inverse matrix, the function will copy the value without recalculating the whole thing. But if the environment does not contain the inverse matrix then the function  will get the input object and calculate the inverse of the matrix and will set the result in the environment for later use. 

cacheSolve <- function(x, ...) {
          inv <- x$getinverse()
          if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
          }
          
          data <- x$get()
          inv <- solve(data, ...)
          x$setinverse(inv)
          inv
}
