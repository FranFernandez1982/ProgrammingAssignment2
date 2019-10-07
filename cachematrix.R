## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(inv) m <<- inv
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmean() # Se guarda el valor getmean de la lista en "m"
    if(!is.null(m)) { # Si el valor de "m" es nulo, entonces salta el "if", pero sino lo devuelve
        message("getting cached data")
        return(m)
    }
    data <- x$get() # En el "data" mete la función "get" que si recuerdas era simplemente el valor "x"
    m <- solve(data, ...) # Calcula el inverso de la matríz y se lo pasa a "m"
    x$setmean(m) # Se ejecuta la función "setmean" y se almacena el valor de la media en la lista setmean
    m
}
