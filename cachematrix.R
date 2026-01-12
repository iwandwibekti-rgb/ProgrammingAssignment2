
## This function creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeVector <- function(x = numeric()) {
                m <- NULL
                set <- function(y) {
                  x <<- y
                  m <<- NULL
                }
                get <- function() x
                setmean <- function(mean) m <<- mean
                getmean <- function() m
                list(
                  set = set,
                  get = get,
                  setmean = setmean,
                  getmean = getmean
                  )
}


## The following function calculates the mean of the special "vector" created with the above function.

cachemean <- function(x, ...) {
              m <- x$getmean()
              if(!is.null(m)) {
                message("getting cached data")
                return(m)
              }
              data <- x$get()
              m <- mean(data, ...)
              x$setmean(m)
              m
}


