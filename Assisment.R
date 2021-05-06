#makeCacheMatrix Function

makeCacheMatrix <- function(x = numeric())
{
  k = NULL
  set <- function(f)
  {
    x <<- f
    k = NULL
  }
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function()k
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}

#cacheSolve Function

cacheSolve <- function(x, ...)
{
  k <- x$getInverse()
  if(!is.null(k))
  {
    message("Getting cached data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat, ...)
  x$setInverse(k)
  k
}

