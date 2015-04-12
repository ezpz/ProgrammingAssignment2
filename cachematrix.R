# 
# Generate a specialized matrix object that caches the inverse result.
# Caches are invalidated on changes to the matrix so functions are 
# provided for updating matrix data as well as reading it's contents
#

# Generate a specialized version of the matrix class that will cache
# calls to 'solve'. Provided interface:
# [update] Modify the contenxts of the matrix
# [read] Read the contents of the matrix
# [getCache] Returns the cached result of a 'solve' operation (or NULL)
# [setCache] Set the value of the cache
# [getMatrix] Returns the underlying matrix object
makeCacheMatrix <- function (m = matrix()) {
    cache <- NULL
    
    # Update the contents of the matrix and invalidate the cache
    update <- function (i, j, value) {
        m[i, j] <- value
        cache <<- NULL
    }

    # Read-only access to the underlying matrix
    read <- function (i, j) { m[i, j] }

    # Return the cached result. This returns NULL unless the cache is valid
    get <- function () { cache }

    # Save a cached 'solve' computation
    set <- function (result) { cache <<- result }

    # Provide access to the underlying object
    object <- function () { m }

    list (
         update = update,
         read = read,
         setCache = set,
         getCache = get,
         getMatrix = object
    )
}


# Perform a 'solve' operation on the passed in specialized matrix
# If there is a cached version of the operation (and it is valid) that 
# result is returned instead of performing the actual 'solve'
cacheSolve <- function (m, ...) {
    slv <- m$getCache ()
    if (! is.null (slv)) {
        return (slv)
    }
    obj <- m$getMatrix ()
    result <- solve(obj,...)
    m$setCache (result)
    result
}
