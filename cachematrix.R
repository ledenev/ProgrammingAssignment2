## Caching the Inverse of a Matrix
##
## These pair of functions implement caching the inverse of a matrix
## rather than computing it repeatedly


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsol <- function(solution) m <<- solution
        getsol <- function() m
        list(set = set, get = get, setsol = setsol, getsol = getsol)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getsol()
        if(!is.null(m)) {
            message("Getting cached matrix")
            return(m)
        }
        matr <- x$get()
        m <- solve(matr, ...)
        x$setsol(m)
        m
}

## Main function with a smoke test

main <- function() {
        matr <- matrix(rep(1,16), nrow = 4, ncol = 4)
        matr[2,2] <- 3
        matr[3,3] <- 0
        matr[4,4] <- 2

        inversed <- matrix(
                c( 1.5, -0.5,  1.0, -1.0,
                  -0.5,  0.5,  0.0,  0.0,
                   1.0,  0.0, -1.0,  0.0,
                  -1.0,  0.0,  0.0,  1.0),
                nrow = 4, ncol = 4)

            ## Create new cached object

        matrix_obj <- makeCacheMatrix(matr)

            ## Print original object

        message("Original matrix object:")
        pointer <- if (is.null(matrix_obj$getsol())) {"NULL"} else {"Not NULL !!!"}
        message(paste("Cached matrix: ", pointer, sep = ""))
        print(matrix_obj$get())

            ## First access to object

        matrix_1 <- cacheSolve(matrix_obj)

            ## Validation of result

        message("Inversed matrix:")
        print(matrix_1)

        if (all.equal(matrix_1, inversed))
            message("Inversed matrix is OK")
        else {
            message("Bad inversed matrix. Expected result:")
            print(inversed)
            return(1)
        }

            ## Second access to object

        matrix_2 <- cacheSolve(matrix_obj)

            ## Validation of result

        if (all.equal(matrix_1, matrix_2)) {
            message("Cached matrix is OK")
            0
        }
        else {
            message("Bad cached matrix. Test failed")
            print(matrix_2)
            1
        }
}
