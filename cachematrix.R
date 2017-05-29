makeVector<- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
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

makeCacheMatrix<-function(X=matrix()){
        M<-NULL
        set<-function(Y=matrix()){                ##set value of matrix
                X<<-Y
                M<<-NULL
        }
        get<-function()X                          ##get value of matrix
        setInverse<-function(Inverse) M<<-Inverse ## set value of the inverse 
        getInverse<-function()M                   ## get value of the inverse
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse
        )
}

cacheSolve <-function(X,...){
        M<-X$getInverse()                       ## function to calculate the inverse
        if(!is.null(M)){                        ## check if inverse is already calculated
                message("getting cached data")
                return(M)                       ## returns the inverse from cache
        }
        
        matrix <- X$get()                       
        M<-solve(matrix,...)                    ## calculates the inverse
        X$setInverse(M)                         ## sets the value of inverse in cache using this function
        return(M)
}
