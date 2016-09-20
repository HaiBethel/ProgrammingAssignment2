##The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <-function(x=matrix()){
       m<- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get<- function() x
       setinverse <- function(inverse) m<<- inverse ## match the inverse matrix to m
       getinverse <- function()m 
       list(set=set, get=get, setinverse=setinverse, getinverse= getinverse) ## return a list of functions
       
}

##Inverse matrix ... if it is a new matrix then calculate the inverse
cacheSolve <- function(x,...){
       m<- x$getinverse()
       if(!is.null(m)) { ##checking if inverse is already done
              message("getting cached data")
              return(m)
       }
       data<- x$get()##Else get matrix
       m<- solve(data)##matrix inversed
       x$setinverse(m)## store the inverse for later checking
       m
}

## Sample results
##> x<-matrix(c(1,2,4,3),2,2)
##> x
##[,1] [,2]
##[1,]    1    4
##[2,]    2    3
##> solve(x)
##[,1] [,2]
##[1,] -0.6  0.8
##[2,]  0.4 -0.2
##> temp2<-makeCacheMatrix(x)
##> cacheSolve(temp2)
##[,1] [,2]
##[1,] -0.6  0.8
##[2,]  0.4 -0.2
##> cacheSolve(temp2)
##getting cached data
##[,1] [,2]
##[1,] -0.6  0.8
##[2,]  0.4 -0.2      
