## "makeCacheMatrix" and "cacheSolve" functions  help us save time and energy 
## when calculating the inverse of a matrix is computed repeatedly.

## Makes a special matrix that can save its inverse

makeCacheMatrix<-function(x=matrix()){
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function()x
        setinverse<-function(n) inverse<<-n
        getinverse<-function()inverse
        list(set=set,get=get,setinverse=setinverse,
             getinverse=getinverse)        
}

## Gets the inverse from cache or calculates it

cacheSolve<-function(x,...){
        inverse<-x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        mat<-x$get()
        inverse<-solve(mat)
        x$setinverse(inverse)
        inverse
}
