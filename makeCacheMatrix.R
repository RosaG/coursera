##makeCacheMatrix
makeCacheMatrix<-function(x=matrix())  {
    xinv<-NULL
    set<- function(y){
        x<<-y
        xinv<<-NULL
    }
    get<- function() x
    setinversa<-function(inversa)  xinv<<-inversa
    getinversa<-function()   xinv
    list (set=set, get=get, setinversa=setinversa, getinversa= getinversa)
    
}
#
cacheSolve <- function(x, ...){
    
    xinv<-x$getinversa()
    if (!is.null(xinv)){
        message("getting cache data")
        return(xinv)
    }
    data<-x$get()
    xinv<-solve(data)
    x$setinversa(xinv)
    xinv
}  

## prueba

##x = rbind(c(1, 2), c(2, 1))
x = rbind(c(1, 2,5), c(2,5,0),c(3,2,1))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)