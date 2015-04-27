makeCacheMatrix = function(x=matrix()) {
	inv=NULL
	set=function(y=x){            
			x <<- y
			inv <<- NULL
			x
        	   }
        get=function() x
if (dim(x)[1]==dim(x)[2]){
if (det(x)!=0){
        setSolve=function(get) inv <<- solve(x)
        getSolve=function() inv
        list(set = set(), get = get(), setSolve = setSolve(), getSolve = getSolve())
}
} else {
mes<<-"matrix is not invertible"
list(set = set(), get = get(), setSolve = mes, getSolve = mes)
}
}

##__________________________________________________________________________

cacheSolve=function(x) {
        m=x$getSolve
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data=x$get
if (dim(data)[1]==dim(data)[2]){
if (det(data)!=0){
        m=solve(data)
}
} else { 
m=mes 
}
        x$setSolve
        m
}
