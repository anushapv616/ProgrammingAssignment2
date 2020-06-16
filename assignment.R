

makeCacheMatrix<- function(x=matrix()) {
     if (ncol(x)==nrow(x) && det(x)!=0) {
             m<-NULL
            set<-function(y) {
                   x<<-y
                   m<<-NULL
                   
                 }
          get<- function() x
             setInverse<- function(inverse) m <<- inverse
             getInverse<- function() m
             list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
            
               
           } else {
                 return(message("The matrix isn't invertible"))
                 
               }
   }

   cacheSolve<- function(x, ...) {
         m<-x$getInverse()
         if(!is.null(m)) {
               message("Getting cached data")
               return(m)
               
             }
        
         mat<-x$get()
         m<-solve(mat, ...)
         x$setInverse(m)
         m
     }
  
  
  
  
  
