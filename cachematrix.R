makeCacheMatrix<-function(x = matrix()){   # makematrix is the function that gets matrix input from the user
  m<-NULL                             # intializing
  set<-function(y){                   # setting the matrix  
    x<<-y
    m<<-NULL
  }
  get<-function() x                   # getting the input matrix
  setinv<-function(solve) m<<-solve   # setting the inverse of the matrix
  getinv<-function() m                # getting the inverse of the matrix 
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}

cacheSolve<-function(x,...){            # cacheinv is the function that calculates the inverse of the matrix 
  m<-x$getinv()                       # the getinv function is called to get the inverse of the matrix
  if(!is.null(m)){                    # checks for non zero condtion of the object 'm'. If it is not zero then the message is displayed and the cached matrix is returned. If is zero then the inverse of the new matrix is calculated. 
    message('getting cached data')    # this message is displayed if the input matrix is same the previous input matrix
    return(m)                         # returns m that contains the inverse of the matrix
  }
  data<-x$get()                       # the natrix is assigned to the object 'data'
  m<-solve(data,...)                  # inverse of the input matrix is calculated
  x$setinv(m)                         # sets the inverse of the matrix
  m                                   # returns the m that contains the inverse of the matrix
}
