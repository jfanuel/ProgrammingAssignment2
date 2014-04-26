#***********************************
#
#       General section
#               
#**********************************
# Please make sure to source this script with echo mode set
# so as to see the output on the console
# syntax
#      source('PathTo_matrixCache.R', echo=T)


#***********************************
#
#       Code section
#               
#**********************************


## makeCacheMatrix returns a "cache matrix maker"
#  which provides 4 services
#     set(M)    : sets the input matrix given as argument
#     get()     : returns the input matrix
#     setinv(Mi): sets the invert matrix given as argument
#     getinv()  : retrieves the invert matrix
#  makeCacheMatrix accepts as single argument the
#  value of the input matrix.  When given this way, it is not
#  necessary to call the set(M) service to pass the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  #invert matrix set to NULL at initiation
  xi <- NULL
  #The set(M) function to save the input matrix
  set <- function(M) {
    #save the input matrix in cache
    x <<- M
    #reset the invert matrix as a new input has been given
    xi <<- NULL
  }
  #the get() function to retrieve the matrix given as input
  get <- function() x
  #setin(M) will sets the invert matrix in cache
  setinv <- function(M) xi <<- M
  #getinv() returns the invert matrix available from cache
  getinv <- function() xi
  #the four services are  placed in a list which is returned
  #by makeCacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# the function cacheSolve receives as input a matrix constructor
# where the input matrix and possibly the inverted matrix are stored
# CacheSolve checks if the invert matrix is already computed
# If yes, it returns the invert matrix as stored in cache.
# If not, it computes the invert matrix and places it into the cache
# through the setinv service of makeCacheMatrix.
cacheSolve <- function(M, ...) {
  #get the input matrix from the cache matrix maker
  Mi <- M$getinv()
  #has the invert matrix already been computed ?
  if(!is.null(Mi)) {
      #Yes, then inform the user and return the cache data
      message("getting cached data")
      return(Mi)
  }
  #No, then fetch the input matrix
  data <- M$get()
  #compute the invert
  Mi <- solve(data, ...)
  #store it in cache
  M$setinv(Mi)
  #return the invert as just computed
  Mi
}

#***********************************
#
#       Test section
#               
#**********************************

#initiate Mc as a cache matrix maker object
Mc<-makeCacheMatrix(matrix(c(7,-1,-1,-3,1,0,-3,0,1),3,3))

#check the matrix we have just made and saved
Mc$get()
Mc$getinv() #should be NULL after the initiation

#compute and save the invert
Mc$setinv(solve(Mc$get()))
Mc$getinv() #returns the invert of the matrix passed to the constructor

#change the matrix
Mc$set(matrix(c(1,1,1,3,4,3,3,3,4),3,3))
Mc$getinv() #should be NULL

#compute and cache the invert of the matrix
cacheSolve(Mc)
Mc$get()
Mc$getinv()

#the next call to cacheSolve should return
#the invert matrix without re-computing the inversion
cacheSolve(Mc)

#as a short recap...
Mc$setinv(NULL) #here we reset the invert
cacheSolve(Mc) #cacheSolve should recompute the invert

#the product of the input matrix with its invert
#should result in the neutral matrix.
#The invert should be retrieved from cache.
Mc$get() %*% cacheSolve(Mc)
