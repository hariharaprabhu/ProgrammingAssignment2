#makeCacheMatrix function contains 4 sub functions set,get,setsolve,getsolve

#the set function will take the value of the input (say x here) and store it in a matrix

#the i is the name of the matrix where we are going to store the inverse of the input

#the value of i is initially set to null so that if there exists a pre value of i it will not be create confusion over here

#now the value of the matrix is set, the get function is used to return the matrix stored in the set function

#the setsolve function is use to compute the inverse of the current matrix, the solve command is used to find the inverse

#note for finding the inverse of something the matrix should be a square matrix

#the get solve function is used to return the value stored in the matrix i

#the list is used to store the 4 functions 


makeCacheMatrix <- function(x = matrix())

  {
      
    i <- NULL
    
    set <- function(y) 
      
      {
        
        x <<- y
        
        i <<- NULL
      }
  
     get <- function() x
     
     setsolve <- function(solve) i <<- solve
   
     getsolve <- function() i
  
     list(set = set, get = get,
          
          setsolve = setsolve,
          
          getsolve = getsolve)
  
  
  }

#the cache solve function contains 3 functions getsolve, get and setsolve

#this function is used to check if a matrix inverse has already been computed 

#the getsolve() will check if a matrix inverse i is already available, 
# if so it displays it current or the cached value of the matrix i

#if the matrix inverse is not determined before then the get function will return the value stored in x

#the solve function will compute the inverse of the matrix

#the setsolve function will store the inverse of the matrix value into i

#finally the matrix i which is the inverse of the input x is returned



cacheSolve <- function(x, ...) {
                                  
                                  i <- x$getsolve()
                                  
                                  if(!is.null(i)) {
                                                    
                                                    message("getting cached data")
                                    
                                                    return(i)
                                                   
                                                    }
                                  
                                  data <- x$get()
                                  
                                  i <- solve(data, ...)
                                  
                                  x$setsolve(i)
                                  
                                  i
                                
                                  }

