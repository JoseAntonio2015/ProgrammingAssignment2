## A function to cache the inverse of a matrix, and a function to calculate the 
## inverse of a matrix, if it has already been calculated then retrieves it from
## the cache, if not the inverse is computed and cached. 
## if X is a matrix, its inverse is X(-1) in this context

## 1.-It caches the inverse of a matrix

makeCacheMatrix<-function(A=matrix()){
 
  setMatrixInv<<-function(B=matrix()){ 
    
    A<<-B               
    
    matrInv<<-solve(A)  
    
  }
  
  getMatrix<<-function(){
    
    A
    
  }
  
  matrInv<<-solve(A) 
  
  
  getInv<<-function(){
    
    matrInv
    
  }
  
  list(setMatrixInv=setMatrixInv, getMatrix=getMatrix, getInv=getInv)
  
}

## 2. if X(-1)=A(-1) and A(-1) is cached, it retrieves A(-1) from the cache, if 
## not, it calculates X(-1)

cacheSolve<-function(X=matrix()){
  
  if((!is.null(getInv())) & (identical(X,getMatrix()))){
    
    return(getInv())  # you may also type "return(matrInv)"
    
  }else {
    
    setMatrixInv(X)
    
    matrInv
  }
}

## 3. some square invertible matrices to practice

L<-matrix(c(1,2,-1,2,2,2,-1,1,-1,-1,1,-1,2,1,-1,2),4,4)

M<-matrix(c(1,3,1,1,1,3,2,1,1),3,3)

N<-matrix(c(3.732, 1.184, 1.003, 4.327),2,2)

O<-matrix(c(2.34, 3.282, 4.34, 3.34, 48.222, 36.432, 4, 8.42, 92.16), 3, 3)

P<-matrix(c(1,1,1,4),2,2)


## 4. initialize "makeCacheMatrix" function with a sample

z<-makeCacheMatrix(P)
