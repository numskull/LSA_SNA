A = c(0,1,0,0,1,0,1,0,1,1,0,0,0,1,0,1,1,1,0,1,1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0) # Matrix 6.2 in Newman
A = matrix(A, 6,6)
dirA = c(0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0) # Matrix 6.6 in Newman (directed)
dirA = matrix(dirA, 6, 6)

cocitation = function(mat) { #formula 
  C = mat %*% t(mat)
  newMat = mat
  for(i in 1:length(C[1,])){
    for(j in 1:length(C[,1])) {
      if(i != j) {
        if(C[i,j] > 0) {
          newMat[i,j] = newMat[i,j] + C[i,j]
        } else {
          newMat[i,j] = 0
        }
      }
    }
  }
  return(newMat)
}

cocitation = function(mat) { # Formula 6.7 in Newman
  newMat = mat
  for(i in 1:length(mat[1,])) {
    for(j in 1:length(mat[,1])) {
      aik = 0
      akj = 0
      for(k in 1:length(mat[1,])) {
        if(i != j) {
          aik = aik + mat[i,k]
          akj = akj + t(mat)[k,j]
          
        }
        newMat[i,j] = aik %*% akj
      }
    }
  }
  return(newMat)
}

bibCoupling = function(mat) {
  B = t(mat) %*% mat
  newMat = mat
  for(i in 1:length(B[1,])) {
    for(j in 1:length(B[,1])){
      if(B[i,j] > 0){
        newMat[i,j] = newMat[i,j] + 1
      }
    }
  }
  return(newMat)
}


degree = function(mat, index) {
  return(sum(mat[index,]))
}

B = c(1,0,0,1,0,1,1,1,1,0,0,1,1,0,1,0,0,1,1,1) # Matrix 6.16
B = matrix(B, byrow = T, 4,5)
g = graph.incidence(B)
plot(g)

m2 = 0
for(i in 1:length(B[,1])) {
  m2 = m2 + degree(B, i)
}

numPaths = function(A, len){ # 6.31
  N = 0
  for(i in 1:length(A[,1])) {
    for(j in 1:length(A[1,])) {
      if((A%^%len)[i,j] == len){
        N = N+1
      }
    }
  }
  return(N)
}
N = numPaths(A, 2)

numLoops = function(A, len){
  return(sum(diag(A %^% len)))
}
L = numLoops(A, 2)

geodesic = function(A, i, j) {
  done = FALSE
  count = 1
  
  while (done == F) {
    aij = A %^% count
    if(aij[i,j] > 0) {
      return(count)
    }
    count = count + 1
  }
}
