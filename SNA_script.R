A = c(0,1,0,0,1,0,1,0,1,1,0,0,0,1,0,1,1,1,0,1,1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0) # Matrix 6.2 in Newman
A = matrix(A, 6,6)
dirA = c(0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,0) # Matrix 6.6 in Newman (directed)
dirA = matrix(dirA, 6, 6)

cocitation = function(mat) { # Formula 6.8 in Newman.
  C = mat %*% t(mat)
  return(C)
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

betweenness = function(A) {
  geos = A
  bets = c()
  bet = c()
  for(i in 1:nrow(A)){
    for(j in 1:ncol(A)) {
      geos[i,j] = geodesic(A,i,j)
    }
  }
  
  for(s in 1:nrow(A)) {
    for(t in 1:ncol(A)) {
      bet[t] = 0
      for(i in 1:nrow(A)) {
        if(geos[s,i] > 0 && geos[t,i] > 0){
          bet[t] = bet[t] + 1
        }
      }
      bet[t] = bet[t]
    }
    bets[s] = sum(bet)/ geos[s,t]
  }
  return(bets)
}

bibCoupling = function(mat) { # Newman 6.11
  B = t(mat) %*% mat
  return(B)
}

# bibCoupling = function(mat) {
#   B = t(mat) %*% mat
#   newMat = mat
#   for(i in 1:length(B[1,])) {
#     for(j in 1:length(B[,1])){
#       if(B[i,j] > 0){
#         newMat[i,j] = newMat[i,j] + 1
#       }
#     }
#   }
#   return(newMat)
# }


degree = function(mat, index) { # Newman 6.19
  return(sum(mat[index,]))
}

B = c(1,0,0,1,0,1,1,1,1,0,0,1,1,0,1,0,0,1,1,1) # Matrix 6.16
B = matrix(B, byrow = T, 4,5)
g = graph.incidence(B)
plot(g)

m2 = 0
for(i in 1:length(B[,1])) { # Newman 6.20
  m2 = m2 + degree(B, i)
}

numPaths = function(A, len){ # Newman 6.31
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

### Trying Random Walk stuff
p = A %*% solve(D) %*% P # 6.58
p
L = D-A
pp = L %*% solve(D) %*% p # 6.59
pp

eigenCent = function(A) {
  x = degreeCent(A)
  E = c()
  k = max(as.numeric(eigen(A)$values)) # Directed graphs give complex number values.
  for(i in 1:nrow(A)) {
    E[i] = (A[i,] %*% x)
    #browser()
    E[i] = k^-1 * E[i]
    names(E)[i] = row.names(A)[i]
  }
  return(E)
}

katz = function(A) { # 7.12 (7.9) in Newman
  k = max(as.numeric(eigen(A)$values))  # Directed graphs give complex number values.
  alpha = 1/k - .05 # Slightly less than 1/max eigenvalue
  B = rep(1, nrow(A))
  x = rep(0, nrow(A)) #start with initial bad estimate of centrality
  for(i in 1:ncol(A)) {
    x = alpha*(A %*% x) + B
  }
  return(x)
}

pageRank = function(A) { # Eq. 7.17 in Newman
  D = diag(degreeCent(A))
  alpha = 1/(max(as.numeric(eigen(A)$values)) - .05)
  B = rep(1, nrow(A))
  x = D%*%solve(D-(alpha * A))%*%B
  row.names(x) = row.names(A)
  return(x)
}

cluster = function(A){
  top = 0
  bottom = numPaths(A, 2)
  for(i in 1:nrow(A)){
    for(j in 1:ncol(A)){
      for(k in 1:ncol(A)){
        if(i !=j && j != k && i != k){
          if(A[i,k] > 0 && A[j,k] > 0 && A[i,j] > 0){
            top = top + 1
          }
        }
      }
    }
  }
  C = top / bottom
  return(C)
}

newdf = c()
for(i in 1:nrow(df)){
  newdf[i] = do.call(paste, c(as.list(df[i,]), sep="; "))
}
mat = matrix(0, length(newdf), length(newdf))
names(mat) = newdf
row.names(mat) = newdf

for(i in 1:length(namesList)) {
  print(i)
  names = namesList[[i]]
  if(length(names) > 1) {
    prev = c()
    for(j in 1:length(names) - 1) {
      if(j > 1) {
        prev = c(prev, names[j-1])
      }
      jname = which(row.names(mat) == names[j])
      iname = which(row.names(mat) == names[j+1])
      mat[jname,iname] = 1
      if(length(prev) > 1) {
        for(k in 1:length(prev)) {
          prevName = which(row.names(mat) == prev[k])
          mat[jname, prevName] = 1
        }
      }
    }
  }
}

namesVec = c()
idsVec = c()
edges = data.frame()
B = data.frame()
for(i in 1:length(namesList)) {
  if(length(namesList[[i]]) > 0) {
    names = namesList[[i]]
    for(j in 1:length(names)){
      namesVec = c(namesVec, names[j])
      idsVec = c(idsVec, names(namesList)[i])
    }
  }
}
library(Matrix)
B = Matrix(0, 177720, 177720)
colnames(B) = edges$idsVec
rownames(B) = edges$namesVec
edges$namesVec = as.character(edges$namesVec)
edges$idsVec = as.character(edges$idsVec)
for (i in 1:nrow(edges)) {
  person = edges[i,1]
  book = edges[i,2]
  B[person,book] = 1
}

cumDist = function(md) {
  md = sort(md, decreasing = T)
  dist = c()
  for(i in 1:length(md)) {
    dist[i] = i/length(md)
  }
  return(dist)
}

md = c()
for(i in 1:nrow(B)) {
  md[i] = degree(B, i)
}

moment = function(md, m) { #8.20
  km = 0
  for(i in 1:length(md)){
    km =  km + (md[i]^m)
  }
  return(1/length(md) * km)
}


clusterDist = function(n, md) { #8.24 Gives the clustering expected by random chance.
  C = 0
  top = (moment(md,2) - moment(md,1))^2
  bottom = moment(md,3)
  C = 1/n* (top/bottom)
  return(C)
}

library(rARPACK)
kat = katz(p2p, eigs(p2p,1, which="LM")$values[1])