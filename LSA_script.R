vec = c(3,1,0,3,2,0,3,0,0,2,2,1)
vec3 = c(2,3,2,1,0,1,1,2,2,3,1,2)
mat = matrix(vec, 4,3)
vec2 = c(2,3,0)

d2

euclid = function(x,y) {
  return(sqrt(sum((y-x)^2)))
}
cosine = function(x,y) {
  top = x %*% y
  bottom = sqrt(x%*%x) %*% sqrt(y%*%y)
  return (top / bottom)
}

ppmi = function(df) { # Turney and Pantel, p. 157
  newDf = df
  total = sum(df)
  bottom = total
  for(i in 1:length(df[,1])){
    sumR = sum(df[i,])
    for(j in 1:length(df[1,])){
      colSum = sum(df[,j])
      pij = (df[i,j] / total)
      pi = sumR / total
      pj = colSum / total
      pmi = log(pij/(pi * pj))
      #browser()
      if (pmi < 0){
        pmi = 0
      }
      newDf[i,j] = pmi
    }
  }
  return(newDf)
}

euclidSim = function(df, term) {
  termVec = df[term,]
  distances = c()
  for(i in 1:length(df[,1])) {
    dist = 0
    for(j in 1:length(df[1,])) {
      if(df[i,] != termVec) {
        dist = dist + (df[i,j]-termVec[j])^2
      }
    }
    #browser()
    distances[i] = sqrt(dist)
    #browser()
    names(distances)[i] = row.names(df)[i]
  }
  return(distances)
}

euclidSim = function(df, term) {
  termVec = df[term,]
  names(termVec) = term
  distances = c()
  for(i in 1:length(df[,1])){
    dist = 0
    if(row.names(df)[i] != names(termVec)){
      dist = euclid(termVec, df[i,])
    }
    distances[i] = dist
    names(distances)[i] = row.names(df)[i]
  }
  return(distances)
}

cosineSim = function(df, term) {
  df = as.matrix(df)
  termVec = df[term,]
  names(termVec) = term
  distances = c()
  for(i in 1:length(df[,1])){
    dist = 0
    if(row.names(df)[i] != names(termVec)){
      dist = cosine(termVec, df[i,])
    }
    distances[i] = dist
    names(distances)[i] = row.names(df)[i]
  }
  return(distances)
}

unitUnit = function(df, termOne, termTwo) {
  i = which(row.names(df) == termOne)
  j = which(row.names(df) == termTwo)
  s = diag(svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$d)
  v = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$v
  u = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$u
  us = u %*% s
  #browser()
  top = u[i,] %*% s %*% s %*% t(u)[,j]
  bottom = sqrt((us[i,] %*% us[i,])) %*% sqrt((us[j,] %*% us[j,]))
  
  return(top/bottom)
}

unitUnitSim = function(df, term) {
  i = which(row.names(df) == term)
  s = diag(svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$d)
  v = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$v
  u = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$u
  us = u %*% s
  terms = c()
  for(j in 1:length(u[,1])){
    dist = 0
    if(j != i) {
      top = top = u[i,] %*% s %*% s %*% t(u)[,j]
      bottom = sqrt((us[i,] %*% us[i,])) %*% sqrt((us[j,] %*% us[j,]))
      dist = top / bottom
      terms[j] = dist
      #browser()
      names(terms)[j] = row.names(df)[j]
    }
  }
  return(terms)
}

compComp = function(df, d1, d2) {
  i = which(names(df) == d1)
  j = which(names(df) == d2)
  s = diag(svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$d)
  v = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$v
  u = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$u
  
  vs = v %*% s
  top = v[i,] %*% s %*% s %*% t(v)[,j]
  bottom = sqrt(vs[i,] %*% vs[i,]) %*% sqrt(vs[j,] %*% vs[j,])
  return(top / bottom)
}

compCompSim = function(df, d1) {
  i = which(names(df) == d1)
  s = diag(svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$d)
  v = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$v
  u = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$u
  docs = c()
  vs = v %*% s
  for(j in 1:length(v[1,])) {
    dist = 0
    if(j != i) {
      top = v[i,] %*% s %*% s %*% t(v)[,j]
      bottom = sqrt(vs[i,] %*% vs[i,]) %*% sqrt(vs[j,] %*% vs[j,])
      dist = top / bottom
      docs[j] = dist
      names(docs)[j] = names(df)[j]
    }
  }
  return(docs)
}

unitComp = function(df, term, doc) {
  s = diag(svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$d)
  v = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$v
  u = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$u
  i = which(row.names(df) == term)
  j = which(names(df) == doc)
  
  us = u %*% sqrt(s)
  vs = v %*% sqrt(s)
  top = u[i,] %*% s %*% t(v)[,j]
  bottom = sqrt(us[i,] %*% us[i,]) %*% sqrt(vs[j,] %*% vs[j,])
  return(top/bottom)
}

unitCompSim = function(df, term="", comp="") {
  s = diag(svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$d)
  v = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$v
  u = svd(df, nv=min(length(df[1,]), length(df[,1])), nu=min(length(df[1,]), length(df[,1])))$u
  us = u %*% sqrt(s)
  vs = v %*% sqrt(s)
  if(term != "") {
    i = which(row.names(df) == term)
    docs = c()
    for(j in 1:length(v[1,])) {
      dist = 0
      top = u[i,] %*% s %*% t(v)[,j]
      bottom = sqrt(us[i,] %*% us[i,]) %*% sqrt(vs[j,] %*% vs[j,])
      docs[j] = top / bottom
      names(docs)[j] = names(df)[j]
    }
    return(docs)
  } else if(comp != "") {
    i = which(names(df) == comp)
    docs = c()
    for(j in 1:length(u[,1])) {
      dist = 0
      top = u[j,] %*% s %*% t(v)[,i]
      bottom = sqrt(us[j,] %*% us[j,]) %*% sqrt(vs[i,] %*% vs[i,])
      docs[j] = top / bottom
      names(docs)[j] = row.names(df)[j]
    }
    return(docs)
  } else {
    echo("Please provide only one value.")
  }
}

df = as.matrix(read.csv('shakespeare.csv', row.names=1))
s = svd(df, nv=5, nu=5)
v = s$v
u = s$u
s = diag(s$d[1:5])


