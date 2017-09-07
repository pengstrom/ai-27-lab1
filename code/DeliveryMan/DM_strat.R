library(combinat)
library(DeliveryMan)

manhattanDist <- function(x1, y1, x2, y2) {
  return(abs(x2-x1)+abs(y2-y1))
}

#Calculates the distance from a point (x,y) to the package P's pickup point, and then to its delivery point.
manhattanPackageDist <- function(x, y, P){
  return(manhattanDist(x,y,P[1],P[2]) + manhattanDist(P[1],P[2], P[3],P[4]))
}

permDistance <- function(car, packages, permutation) {
  dist = 0
  
  #print(permutation)
  for (i in 0:max(1,(length(permutation)-1))) {
    A = permutation[i]
    B = permutation[i+1]
    if (i == 0) {
      dist = dist + manhattanPackageDist(car$x, car$y, packages[B, ])
    }
    else {
      dist = dist + manhattanPackageDist(packages[A,3], packages[A,4], packages[B, ])
    }
    #print(dist)
  }
  return(dist)
}

strategy <- function(roads,car,packages) {
  
  if (car$load > 0) {
    package = packages[car$load, ]
    target = findShortestPath(roads, car$x, car$y, package[3], package[4])
    nextMove = 5
    if (car$x<target[1]) {nextMove=6}
    else if (car$x>target[1]) {nextMove=4}
    else if (car$y<target[2]) {nextMove=8}
    else if (car$y>target[2]) {nextMove=2}
    
    car$nextMove=nextMove
    return(car)
  }
  else {
    
    packages = packages[packages[,5] == 0, ]
    
    min = .Machine$integer.max
    nrOfPackages = max(1,nrow(packages));
    
    nextPackage = packages
    if (nrOfPackages > 1) {
      #permutations = permutations(1:nrOfPackages)
      permutations = permn(1:nrOfPackages)
      bestPerm = permutations[1]
      
      for (p in permutations) {
        dist = permDistance(car, packages, p)
        if (dist < min) {
          min = dist
          bestPerm = p
        }
      }
      nextPackage = packages[bestPerm[1], ]
    }
    
    target = findShortestPath(roads, car$x, car$y, nextPackage[1], nextPackage[2])
    
    nextMove = 5
    if (car$x<target[1]) {nextMove=6}
    else if (car$x>target[1]) {nextMove=4}
    else if (car$y<target[2]) {nextMove=8}
    else if (car$y>target[2]) {nextMove=2}
    
    car$nextMove=nextMove
    return(car)
  }
  
}

# Returns a matrix whose rows are the permutations of v in lexicographical
# order. Not safe to run on the empty vector.
permutations <- function(v) {
  ps <- c()
  p <- sort(v)
  ps <- rbind(ps, p)
  while (any(p[-length(p)] < p[-1])) {
    k <- max(which(p[-length(p)] < p[-1]))
    l <- max(which(p[k] < p))
    p[c(k, l)] <- p[c(l, k)]
    t <- (k + 1) : length(p)
    p[t] <- rev(p[t])
    ps <- rbind(ps, p)
  }
  return(ps)
}

#returns a list of coordinates that make the shortest path
findShortestPath <- function(roads, fromX, fromY, toX, toY) {
  if (fromX == toX | fromY == toY) return(c(toX, toY));
  
  hCost = 0
  vCost = 0
  if (fromX < toX) {
    hCost = roads$hroads[fromY, fromX] 
  }
  else {
    hCost = roads$hroads[fromY, fromX-1]
  }
  
  if (fromY < toY) {
    vCost = roads$vroads[fromY, fromX] 
  }
  else {
    vCost = roads$vroads[fromY-1, fromX]
  }
  
  if (vCost > hCost) {
    return(c(toX, fromY))
  }
  else {
    return(c(fromX, toY))
  }
}

runDeliveryMan(strategy)