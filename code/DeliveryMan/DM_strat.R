manhattanDist <- function(x1, y1, x2, y2) {
  return(abs(x2-x1)+abs(y2-y1))
}

#Calculates the distance from a point (x,y) to the package P's pickup point, and then to its delivery point.
manhattanPackageDist <- function(x, y, P){
  return(manhattanDist(x,y,P[1],P[2]) + manhattanDist(P[1],P[2], P[3],P[4]))
}

permDistance <- function(car, packages, permutation) {
  dist = 0
  
  for (i in 1:max(1,(length(permutation)-1))) {
    A = permutation[i]
    B = permutation[i+1]
    if (i == 1) {
      dist = dist + manhattanPackageDist(car$x, car$y, packages[A, ])
    }
    else {
      dist = dist + manhattanPackageDist(packages[A,3], packages[A,4], packages[B, ])
    }
  }
  return(dist)
}

strategy <- function(roads,car,packages) {
  
  if (car$load > 0) {
    target = packages[car$load, ]
    nextMove=5
    if (car$x<target[3]) {nextMove=6}
    else if (car$x>target[3]) {nextMove=4}
    else if (car$y<target[4]) {nextMove=8}
    else if (car$y>target[4]) {nextMove=2}
    
    car$nextMove=nextMove
    return(car)
  }
  else {
    
    packages = packages[packages[,5] == 0, ]
    
    min = 100000
    nrOfPackages = max(1,nrow(packages));
    
    
    nextPackage = packages
    if (nrOfPackages > 1) {
      
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
    
    
    
    nextMove = 5
    if (car$x<nextPackage[1]) {nextMove=6}
    else if (car$x>nextPackage[1]) {nextMove=4}
    else if (car$y<nextPackage[2]) {nextMove=8}
    else if (car$y>nextPackage[2]) {nextMove=2}
    
    
    
    car$nextMove=nextMove
    return(car)
  }
  
}

#returns a list of coordinates that make the shortest path
findShortestPath(car, fromX, fromY, toX, toY) {
  #car$vroads - vertical penalties
  #car$hroads - horizontal penalties
}

runDeliveryMan(strategy)

