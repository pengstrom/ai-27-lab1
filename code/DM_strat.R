library(DeliveryMan)

strategy <- function(roads,car,packages) {
  rows = nrow(roads$hroads)
  cols = ncol(roads$vroads)
  size = rows*cols
  
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
    packages = packages[packages[,5] == 0, ] #discard already delivered packages
    
    min = .Machine$integer.max
    nrOfPackages = max(1,nrow(packages))
    nextPackage = packages #if there is only one package, packages is not a matrix but a vector 
    if (nrOfPackages > 1) {
      permutationMatrix = permutations(1:nrOfPackages)
      bestPerm = permutationMatrix[1, ]
      
      for (r in 1:nrow(permutationMatrix)) {
        p = permutationMatrix[r, ]
        dist = permDistance(car, roads, packages, p, matrix(0, nrow=size, ncol=size))
        
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



#Calculates the distance between (x1,y1) and (x2,y2).
manhattanDist <- function(x1, y1, x2, y2) {
  return(abs(x2-x1)+abs(y2-y1))
}

#Calculates the distance from a point (x,y) to the package P's pickup point, and then to its delivery point.
packageDist <- function(x, y, P){
  return(manhattanDist(x, y, P[1], P[2]) + manhattanDist(P[1], P[2], P[3], P[4]))
}

#Estimates the total (manhattan) distnace required to pick up all packages in the order given by the permutation
permDistance <- function(car, roads, packages, permutation, preCalc) {
  dist = 0
  for (i in 0:max(1,(length(permutation)-1))) {
    A = permutation[i]
    B = permutation[i+1]
    if (i == 0) {
      dist = dist + packageDist(car$x, car$y, packages[B, ])
    }
    else {
      dist = dist + packageDist(packages[A,3], packages[A,4], packages[B, ])
    }
  }
  return(dist)
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
  return(a.star(c(fromX, fromY), c(toX, toY), roads)[[2]]);
}

# A node has a position, a cost, and possibly a parent. We say that two
# noded are equivalent if they share the same position.
node <- function(pos, cost, parent = NULL) {
  list(pos = pos, cost = cost, parent = parent)
}

# Utility funciton. Find the least index i such that n and ns[[i]] are
# equivalent, or else 0.
find <- function(n, ns) {
  for (i in seq_along(ns))
    if (all(n$pos == ns[[i]]$pos))
      return(i)
  return(0)
}

# List the children of a node under given traffic conditions.
children <- function(n, roads) {
  actions  <- list(c(0, 1), c(0, -1), c(-1, 0), c(1, 0))
  in.range <- function(a) all(1 <= n$pos + a & n$pos + a <= 10)
  node.from.action <- function(a) {
    r <- t(roads[[which(a != 0)]])
    i <- rbind(n$pos + pmin(a, 0))
    node(n$pos + a, n$cost + r[i], n)
  }
  lapply(Filter(in.range, actions), node.from.action)
}

# A priority queues is represented by an ordered lists of node-priority
# pairs.

# Update or insert. If an equivalent node with lower priority already
# exists in the queue, do nothing. Otherwise, insert the node with the
# given priority removing any equivalent nodes.
pq.upsert <- function(q, n, p) {
  i <- find(n, lapply(q, '[[', 1))
  if (i) {
    if (p >= q[[i]][2]) return(q)
    q <- q[-i]
  }
  i <- sum(p >= sapply(q, '[[', 2))
  return(append(q, list(list(n, p)), i))
}

# Extract the shortest path, as a list of positions, from the goal node.
extract.path <- function(n) {
  p <- list()
  repeat {
    p <- append(p, list(n$pos), 0)
    if (is.null(n$parent)) return(p)
    n <- n$parent
  }
}

# Textbook graph A*, with heuristic given by the distance.
a.star <- function(start, goal, roads) {
  h <- function(n) sum(abs(n - goal))
  f <- list()
  v <- list()
  f <- pq.upsert(f, node(start, 0), 0)
  repeat {
    n <- f[[1]][[1]]
    f <- f[-1]
    if (all(n$pos == goal)) {
      
      return(extract.path(n))
    }
    v <- append(v, list(n))
    for (c in children(n, roads)) {
      if (find(c, v)) next
      f <- pq.upsert(f, c, c$cost + h(c$pos))
    }
  }
}


runDeliveryMan(strategy)


averageTest <- function(tests){
  sum = 0
  for (i in 1:tests) {
    sum=sum+runDeliveryMan(carReady = strategy, dim = 10, turns = 2000, doPlot = F, pause = 0, del = 5)
    if(i%%10==0){
      print(i)
      print(sum/i)
    }
  }
  print(sum/i)
  return(0)
}
#averageTest(500)