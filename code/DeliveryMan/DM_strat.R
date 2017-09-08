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

# List the permutations of a vector in lexicographical order.
permutations <- function(v) {
    if (!length(v)) return(list(c()))
    p <- sort(v)
    ps <- list(p)
    while (any(p[-length(p)] < p[-1])) {
        k <- max(which(p[-length(p)] < p[-1]))
        l <- max(which(p[k] < p))
        p[c(k, l)] <- p[c(l, k)]
        t <- (k + 1) : length(p)
        p[t] <- rev(p[t])
        ps <- append(ps, list(p))
    }
    return(ps)
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
        if (all(n$pos == goal)) return(extract.path(n))
        v <- append(v, list(n))
        for (c in children(n, roads)) {
            if (find(c, v)) next
            f <- pq.upsert(f, c, c$cost + h(c$pos))
        }
    }
}

test.a.star <- function() {
    hroads <- c(1, 7, 15, 2, 23, 7, 9, 8, 1, 4, 2, 14, 4, 5, 15, 2, 2, 8, 11, 5, 10, 5, 3, 9, 12, 8, 6, 4, 13, 4, 7, 4, 35, 14, 2, 1, 1, 11, 7, 5, 20, 8, 5, 2, 15, 10, 5, 2, 9, 4, 8, 3, 3, 7, 20, 15, 6, 3, 1, 4, 13, 15, 19, 26, 3, 17, 1, 1, 1, 11, 1, 21, 3, 1, 10, 6, 6, 3, 3, 15, 2, 15, 11, 11, 15, 9, 15, 7, 11, 7)
    vroads <- c(20, 18, 11, 4, 8, 15, 1, 9, 12, 7, 12, 9, 8, 6, 2, 2, 12, 7, 12, 5, 6, 12, 9, 2, 2, 2, 5, 5, 17, 18, 7, 9, 1, 18, 6, 7, 14, 11, 8, 11, 30, 9, 3, 13, 10, 7, 8, 19, 4, 14, 7, 2, 9, 2, 15, 19, 17, 10, 1, 6, 8, 8, 20, 23, 11, 10, 8, 2, 3, 9, 13, 16, 7, 12, 2, 2, 4, 11, 7, 4, 3, 6, 8, 1, 9, 4, 20, 13, 2, 4)
    roads <- list(
        hroads = matrix(hroads, nrow = 10),
        vroads = matrix(vroads, ncol = 10)
    )
    all(
        all.equal(a.star(c(1, 1), c(10, 10), roads), list(c(1, 1), c(2, 1), c(3, 1), c(3, 2), c(3, 3), c(3, 4), c(4, 4), c(4, 5), c(4, 6), c(4, 7), c(5, 7), c(5, 8), c(6, 8), c(7, 8), c(8, 8), c(9, 8), c(10, 8), c(10, 9), c(10, 10))),
        all.equal(a.star(c(1, 10), c(10, 1), roads), list(c(1, 10), c(2, 10), c(3, 10), c(3, 9), c(3, 8), c(3, 7), c(4, 7), c(5, 7), c(6, 7), c(7, 7), c(8, 7), c(8, 6), c(8, 5), c(8, 4), c(9, 4), c(9, 3), c(9, 2), c(9, 1), c(10, 1)))
    )
}
