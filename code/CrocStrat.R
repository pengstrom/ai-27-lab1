stratCroc = function(moveInfo,readings,positions,edges,probs) {
  ponds = nrow(probs[[1]])
  
  oldProb = NA
  dist = NA
  transitionMatrix = NA
  if (length(moveInfo$mem) == 0) {
    oldProb = rep(1, ponds)
    transitionMatrix = makeTransitionMatrix(ponds, edges)
    moveInfo$mem[[3]] <- transitionMatrix
    dist = floydWarshall(ponds, transitionMatrix)
    moveInfo$mem[[4]] <- dist
  }
  else {
    oldProb = moveInfo$mem[[1]]
    lastCheck = moveInfo$mem[[2]]
    transitionMatrix = moveInfo$mem[[3]]
    dist = moveInfo$mem[[4]]
    oldProb[lastCheck] = 0
  } 
  
  probPerPond = probPonds(readings, probs)
  probPerPond = probPerPond/sum(probPerPond)
  
  for (tourist in 1:2)
  if (!is.na(positions[tourist])) {
    if (positions[tourist] < 0) {
      probPerPond = rep(0, ponds)
      probPerPond[-positions[tourist]] = 1
    }
    else {
      probPerPond[positions[tourist]] = 0    
    }
  }
  
  
  
  newProb = oldProb%*%transitionMatrix
  newProb = newProb*probPerPond
  newProb = newProb/sum(newProb)
  
  validMoves = c(positions[3], edges[edges[,1] == positions[3], 2], edges[edges[,2] == positions[3], 1])
  moveTo = getBestMove(validMoves, newProb, dist)
  
  validChecks = c(moveTo, edges[edges[,1] == moveTo, 2], edges[edges[,2] == moveTo, 1])
  check = getBestMove(validChecks, newProb, dist)
  
  if (check == moveTo) {
    check = 0
  }
  if (moveTo == positions[3]) {
    moveTo = 0
  }
  
  
  moveInfo$mem[[1]] <- newProb
  moveInfo$mem[[2]] <- check
  moveInfo$moves <- c(moveTo, check)
  
  return(moveInfo)
}

getBestMove = function(validMoves, probs, dist) {
  targets = rep(0, length(probs))
  maxDist = max(dist)
  for (target in validMoves) {
    for (pond in 1:length(probs)) {
      targets[target] = targets[target] + probs[pond]/(1+dist[target, pond])
    }
  }
  return(which.max(targets))
}

makeTransitionMatrix = function(nrOfNodes, edges) {
  transitionMatrix = matrix(0, nrow = nrOfNodes, ncol = nrOfNodes)
  for (x in 1:nrOfNodes) {
    targets = c(x, edges[edges[,1] == x, 2], edges[edges[,2] == x, 1])
    p = 1.0/length(targets)
    for (n in targets) {
      transitionMatrix[x,n] <- p
    }
  }
  return(transitionMatrix)
}

floydWarshall = function(nrOfNodes, transitionMatrix) {
  dist = matrix(.Machine$integer.max, nrow = nrOfNodes, ncol = nrOfNodes)
  for (i in 1:length(transitionMatrix)) {
    if (transitionMatrix[i] > 0) {
      dist[i] <- 1
    } 
  }
  for (v in 1:nrOfNodes) {
    dist[v,v] <- 0
  }
  for (k in 1:nrOfNodes) {
      for (i in 1:nrOfNodes) {
        for (j in 1:nrOfNodes) {
          if (dist[i,j] > dist[i,k] + dist[k,j]) {
            dist[i,j] <- dist[i,k] + dist[k,j]
          }
        }
      }
  }
  return(dist)
}


probPonds = function(readings, probs) {
  results = c()
  for (i in 1:nrow(probs[[1]])) {
    result = 0
    for (j in 1:3) {
      result = result + dnorm(readings[j], probs[[j]][i,1], probs[[j]][i,2])
    }
    results <- c(results, result)
  }
  return(results)
}


runWheresCroc(makeMoves=stratCroc, showCroc = T)
