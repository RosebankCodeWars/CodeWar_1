#Author: Colin Philips 

#Problem 2: Giraffes
library(tidyverse)
giraffes = 3
df = data_frame(Order =  1:3, Name = c("Irving","George","Geoffrey"), Height = c(10,12,15), minsteps = 0)
fieldrows = 4
fieldcols = 4
initialstate = matrix(c(100,5,11,11,11,6,11,11,11,11,11,11,11,14,11,100), ncol = fieldcols, byrow = TRUE)
moves = data_frame(Name = c("U","L","D","R"), x = c(-1,0,1,0), y = c(0,-1,0,1))
moves = data_frame(Name = c("D","R","U","L"), x = c(1,0,-1,0), y = c(0,1,0,-1))
bignum = fieldrows*fieldcols*giraffes*prod(df$Height) + 1
mingm = fieldrows + fieldcols  - 2 # min walking steps for each giraffe
df$minsteps = mingm
for (g in 2:giraffes) {
  # assume the entire field has been eaten by all previous giraffes
  mystate = initialstate
  mystate[mystate < max(df$Height[1:(g - 1)])] = max(df$Height[1:(g - 1)])
  # best case scenario, I only visit the highest mingm sites plus start and finish
  bestvisits = sort(mystate, decreasing = TRUE)[1:(mingm + 2)]
  besteats = sum(pmax(0,(df$Height[g] - bestvisits)))
  df$minsteps[g] = df$minsteps[g] + besteats
}
options(expressions = bignum)

state = list(positions = data_frame(gid = 1:3, x = 1, y = 1),
             fieldstate = initialstate,
             g = 1,
             movehistory = vector("list",giraffes),
             num_moves = 0,
             positionhistory = data_frame(gid = numeric(), x = numeric(), y = numeric())
             )
overallbestmoves = bignum
updateState = function(currentstate, bestmovessofar) {
  # unpack
  positions = currentstate$positions
  fieldstate = currentstate$fieldstate
  g = currentstate$g
  movehistory = currentstate$movehistory
  num_moves = currentstate$num_moves
  positionhistory = currentstate$positionhistory
  cat("Best solution so far: ", bestmovessofar, " moves \n")
  for (i in 1:giraffes) {
    cat("Giraffe ",i,": \n")
    cat(movehistory[[i]], "\n")
  }
  print(positions)
  print(fieldstate)
  cat("Giraffe: ",g, "\n")
  cat("Current moves:", num_moves, "\n")
  
  # if we have already exceeded the best soluton to date, then stop and die
  minremaining = ifelse(g < giraffes, sum(df$minsteps[(g+1):giraffes]),0)
  if (num_moves + minremaining >= bestmovessofar - 1) {
    return(NULL)
  }
  # if the last giraffe is at the finish point, then stop and give an answer
  if (positions$x[giraffes] == fieldrows & positions$y[giraffes] == fieldcols) {
    return(currentstate)
  }
  # if the current giraffe is at the finish point, then start the next giraffe
  if (positions$x[g] == fieldrows & positions$y[g] == fieldcols) {
    g = g + 1
    currentstate$g = g
  }
  # if the current giraffe is able to eat, add all eat steps to the history
  if (fieldstate[positions$x[g], positions$y[g]] <= df$Height[g]) {
    eatsteps = df$Height[g] - fieldstate[positions$x[g], positions$y[g]] 
    movehistory[[g]] = c(movehistory[[g]], rep("E", eatsteps))
    fieldstate[positions$x[g], positions$y[g]] = df$Height[g]
    num_moves = num_moves + eatsteps
    if (num_moves + minremaining  > bestmovessofar) {
      return(NULL)
    }
  }
  #update the positionhistory with the current position
  positionhistory = rbind(positionhistory, positions[g,])
  #repack current state
  currentstate = list(positions = positions,
                      fieldstate = fieldstate,
                      g = g,
                      movehistory = movehistory,
                      num_moves = num_moves, 
                      positionhistory = positionhistory)
  # for the current giraffe, evaluate all moves and return the state from the best move
  nextstates = vector("list", nrow(moves))
  nextmoves = rep(bignum, nrow(moves))
  for (m in seq_len(nrow(moves))) {
    move = moves[m,]
    newstate = currentstate
    # update the giraffe position, if possible
    newx = newstate$positions$x[newstate$g] + move$x[1]
    newy = newstate$positions$y[newstate$g] + move$y[1]
    #check this position against positionhistory
    before = positionhistory %>% filter(gid == g, x == newx, y == newy)
    if (newx >= 1 & newy >= 1 & newx <= fieldrows & newy <= fieldcols & nrow(before) == 0) {
      # add the move to history
      newstate$movehistory[[newstate$g]] = c(newstate$movehistory[[newstate$g]], move$Name[1])
      # update the number of moves
      newstate$num_moves = newstate$num_moves + 1
      # update the giraffe position
      newstate$positions$x[newstate$g] = newx
      newstate$positions$y[newstate$g] = newy
      # update the state
      nextstate = updateState(newstate, bestmovessofar)
      nextstates[[m]] = nextstate
      if (!is.null(nextstate)) {
        nextmoves[m] = nextstate$num_moves
        bestmovessofar = min(bestmovessofar, nextstate$num_moves)
      }
      
    }
  }
  # pick the best state
  if (min(nextmoves) == bignum) {
    return(NULL)
  }
  beststate = nextstates[[which.min(nextmoves)]]
  return(beststate)
}

(best = updateState(state, bignum))