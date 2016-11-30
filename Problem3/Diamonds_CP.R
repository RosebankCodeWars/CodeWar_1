
# Author: Colin Phillips

# Problem 3: draw a diamond

library(tidyverse)
input = matrix(c(12,3,3,4,1,2,0,0,0), byrow = TRUE, ncol = 3)

drawDiamond = function(size) {
  base = matrix(rep("#", size*size), ncol = size)
  mid = size/2
  df = expand.grid(rows = 1:size, cols = 1:size)
  topleft = df[df$rows <= mid & df$cols <= mid & df$rows + df$cols >= mid + 1,]
  topleft$label = "/"
  topright = df[df$rows <= mid & df$cols > mid & df$cols - df$rows <= mid,]
  topright$label = "\\"
  bottomleft = df[df$rows > mid & df$cols <= mid & df$rows - df$cols <= mid,]
  bottomleft$label = "\\"
  bottomright = df[df$rows > mid & df$cols > mid & 2* size - df$rows - df$cols >= mid - 1,]
  bottomright$label = "/"
  change = rbind(topleft, topright, bottomleft, bottomright)
  for (i in seq_len(nrow(change))) {
    base[change$rows[i], change$cols[i]] = change$label[i]
  }
  return(base)
}

drawDiamondSet = function(size, rows, cols) {
  d = drawDiamond(size)
  myrow = do.call(cbind, rep(list(d), rows))
  myset = do.call(rbind, rep(list(myrow), cols))
  return(myset)
}

flattenDiamonds = function(d) {
  output = vector("list", nrow(d))
  for( i in seq_len(nrow(d))) {
    output[[i]] = paste0(d[i,], collapse = "")
  }
  tp = do.call(paste0, list(output, collapse = "\n"))
  return(tp)
}

ConsumeInput = function(input) {
  for (i in seq_len(nrow(input))) {
    if(all(input[i,] == 0)){
      return()
    }
    s = drawDiamondSet(input[i,1],input[i,3],input[i,2])
    tp = flattenDiamonds(s)
    cat(tp)
    cat("\n\n\n")
  }
}


a = ConsumeInput(input)
