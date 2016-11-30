
#Author: Colin Philips

#Problem 1: peppers

packing = function(p) {
  minpacks = floor(p/13)
  maxpacks = ceiling(p/5)
  best = c(0,0,0,p)
  for(sixes in 0:maxpacks){
      maxelevens = ceiling((p - 6*sixes)/11)
      for(elevens in 0:maxelevens) {
        total = 6*sixes + 11*elevens
        rem = p - total
        if(rem %% 13 == 0){
          thirteens = rem/13
          numpacks = sixes + elevens + thirteens
          if(numpacks < best[4] | (numpacks == best[4] & thirteens > best[3])) {
            best = c(sixes, elevens, thirteens, numpacks)
          }
        }
      }
  }
  if(best[4] == p) {
    cat(paste(p, " peppers cannot be packed"))
  } else {
    st = paste0(p," peppers can be packed most economically in:\n", best[3]," packages of 13\n",
                best[2]," packages of 11\n",
                best[1], " packages of 6\n",
                best[4], " total packages")
    cat(st)
  }
}
