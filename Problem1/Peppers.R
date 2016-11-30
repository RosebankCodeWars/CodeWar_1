
# Author: David Lubinsky

peppers = function(m){
    s = 0:floor(m/6)
    e = 0:floor(m/11)
    r = 0:floor(m/13)
    counts = expand.grid(s,e,r)
    p = expand.grid(s*6,e*11,r*13)
    v = rowSums(p) == m
    answers = counts[v,]
    best = which(rowSums(answers)== min(rowSums(answers)))
    answers[best,]
}



