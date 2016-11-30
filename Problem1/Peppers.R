#Author: David Lubinsky

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

# 42 Peppers -> 3 of 6, 1 of 11, 1 of 13
peppers(42)

# 55 Peppers -> 5 of 11
peppers(55)

# 27 Peppers -> Cannot be packed
peppers(27)

# 88 Peppers -> Two possible packings
# 8 of 11 and
# 2 of 6, 1 of 11 and 5 of 13 (correct answer for this problem)
peppers(88)



