
#Author: David Lubinsky

diamonds = function(d,r,c){
    res = matrix("#",r*d,c*d)
    di = diamond(d)
    for(i in 1:r){
        for(j in 1:c){
            res[(1+(i-1)*d):(i*d), (1+(j-1)*d):(j*d)] = di
        }
    }
    for(i in 1:(r*d)) {cat(paste(res[i,],collapse = "")); cat("\n")}
}

diamond = function(d){
    o = matrix("#",d,d)
    m = d/2
    for(i in 1:m){
        s = m - i 
        for(j in (1:i)){
            o[i,j+s] = "/"
            o[i,d-s-j+1] = "\\"
        }
    }
    for(i in 1:m) o[m+i,] = rev(o[m-i+1,])
    o
}
