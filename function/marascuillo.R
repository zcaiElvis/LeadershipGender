
marascuillo <- function(lp, n, alpha=0.05){
  k <- length(lp)
  lp_comb <- combn(k, m=2)
  
  sqrt_chisq <- sqrt(qchisq(p=1-alpha, df=k-1))
  output <- data.frame(pi = lp_comb[1,], pj = lp_comb[2,], rij = seq(1,ncol(lp_comb)),
                       pipj = seq(1,ncol(lp_comb)))
  
  for(i in 1:ncol(lp_comb)){
    ith <- lp_comb[1,i]
    jth <- lp_comb[2,i]
    
    pi <- lp[ith]
    ni <- n[ith]
    pj <- lp[jth]
    nj <- n[jth]
    
    rij <- sqrt_chisq* sqrt((pi*(1-pi))/ni + (pj*(1-pj))/nj)
    pipj <- abs(pi-pj)
    
    output[i,3] = rij
    output[i,4] = pipj
  }
  return(output)
}