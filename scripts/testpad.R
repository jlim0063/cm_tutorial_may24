calculate_Q <- function(eta = 1, old_Q = .5, reward){
  new_Q <- old_Q + eta*(reward - old_Q)
  return(new_Q)
}


calculate_prA <- function(QA, QB, beta){
  prA <- exp(beta*(QA))/(exp(beta*QA) + exp(beta*QB))
  return(prA)
}

calculate_prA <- function(QA, QB, beta){
  prA <- 1/(1 + exp(-beta*(QA-QB)))
  return(prA)
}
