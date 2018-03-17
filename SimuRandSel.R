#' SimuSel function to calcuate probabiliy of identifying
#' superior varieties multipe times in a row
#'
#' @param nsim the number of simulations
#' @param ngen the number of generations of selection and number of attempts
#' @param the number of total selection candidates each breeding cycle
#' @param the heritability of the trait
#' @param the genetic variance
#' @param the number of individuals selected each breeding cycle
#' @return the probability of identifing a superior variety ngen times in a row
#' @export
#'
SimuRandSel<- function(nsim=100000, ngen=3, n=100, h2, Vg=1){
Ve<- Vg/h2- Vg
stdG<- sqrt(Vg)
stdE<- sqrt(Ve)
succall<-c()
for(j in 1:nsim){
g<- rnorm(n,sd=stdG)
e<- rnorm(n,sd=stdE)
p<- g+e
val0<- g[which.max(p)]
succ<-c()
for(i in 1:c(ngen-1)){
  g<- rnorm(n,sd=stdG)
  e<- rnorm(n,sd=stdE)
  p<- g+e
  val<- g[which.max(p)]
  suc<- val>val0
  if(suc){
    val0<- val
  }
  succ<- append(succ, suc)
}
succall<- append(succall, !FALSE %in% succ)
}
probsuccess<- sum(succall)/nsim
return(probsuccess)
}