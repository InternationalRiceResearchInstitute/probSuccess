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
SimuSel<- function(nsim=10, ngen=3, n=100, h2=0.5, Vg=1, nsel=30){
  r<- sqrt(h2)
  p<- nsel/n
  i<- dnorm(qnorm(p))/p
  Ve<- Vg/h2- Vg
  stdG<- sqrt(Vg)
  stdE<- sqrt(Ve)
  succall<-c()
  for(j in 1:nsim){
    mn<- 0
    g<- rnorm(n,sd=stdG)
    e<- rnorm(n,sd=stdE)
    p<- g+e
    val0<- g[which.max(p)]
    succ<-c()
    for(i in 1:c(ngen-1)){
      mn<- mn+c(stdG*i*r)
      g<- rnorm(n,mean=mn, sd=stdG)
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
