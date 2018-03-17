#Function to simulate random selection
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

#########
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

a2<- SimuRandSel(10000, 2, 1000, 0.5) 
a3<- SimuRandSel(10000, 3, 1000, 0.5)
a4<- SimuRandSel(10000, 4, 1000, 0.5)
a5<- SimuRandSel(10000, 5, 1000, 0.5)

b2<- SimuRandSel(10000, 2, 1000, 1) 
b3<- SimuRandSel(10000, 3, 1000, 1)
b4<- SimuRandSel(10000, 4, 1000, 1)
b5<- SimuRandSel(10000, 5, 1000, 1)




hs<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
rslts<- matrix(nrow=length(hs), ncol=4)
for(i in 1:length(hs)){
  c2<- SimuSel(nsim=100000, ngen=2, n=1000, h2=hs[i], nsel=30)
  c3<- SimuSel(nsim=100000, ngen=3, n=1000, h2=hs[i], nsel=30)
  c4<- SimuSel(nsim=100000, ngen=4, n=1000, h2=hs[i], nsel=30)
  c5<- SimuSel(nsim=100000, ngen=5, n=1000, h2=hs[i], nsel=30)
  rslts[i,]<- c(c2, c3, c4, c5)
}

row.names(rslts)<- paste('h2s', hs, sep="_")
colnames(rslts)<- paste('gen', c(2:5), sep="_")


hs<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
rslts2<- matrix(nrow=length(hs), ncol=4)
for(i in 1:length(hs)){
  c2<- SimuSel(nsim=100000, ngen=6, n=1000, h2=hs[i], nsel=30)
  c3<- SimuSel(nsim=100000, ngen=7, n=1000, h2=hs[i], nsel=30)
  c4<- SimuSel(nsim=100000, ngen=8, n=1000, h2=hs[i], nsel=30)
  c5<- SimuSel(nsim=100000, ngen=9, n=1000, h2=hs[i], nsel=30)
  rslts2[i,]<- c(c2, c3, c4, c5)
}

row.names(rslts2)<- paste('h2s', hs, sep="_")
colnames(rslts2)<- paste('gen', c(6:9), sep="_")



#set A
#1/2 #one generation

0.5 * 1/3 
#1/6 # 1/c(2*3) #two generations
#1/24 # 1/c(6*4) #three generations
#1/120 # 1/c(24*5) #our generations






