library(tikzDevice)

clr1<-rgb(27/255,158/255,119/255)
clr2<-rgb(217/255,95/255,2/255)
clr3<-rgb(117/255,112/255,179/255)
clr4<-rgb(231/255,41/255,138/255)
clr5<-rgb(102/255,166/255,30/255)
clr6<-rgb(230/255,171/255,2/255)
clr7<-rgb(166/255,118/255,29/255)

clr1b<-rgb(27/255,158/255,119/255,0.2)
clr2b<-rgb(217/255,95/255,2/255,0.2)
clr3b<-rgb(117/255,112/255,179/255,0.2)
clr4b<-rgb(231/255,41/255,138/255,0.2)
clr5b<-rgb(102/255,166/255,30/255,0.2)
clr6b<-rgb(230/255,171/255,2/255,0.2)
clr7b<-rgb(166/255,118/255,29/255,0.2)

clr1c<-rgb(27/255,158/255,119/255,0.5)
clr2c<-rgb(217/255,95/255,2/255,0.5)
clr3c<-rgb(117/255,112/255,179/255,0.5)
clr4c<-rgb(231/255,41/255,138/255,0.5)
clr5c<-rgb(102/255,166/255,30/255,0.5)
clr6c<-rgb(230/255,171/255,2/255,0.5)
clr7c<-rgb(166/255,118/255,29/255,0.5)

tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/Fig1.tex",width=6,height=3)

par(mfrow = c(1,3),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
#Hazard ratio

#Gamma#
OH_1f<-function(t,HR0,theta0){
    HR0*(1/theta0+t^3/60)/(1/theta0+t^3/(60/HR0))
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylab="Hazard ratio",xlab="Time",lty=2,col=clr1)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
abline(h=3,lwd=2,col=clr1b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylab="Hazard ratio",xlab="Time",col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylab="Hazard ratio",xlab="Time",lty=2,col=clr2)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
abline(h=1/3,lwd=2,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = TRUE)
box(which = "plot", bty = "l")

legend(6,2.75,legend=c(3,0.33),lty=c(1,1), col=c(clr1,clr2),title="CHR")


#InvGau#

OH_1f<-function(t,HR,theta0){
    HR*((1/theta0+2*t^3/60)/(1/theta0+2*t^3/(60/HR)))^(0.5)
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
abline(h=3,lwd=2,col=clr1b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
abline(h=1/3,lwd=2,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")

#Cpoi#


OH_1f<-function(t,HR,theta0){
    
    HR*((2*theta0*(t^3/60)+3)/(2*theta0*(t^3/(60/HR))+3))^(3/2)
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
abline(h=3,lwd=2,col=clr1b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
abline(h=1/3,lwd=2,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")



title(xlab = "t",
      ylab = "Marginal causal hazard ratio",
      outer = TRUE, line = 3)

legend(6,2.75,legend=c(0.5,1,2),lty=c(3,1,2),title="var(U0)")
dev.off()

##########################
## Effect heterogeneity ##
##########################

tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/Fig2.tex",width=8,height=3)

par(mfrow = c(1,4),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)

#Discrete

# derive suitable setting
# 
# HR<-3
# var<-0.5
# 
# setting<-NULL
# 
# for (p1 in seq(0,0.5,0.05)){
#     for (a in seq(0.05,0.95,0.05)){
#         p2<- (-1 + HR + p1 - a*p1)^2/(1+var-2*HR+HR^2-p1+2*a*p1-a^2*p1)
#         b<- (var-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
#         setting<-rbind(setting,c(p1,a,p2,b))
#     }
# }
# 
# settingH<-setting[which(0< (setting[,1]+setting[,3]) & (setting[,1]+setting[,3]) <1 & setting[,4]>0),]
# 
# HR<-1/3
# var<-1
# 
# setting<-NULL
# 
# for (p2 in seq(0,0.1,0.001)){
#     for (b in seq(1.1,10,0.1)){
#         p1<- (-1 + HR + p2 - b*p2)^2/(1+var-2*HR+HR^2-p2+2*b*p2-b^2*p2)
#         a<- (var-HR+HR^2+b*p2-b^2*p2)/(-1+HR+p2-b*p2)
#         setting<-rbind(setting,c(p1,a,p2,b))
#     }
# }
# 
# settingB1<-(setting[which((0< (setting[,1]+setting[,3]) & (setting[,1]+setting[,3]) <1& setting[,2]>0)),])
# View(settingB1)
# 
# 
# HR<-1/3
# var<-2
# 
# setting<-NULL
# 
# for (p1 in seq(0.5,1,0.05)){
#     for (a in seq(0.01,0.75,0.01)){
#         p2<- (-1 + HR + p1 - a*p1)^2/(1+var-2*HR+HR^2-p1+2*a*p1-a^2*p1)
#         b<- (var-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
#         setting<-rbind(setting,c(p1,a,p2,b))
#     }
# }
# 
# settingB2<-setting[which(0< (setting[,1]+setting[,3]) & (setting[,1]+setting[,3]) <1 & setting[,4]>0),]
# View(settingB2

LaplaceG<-function(s,p1,a,p2,b){p1*exp(-s*a)+(1-p1-p2)*exp(-s)+p2*exp(-s*b)}
dLaplaceG<-function(s,p1,a,p2,b){-a*p1*exp(-s*a)-(1-p1-p2)*exp(-s)-b*p2*exp(-s*b)}

OH_1h<-function(t,HR,var,p1=0,a=0,p2=0,b=0){
    #print(c(p1,a,p2,b))
    if(p2==0){
        p2<- (-1 + HR + p1 - a*p1)^2/(1+var-2*HR+HR^2-p1+2*a*p1-a^2*p1)
        b<- (var-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
        if(p1+p2>1|p2<0|b<0){ print('Unvalid input')}
    }
    else{
        p1<- (-1 + HR + p2 - b*p2)^2/(1+var-2*HR+HR^2-p2+2*b*p2-b^2*p2)
        a<- (var-HR+HR^2+b*p2-b^2*p2)/(-1+HR+p2-b*p2)
        if(p1+p2>1|p1<0|a<0){ print('Unvalid input')}
    }
    
    
    s<-t^3/(60)
    return(-dLaplaceG(s,p1,a,p2,b)/LaplaceG(s,p1,a,p2,b))}

# 
# ##new
# OH_1h<-function(t,HR,var,p1=0,p2=0){
#     #print(c(p1,a,p2,b))
#     a<- (p1^2 + p1*(-1 + HR + p2) - sqrt(
#         p1*p2*(-1 + p1 + var*p1 + p2 + var*p2 - 2*HR*(-1 + p1 + p2) + 
#                    HR^2*(-1 + p1 + p2))))/(p1*(p1 + p2))
#     
#     b<- ((-1 + HR + p1)*p2 + p2^2 + sqrt(
#         p1*p2*(-1 + p1 + var*p1 + p2 + var*p2 - 2*HR*(-1 + p1 + p2) + 
#                    HR^2*(-1 + p1 + p2))))/(p2*(p1 + p2))
#     
#     if(p1+p2>1|p2<0|p1<0|a<0|a>1|b<1){ print('Unvalid input')}
#     
#     
#     
#     
#     s<-t^3/(60)
#     return(-dLaplaceG(s,p1,a,p2,b)/LaplaceG(s,p1,a,p2,b))}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,HR=3,var=1,p1=0.05,a=0.5)}),type="l",ylim=c(0,max(1,3)),col=clr3,axes=F)
abline(h=3,lwd=2,col=clr3b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,HR=3,var=2,p1=0.05,a=0.5)}),type="l",lty=2, col=clr3)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,HR=3,var=0.5,p1=0.05,a=0.5)}),type="l",lty=3, col=clr3)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))

lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,HR=1/3,var=1,p1=0.90,a=0.1)}),type="l",lty=1, col=clr4)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,HR=1/3,var=2,p1=0.90,a=0.1)}),type="l",lty=2, col=clr4)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,HR=1/3,var=0.5,p1=0.90,a=0.1)}),type="l",lty=3, col=clr4)
abline(h=1/3,lwd=2,col=clr4b)
abline(h=0,lwd=2,col=rgb(220/255,220/255,220/255,0.2))

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = TRUE)
box(which = "plot", bty = "l")

legend(6,2.75,c(0.5,1,2),lty=c(3,1,2),title="var(U1)")




#Gamma
OH_1h<-function(t,mu1,theta1){
    a<-theta1/mu1
    b<-mu1^2/theta1
    s<-t^3/60
    return((a*b)/(a*s+1))
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr3,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr3)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr3)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr3)
abline(h=3,lwd=2,col=clr3b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr4)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr4)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr4)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr4)
abline(h=1/3,lwd=2,col=clr4b)
abline(h=0,lwd=2,col=rgb(220/255,220/255,220/255,0.2))

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")

#Inverse Gaussian
OH_1h<-function(t,mu1,theta1){
    a<-(mu1^3/theta1)
    b<-2*mu1^2
    s<-t^3/60
    return(b*sqrt(a/(a+b*s))/(2*mu1))
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr3,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr3)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr3)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr3)
abline(h=3,lwd=2,col=clr3b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr4)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr4)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr4)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr4)
abline(h=1/3,lwd=2,col=clr4b)
abline(h=0,lwd=2,col=rgb(220/255,220/255,220/255,0.2))

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")

#Compound Poisson
OH_1h<-function(t,mu1,theta1){
    a<-3*mu1^2/theta1
    b<-(mu1/theta1)*(3/2)
    s<-t^3/60
    #return(a*(s*sqrt(b+s)+b*(sqrt(b+s)+s))/(2*(b*(b+s))^(3/2)))
    return(0.5*a*sqrt(b)/(b+s)^(3/2))
    
    
    #-(3*mu^2/theta)*(1/60)*(t^3)/(2*sqrt(((mu/theta)*(3/2))/((mu/theta)*(3/2)+(1/60)*(t^3)))*((mu/theta)*(3/2)+(1/60)*(t^3))^2)
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr3,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr3)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr3)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr3)
abline(h=3,lwd=2,col=clr3b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr4)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr4)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr4)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1h(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr4)
abline(h=1/3,lwd=2,col=clr4b)
abline(h=0,lwd=2,col=rgb(220/255,220/255,220/255,0.2))

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")

title(xlab = "t",
      ylab = "Marginal causal hazard ratio",
      outer = TRUE, line = 3)

dev.off()
#####################
#### Combination ####
#####################

tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/Fig3.tex",width=6,height=3)

par(mfrow = c(1,3),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)

n<-1000000
HR<-3
var1<-1
var0<-0

U0<-rgamma(n,shape=1/var0,scale=var0)
U1<-rgamma(n,shape=HR^2/var1,scale=var1/HR)

NT<-runif(n,0,1)
T0<-(-(60/U0)*log(NT))^(1/3)
T1<-(-(60/(U1*U0))*log(NT))^(1/3)

pA<-0.5
A<-rbinom(n,1,pA)

T<-A*T1+(1-A)*T0

test<-function(t){
    mean((U1*U0)[which(T1>t)])/mean((U0)[which(T0>t)])    
}

test1<-function(t){
    mean((U1*U0)[which(T1>t)])    
}

test0<-function(t){
    mean((U0)[which(T0>t)])    
}


LaplaceG0<-function(s,var0){(1+var0*s)^-(1/var0)}
dLaplaceG0<-function(s,c,var0){c*-(var0*s+1)^-((var0+1)/var0)}

OH_1<-function(t,HR,var0,var1,p1=0,a=0,p2=0,b=0){
    if(p2==0){
        p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
        b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
        if(p1+p2>1|p2<0|b<0){ print('Unvalid input')}
    }
    else{
        p1<- (-1 + HR + p2 - b*p2)^2/(1+var1-2*HR+HR^2-p2+2*b*p2-b^2*p2)
        a<- (var1-HR+HR^2+b*p2-b^2*p2)/(-1+HR+p2-b*p2)
        if(p1+p2>1|p1<0|a<0){ print('Unvalid input')}
    }
    
    s<-t^3/60
    return(
        (-(p1*dLaplaceG0(a*s,a,var0)+(1-p1-p2)*dLaplaceG0(1*s,1,var0)+p2*dLaplaceG0(b*s,b,var0))/(p1*LaplaceG0(a*s,var0)+(1-p1-p2)*LaplaceG0(1*s,var0)+p2*LaplaceG0(b*s,var0)))
        /(-dLaplaceG0(s,1,var0)/LaplaceG0(s,var0)))
    #)
}

OH_1f<-function(t,HR0,theta0){
    HR0*(1/theta0+t^3/60)/(1/theta0+t^3/(60/HR0))
}

#var1=1
plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=1,var1=1,p1=0.05,a=0.5)}),type="l",lty=1,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time",axes=F)
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=2,var1=1,p1=0.05,a=0.5)}),type="l",lty=2,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=0.5,var1=1,p1=0.05,a=0.5)}),type="l",lty=3,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=3,lwd=2,col=clr5b)
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=1,var1=1,p1=0.9,a=0.1)}),type="l",lty=1,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=2,var1=1,p1=0.9,a=0.1)}),type="l",lty=2,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=0.5,var1=1,p1=0.9,a=0.1)}),type="l",lty=3,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=1/3,lwd=2,col=clr6b)
#abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))
abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))
abline(h=1/3,lwd=2,col=clr2b)
abline(h=3,lwd=2,col=clr1b)

lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1b)


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = TRUE)
box(which = "plot", bty = "l")

legend(6,2.5,c(0.5,1,2),lty=c(3,1,2),title="var(U0)",cex=0.9)


#################################
###Inverse Gaussian + Discrete ###
#################################

LaplaceG0<-function(s,var0){exp((1-sqrt(1+2*var0*s))/var0)}
dLaplaceG0<-function(s,c,var0){c*exp((1-sqrt(1+2*var0*s))/var0)/sqrt(2*var0*s+1)}

OH_1f<-function(t,HR,theta0){
    HR*((1/theta0+2*t^3/60)/(1/theta0+2*t^3/(60/HR)))^(0.5)
}


plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=1,var1=1,p1=0.05,a=0.5)}),type="l",lty=1,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time",axes=F)
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=2,var1=1,p1=0.05,a=0.5)}),type="l",lty=2,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=0.5,var1=1,p1=0.05,a=0.5)}),type="l",lty=3,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=3,lwd=2,col=clr5b)
abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=1,var1=1,p1=0.9,a=0.1)}),type="l",lty=1,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=2,var1=1,p1=0.9,a=0.1)}),type="l",lty=2,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=0.5,var1=1,p1=0.9,a=0.1)}),type="l",lty=3,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=1/3,lwd=2,col=clr6b)
abline(h=1/3,lwd=2,col=clr2b)
abline(h=3,lwd=2,col=clr1b)


abline(h=sqrt(0.5),lwd=2,col=clr5b)
abline(h=sqrt(0.1),lwd=2,col=clr6b)


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")



#################################
###Compound Poisson + Discrete ###
#################################

LaplaceG0<-function(s,var0){exp((-3/var0)*(1-(((3/2)/var0)/((3/2)/var0+s))^(0.5)))}
dLaplaceG0<-function(s,c,var0){c*(-3*sqrt(3)*(1/(2*var0*s+3))^(3/2))*LaplaceG0(s,var0)}

OH_1f<-function(t,HR,theta0){
    
    HR*((2*theta0*(t^3/60)+3)/(2*theta0*(t^3/(60/HR))+3))^(3/2)
}

plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=1,var1=1,p1=0.05,a=0.5)}),type="l",lty=1,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time",axes=F)
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=2,var1=1,p1=0.05,a=0.5)}),type="l",lty=2,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=0.5,var1=1,p1=0.05,a=0.5)}),type="l",lty=3,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=3,lwd=2,col=clr5b)
abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=1,var1=1,p1=0.9,a=0.1)}),type="l",lty=1,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=2,var1=1,p1=0.9,a=0.1)}),type="l",lty=2,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=0.5,var1=1,p1=0.9,a=0.1)}),type="l",lty=3,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=1/3,lwd=2,col=clr6b)
abline(h=1/3,lwd=2,col=clr2b)
abline(h=3,lwd=2,col=clr1b)
abline(h=2.929066,lwd=2,col=clr6b)
abline(h=0.640348,lwd=2,col=clr5b)

lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2b)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")

title(xlab = "t",
      ylab = "Marginal causal hazard ratio",
      outer = TRUE, line = 3)
dev.off()
#############
#### T>> ####
#############

tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/Fig4.tex",width=6,height=3)
par(mfrow = c(1,3),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
LaplaceG0<-function(s,var0){(1+var0*s)^-(1/var0)}
dLaplaceG0<-function(s,c,var0){c*-(var0*s+1)^-((var0+1)/var0)}

OH_1<-function(t,HR,var0,var1,p1=0,a=0,p2=0,b=0){
    if(p2==0){
        p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
        b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
        if(p1+p2>1|p2<0|b<0){ print('Unvalid input')}
    }
    else{
        p1<- (-1 + HR + p2 - b*p2)^2/(1+var1-2*HR+HR^2-p2+2*b*p2-b^2*p2)
        a<- (var1-HR+HR^2+b*p2-b^2*p2)/(-1+HR+p2-b*p2)
        if(p1+p2>1|p1<0|a<0){ print('Unvalid input')}
    }
    
    s<-t^3/60
    return(
        (-(p1*dLaplaceG0(a*s,a,var0)+(1-p1-p2)*dLaplaceG0(1*s,1,var0)+p2*dLaplaceG0(b*s,b,var0))/(p1*LaplaceG0(a*s,var0)+(1-p1-p2)*LaplaceG0(1*s,var0)+p2*LaplaceG0(b*s,var0)))
        /(-dLaplaceG0(s,1,var0)/LaplaceG0(s,var0)))
    #)
}

OH_1f<-function(t,HR0,theta0){
    HR0*(1/theta0+t^3/60)/(1/theta0+t^3/(60/HR0))
}

#var1=1
plot(seq(0.1,30,0.1),sapply(seq(0.1,30,0.1),function(x){OH_1(x,HR=3,var0=1,var1=1,p1=0.05,a=0.5)}),type="l",lty=1,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time",axes=F)
lines(seq(0.1,30,0.1),sapply(seq(0.1,30,0.1),function(x){OH_1(x,HR=3,var0=2,var1=1,p1=0.05,a=0.5)}),type="l",lty=2,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,30,0.1),sapply(seq(0.1,30,0.1),function(x){OH_1(x,HR=3,var0=0.5,var1=1,p1=0.05,a=0.5)}),type="l",lty=3,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=3,lwd=2,col=clr5b)
lines(seq(0.1,30,0.1),sapply(seq(0.1,30,0.1),function(x){OH_1(x,HR=1/3,var0=1,var1=1,p1=0.9,a=0.1)}),type="l",lty=1,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,30,0.1),sapply(seq(0.1,30,0.1),function(x){OH_1(x,HR=1/3,var0=2,var1=1,p1=0.9,a=0.1)}),type="l",lty=2,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,30,0.1),sapply(seq(0.1,30,0.1),function(x){OH_1(x,HR=1/3,var0=0.5,var1=1,p1=0.9,a=0.1)}),type="l",lty=3,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=1/3,lwd=2,col=clr6b)
#abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))
abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))
abline(h=1/3,lwd=2,col=clr2b)
abline(h=3,lwd=2,col=clr1b)

lines(seq(0,30,0.1),sapply(seq(0,30,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1b)
lines(seq(0,30,0.1),sapply(seq(0,30,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1b)
lines(seq(0,30,0.1),sapply(seq(0,30,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1b)


lines(seq(0,30,0.1),sapply(seq(0,30,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2b)
lines(seq(0,30,0.1),sapply(seq(0,30,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2b)
lines(seq(0,30,0.1),sapply(seq(0,30,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = TRUE)
box(which = "plot", bty = "l")

legend(6,2.5,c(0.5,1,2),lty=c(3,1,2),title="var(U0)",cex=0.9)


#################################
###Inverse Gaussian + Discrete ###
#################################

LaplaceG0<-function(s,var0){exp((1-sqrt(1+2*var0*s))/var0)}
dLaplaceG0<-function(s,c,var0){c*exp((1-sqrt(1+2*var0*s))/var0)/sqrt(2*var0*s+1)}

OH_1f<-function(t,HR,theta0){
    HR*((1/theta0+2*t^3/60)/(1/theta0+2*t^3/(60/HR)))^(0.5)
}


plot(seq(0.1,20,0.1),sapply(seq(0.1,20,0.1),function(x){OH_1(x,HR=3,var0=1,var1=1,p1=0.05,a=0.5)}),type="l",lty=1,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time",axes=F)
lines(seq(0.1,20,0.1),sapply(seq(0.1,20,0.1),function(x){OH_1(x,HR=3,var0=2,var1=1,p1=0.05,a=0.5)}),type="l",lty=2,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,20,0.1),sapply(seq(0.1,20,0.1),function(x){OH_1(x,HR=3,var0=0.5,var1=1,p1=0.05,a=0.5)}),type="l",lty=3,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=3,lwd=2,col=clr5b)
abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))
lines(seq(0.1,20,0.1),sapply(seq(0.1,20,0.1),function(x){OH_1(x,HR=1/3,var0=1,var1=1,p1=0.9,a=0.1)}),type="l",lty=1,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,20,0.1),sapply(seq(0.1,20,0.1),function(x){OH_1(x,HR=1/3,var0=2,var1=1,p1=0.9,a=0.1)}),type="l",lty=2,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,20,0.1),sapply(seq(0.1,20,0.1),function(x){OH_1(x,HR=1/3,var0=0.5,var1=1,p1=0.9,a=0.1)}),type="l",lty=3,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=1/3,lwd=2,col=clr6b)
abline(h=1/3,lwd=2,col=clr2b)
abline(h=3,lwd=2,col=clr1b)


abline(h=sqrt(0.5),lwd=2,col=clr5b)
abline(h=sqrt(0.1),lwd=2,col=clr6b)


lines(seq(0,20,0.1),sapply(seq(0,20,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1b)
lines(seq(0,20,0.1),sapply(seq(0,20,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1b)
lines(seq(0,20,0.1),sapply(seq(0,20,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1b)
lines(seq(0,20,0.1),sapply(seq(0,20,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2b)
lines(seq(0,20,0.1),sapply(seq(0,20,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2b)
lines(seq(0,20,0.1),sapply(seq(0,20,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")


#################################
###Compound Poisson + Discrete ###
#################################

LaplaceG0<-function(s,var0){exp((-3/var0)*(1-(((3/2)/var0)/((3/2)/var0+s))^(0.5)))}
dLaplaceG0<-function(s,c,var0){c*(-3*sqrt(3)*(1/(2*var0*s+3))^(3/2))*LaplaceG0(s,var0)}

OH_1f<-function(t,HR,theta0){
    
    HR*((2*theta0*(t^3/60)+3)/(2*theta0*(t^3/(60/HR))+3))^(3/2)
}

plot(seq(0.1,50,0.1),sapply(seq(0.1,50,0.1),function(x){OH_1(x,HR=3,var0=1,var1=1,p1=0.05,a=0.5)}),type="l",lty=1,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time",axes=F)
lines(seq(0.1,50,0.1),sapply(seq(0.1,50,0.1),function(x){OH_1(x,HR=3,var0=2,var1=1,p1=0.05,a=0.5)}),type="l",lty=2,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,50,0.1),sapply(seq(0.1,50,0.1),function(x){OH_1(x,HR=3,var0=0.5,var1=1,p1=0.05,a=0.5)}),type="l",lty=3,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=3,lwd=2,col=clr5b)
abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))
lines(seq(0.1,50,0.1),sapply(seq(0.1,50,0.1),function(x){OH_1(x,HR=1/3,var0=1,var1=1,p1=0.9,a=0.1)}),type="l",lty=1,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,50,0.1),sapply(seq(0.1,50,0.1),function(x){OH_1(x,HR=1/3,var0=2,var1=1,p1=0.9,a=0.1)}),type="l",lty=2,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
lines(seq(0.1,50,0.1),sapply(seq(0.1,50,0.1),function(x){OH_1(x,HR=1/3,var0=0.5,var1=1,p1=0.9,a=0.1)}),type="l",lty=3,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=1/3,lwd=2,col=clr6b)
abline(h=1/3,lwd=2,col=clr2b)
abline(h=3,lwd=2,col=clr1b)
abline(h=2.929066,lwd=2,col=clr6b)
abline(h=0.640348,lwd=2,col=clr5b)

lines(seq(0,50,0.1),sapply(seq(0,50,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1b)
lines(seq(0,50,0.1),sapply(seq(0,50,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1b)
lines(seq(0,50,0.1),sapply(seq(0,50,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1b)
lines(seq(0,50,0.1),sapply(seq(0,50,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2b)
lines(seq(0,50,0.1),sapply(seq(0,50,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2b)
lines(seq(0,50,0.1),sapply(seq(0,50,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")

title(xlab = "t",
      ylab = "Marginal causal hazard ratio",
      outer = TRUE, line = 3)
dev.off()


########################
#### U0U1 dependent ####
########################
library(copula)
library(gumbel)
library(GoFKernel)
library(Compounding)
library(stabledist)
library(statmod)

g <- function(a) {
    iu <- complex(real=0, imaginary=1)
    return(abs(1 - iu * tan(pi * a / 2)) ^ (-1 / a))
}

n<-10000000

tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/Fig5b.tex",width=6,height=3)

par(mfrow = c(1,3),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
var0<-1
var1<-1
cop<-"Gaussian"


####EU0U1####

simcop<-function(n, cop, tau, frail, var0=1, alpha=0.5, HR, var1=1, p1, a){
    
    if(cop=="Gaussian"){
        #####Gaussian########
        rho<- sin(pi*tau/2)
        nc<- normalCopula(rho)
        NU  <- rCopula(n, copula = nc)
    }
    
    
    #####Frank Copula####
    if(cop=="Frank"){
        theta <- iTau(copFrank, tau) # copula parameter
        fc <- frankCopula(theta, dim = 2) # define a Frank copula
        NU  <- rCopula(n, copula = fc)
    }
    
    
    #####Gumbel #####
    if(cop=="Gumbel"){
        #theta<-1/(1-tau)
        theta<-iTau(copGumbel, tau)
        
        if(theta>=1){
            #iTau(copGumbel, tau)
            #NU<-rgumbel(n,theta,dim=2)
            gc <- gumbelCopula(theta) # (note the default dim = 2)
            NU  <- rCopula(n, copula = gc)
        }
        
        if(theta<1){
            #theta<-1/(1+tau)
            theta<-iTau(copGumbel, -tau)
            #NU<-rgumbel(n,theta,dim=2)
            gc <- gumbelCopula(theta) # (note the default dim = 2)
            NU  <- rCopula(n, copula = gc)
            NU[,1]<-1-NU[,1]
        }
        
    }
    
    ####Clayton ######
    if(cop=="Clayton"){
        #theta<-2*tau/(1-tau)
        theta<-iTau(copClayton, tau)
        cc <- claytonCopula(theta, dim = 2) 
        
        NU  <- rCopula(n, copula = cc)
    }
    
    if(cop=="Independent"){
        NU  <- cbind(runif(n),runif(n))
    }
    
    if(cop=="Dependent"){
        NU  <- runif(n)
        if(tau==1){
            NU <-cbind(NU,NU)
        }
        if(tau== -1){
            NU <-cbind(NU,1-NU)
        }
    }
    
    ###Simulate U0, U1 #####
    if(frail=="gamma"){
        
        U0<-sapply(NU[,1],function(x){qgamma(x, shape=1/var0,scale=var0)})
        
    }
    
    if(frail=="cpois"){
    
        
        cpoisdata<-(sapply(rpois(n,3/var0),function(x){ifelse(x>0,sum(rgamma(x,shape=1/2,scale=(2/3)*var0)),0)}))
        U0<-quantile(cpoisdata,NU[,1])
        
        
    }
    
    if(frail=="pstable"){
       
        gamma<-g(alpha)
        U0<-sapply(NU[,1],function(x){tryCatch({qstable(x, alpha=alpha, beta=1, gamma=gamma, delta=0, pm=1)}, error=function(e){qstable(round(x,3), alpha=alpha, beta=1, gamma=gamma, delta=0, pm=1)})})
        
    }
    
    if(frail=="invgauss"){
      
           U0<-sapply(NU[,1],function(x){qinvgauss(x,1,1/var0)})    
    }
    
    p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
    b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
    
    U1<-sapply(NU[,2],function(x){if(x<p1){return(a)}else{
        if(x<(1-p2)){return(1)}
        else{
            return(b)}
    }
    })
    
    
    NT<-runif(n,0,1)
    T0<-(-(60/U0)*log(NT))^(1/3)
    T1<-(-(60/(U1*U0))*log(NT))^(1/3)
    
    
    pA<-0.5
    A<-rbinom(n,1,pA)
    
    T<-A*T1+(1-A)*T0
    
    return(data.frame(A=A, T=T, T0=T0, T1=T1, U0=U0, U1=U1))
    
}

#cop: "Gaussian", "Frank", "Gumbel", "Clayton"
#frail: "gamma", "cpois", "pstable", "invgauss"


for(frail in c("gamma", "invgauss", "cpois")){
    #for(var0 in c(0.5,1,2)){
    # for(var1 in c(0.5,1,2)){
    
    
    dataH0<-simcop(n, cop='Independent', tau= -1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataB0<-simcop(n, cop='Independent', tau= 1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    dataHneg<-simcop(n, cop='Dependent', tau= -1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataHpos<-simcop(n, cop='Dependent', tau= 1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataBneg<-simcop(n, cop='Dependent', tau= -1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    dataBpos<-simcop(n, cop='Dependent', tau= 1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    
    U1<-dataH0$U1
    U0<-dataH0$U0
    T1<-dataH0$T1
    T0<-dataH0$T0   
    
    #plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylim=c(0,3.5),ylab="Hazard ratio", xlab="t",col=clr5,type="l",lty=1,axes=F)
    plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])}),ylim=c(0,3.5),ylab="Hazard ratio", xlab="t",col=clr5,type="l",lty=1,axes=F)
    #,main=paste(cop,'+',frail,sep=" ")
    #lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U0)[which(T0>t)])}),ylim=c(0,3.5),ylab="Hazard ratio", xlab="t",col=clr5b,type="l",lty=1,axes=F)
    
    U1<-dataHneg$U1
    U0<-dataHneg$U0
    T1<-dataHneg$T1
    T0<-dataHneg$T0
    
    #lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylim=c(0,4),ylab="Hazard ratio", xlab="t",main=paste(cop,'+',frail,sep=" "),col=clr5,type="l",lty=5)
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])}),ylim=c(0,4),ylab="Hazard ratio", xlab="t",main=paste(cop,'+',frail,sep=" "),col=clr5,type="l",lty=5)
    
    type<-4
    for(tau in c(-0.5, 0.5)){
        data<-simcop(n, cop=cop, tau= tau, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
        
        U1<-data$U1
        U0<-data$U0
        T1<-data$T1
        T0<-data$T0
        #plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",type="l",col=clr5,ylim=c(0,4))
        lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr5,ylim=c(0,4),lty=type)
        type<-type-2
    }
    
    U1<-dataHpos$U1
    U0<-dataHpos$U0
    T1<-dataHpos$T1
    T0<-dataHpos$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])}),ylab="Hazard ratio",xlab="t",col=clr5,ylim=c(0,4),lty=3)
    
    U1<-dataB0$U1
    U0<-dataB0$U0
    T1<-dataB0$T1
    T0<-dataB0$T0 
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=1)
    
    U1<-dataBneg$U1
    U0<-dataBneg$U0
    T1<-dataBneg$T1
    T0<-dataBneg$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=5)
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U0)[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col="black",ylim=c(0,4),lty=1)
    
    type<-4
    for(tau in c(-0.5, 0.5)){
        data<-simcop(n, cop=cop, tau= tau, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
        
        U1<-data$U1
        U0<-data$U0
        T1<-data$T1
        T0<-data$T0
        
        #plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",type="l",col=clr5,ylim=c(0,4))
        lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=type)
        type<-type-2
        
    }
    
    U1<-dataBpos$U1
    U0<-dataBpos$U0
    T1<-dataBpos$T1
    T0<-dataBpos$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=3)
    
    if(frail=="gamma"){
        axis(side = 2,
             labels = TRUE)
        axis(side = 1,
             labels = TRUE)
    }
    else{
        axis(side = 2,
             labels = F)
        axis(side = 1,
             labels = TRUE)
    }
    
    
    box(which = "plot", bty = "l")
    
}
#}
#}

legend(6, 3.25, c(-1,0.5,0,0.5,1),lty=c(1,2,3,4,5),seg.len=3,col="black",title="tau")


title(xlab = "t",
      ylab = "Conditional expectation",
      outer = TRUE, line = 3)
dev.off()

####### MCHR ########
tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/Fig6b.tex",width=6,height=3)
par(mfrow = c(1,3),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
simcop<-function(n, cop, tau, frail, var0=1, alpha=0.5, HR, var1, p1, a){
    
    if(cop=="Gaussian"){
        #####Gaussian########
        rho<- sin(pi*tau/2)
        nc<- normalCopula(rho)
        NU  <- rCopula(n, copula = nc)
    }
    
    
    #####Frank Copula####
    if(cop=="Frank"){
        theta <- iTau(copFrank, tau) # copula parameter
        fc <- frankCopula(theta, dim = 2) # define a Frank copula
        NU  <- rCopula(n, copula = fc)
    }
    
    
    #####Gumbel #####
    if(cop=="Gumbel"){
        #theta<-1/(1-tau)
        theta<-iTau(copGumbel, tau)
        
        if(theta>=1){
            #iTau(copGumbel, tau)
            #NU<-rgumbel(n,theta,dim=2)
            gc <- gumbelCopula(theta) # (note the default dim = 2)
            NU  <- rCopula(n, copula = gc)
        }
        
        if(theta<1){
            #theta<-1/(1+tau)
            theta<-iTau(copGumbel, -tau)
            #NU<-rgumbel(n,theta,dim=2)
            gc <- gumbelCopula(theta) # (note the default dim = 2)
            NU  <- rCopula(n, copula = gc)
            NU[,1]<-1-NU[,1]
        }
        
    }
    
    
    ####Clayton ######
    if(cop=="Clayton"){
        #theta<-2*tau/(1-tau)
        theta<-iTau(copClayton, tau)
        cc <- claytonCopula(theta, dim = 2) 
        
        NU  <- rCopula(n, copula = cc)
    }
    
    if(cop=="Independent"){
        NU  <- cbind(runif(n),runif(n))
    }
    
    if(cop=="Dependent"){
        NU  <- runif(n)
        if(tau==1){
            NU <-cbind(NU,NU)
        }
        if(tau== -1){
            NU <-cbind(NU,1-NU)
        }
    }
    
    ###Simulate U0, U1 #####

    if(frail=="gamma"){
        # f <- function(x) pgamma(x, shape=1/var0,scale=var0)
        # f.inv <- inverse(f,lower=0,upper=qgamma(max(NU[,1]), shape=1/var0,scale=var0)+1)
        # U0<-sapply(NU[,1],function(x){f.inv(x)})
        
        U0<-sapply(NU[,1],function(x){qgamma(x, shape=1/var0,scale=var0)})
        
    }
    
    if(frail=="cpois"){
        
        cpoisdata<-(sapply(rpois(n,3/var0),function(x){ifelse(x>0,sum(rgamma(x,shape=1/2,scale=(2/3)*var0)),0)}))
        U0<-quantile(cpoisdata,NU[,1])
        
        
    }
    
    if(frail=="pstable"){
        gamma<-g(alpha)
        U0<-sapply(NU[,1],function(x){tryCatch({qstable(x, alpha=alpha, beta=1, gamma=gamma, delta=0, pm=1)}, error=function(e){qstable(round(x,3), alpha=alpha, beta=1, gamma=gamma, delta=0, pm=1)})})
        
    }
    
    if(frail=="invgauss"){
        
        U0<-sapply(NU[,1],function(x){qinvgauss(x,1,1/var0)})    
    }
    
    p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
    b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
    
    U1<-sapply(NU[,2],function(x){if(x<p1){return(a)}else{
        if(x<(1-p2)){return(1)}
        else{
            return(b)}
    }
    })
    
    
    NT<-runif(n,0,1)
    T0<-(-(60/U0)*log(NT))^(1/3)
    T1<-(-(60/(U1*U0))*log(NT))^(1/3)
    
    
    pA<-0.5
    A<-rbinom(n,1,pA)
    
    T<-A*T1+(1-A)*T0
    
    return(data.frame(A=A, T=T, T0=T0, T1=T1, U0=U0, U1=U1))
    
}


cop<-"Gaussian"
for(frail in c("gamma", "invgauss", "cpois")){
    #for(var0 in c(0.5,1,2)){
    # for(var1 in c(0.5,1,2)){
    
    
    dataH0<-simcop(n, cop='Independent', tau= -1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataB0<-simcop(n, cop='Independent', tau= 1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    dataHneg<-simcop(n, cop='Dependent', tau= -1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataHpos<-simcop(n, cop='Dependent', tau= 1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataBneg<-simcop(n, cop='Dependent', tau= -1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    dataBpos<-simcop(n, cop='Dependent', tau= 1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    
    U1<-dataH0$U1
    U0<-dataH0$U0
    T1<-dataH0$T1
    T0<-dataH0$T0   
    
    plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylim=c(0,3.5),ylab="Hazard ratio", xlab="t",col=clr5,type="l",lty=1,axes=F)
    #,main=paste(cop,'+',frail,sep=" ")
    
    U1<-dataHneg$U1
    U0<-dataHneg$U0
    T1<-dataHneg$T1
    T0<-dataHneg$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylim=c(0,4),ylab="Hazard ratio", xlab="t",main=paste(cop,'+',frail,sep=" "),col=clr5,type="l",lty=5)
    
    type<-4
    for(tau in c(-0.5, 0.5)){
        data<-simcop(n, cop=cop, tau= tau, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
        
        U1<-data$U1
        U0<-data$U0
        T1<-data$T1
        T0<-data$T0
        #plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",type="l",col=clr5,ylim=c(0,4))
        lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr5,ylim=c(0,4),lty=type)
        type<-type-2
    }
    
    U1<-dataHpos$U1
    U0<-dataHpos$U0
    T1<-dataHpos$T1
    T0<-dataHpos$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr5,ylim=c(0,4),lty=3)
    
    U1<-dataB0$U1
    U0<-dataB0$U0
    T1<-dataB0$T1
    T0<-dataB0$T0 
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=1)
    
    U1<-dataBneg$U1
    U0<-dataBneg$U0
    T1<-dataBneg$T1
    T0<-dataBneg$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=5)
    
    type<-4
    for(tau in c(-0.5, 0.5)){
        data<-simcop(n, cop=cop, tau= tau, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
        
        U1<-data$U1
        U0<-data$U0
        T1<-data$T1
        T0<-data$T0
        
        #plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",type="l",col=clr5,ylim=c(0,4))
        lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=type)
        type<-type-2
        
    }
    
    U1<-dataBpos$U1
    U0<-dataBpos$U0
    T1<-dataBpos$T1
    T0<-dataBpos$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=3)
    
    if(frail=="gamma"){
        axis(side = 2,
             labels = TRUE)
        axis(side = 1,
             labels = TRUE)
    }
    else{
        axis(side = 2,
             labels = F)
        axis(side = 1,
             labels = TRUE)
    }
    
    
    box(which = "plot", bty = "l")
    
}
#}
#}

legend(6, 3.25, c(-1,0.5,0,0.5,1),lty=c(1,2,3,4,5),seg.len=3,col="black",title="tau")


title(xlab = "t",
      ylab = "Marginal causal hazard ratio",
      outer = TRUE, line = 3)
dev.off()
##### Gamma modifier #####
tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/Fig7b.tex",width=7,height=3)
par(mfrow = c(1,3),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
simcop<-function(n, cop, tau, frail, var0=1, alpha=0.5, HR, var1, p1, a){
    
    if(cop=="Gaussian"){
        #####Gaussian########
        rho<- sin(pi*tau/2)
        nc<- normalCopula(rho)
        NU  <- rCopula(n, copula = nc)
    }
    
    
    #####Frank Copula####
    if(cop=="Frank"){
        theta <- iTau(copFrank, tau) # copula parameter
        fc <- frankCopula(theta, dim = 2) # define a Frank copula
        NU  <- rCopula(n, copula = fc)
    }
    
    
    #####Gumbel #####
    if(cop=="Gumbel"){
        #theta<-1/(1-tau)
        theta<-iTau(copGumbel, tau)
        
        if(theta>=1){
            #iTau(copGumbel, tau)
            #NU<-rgumbel(n,theta,dim=2)
            gc <- gumbelCopula(theta) # (note the default dim = 2)
            NU  <- rCopula(n, copula = gc)
        }
        
        if(theta<1){
            #theta<-1/(1+tau)
            theta<-iTau(copGumbel, -tau)
            #NU<-rgumbel(n,theta,dim=2)
            gc <- gumbelCopula(theta) # (note the default dim = 2)
            NU  <- rCopula(n, copula = gc)
            NU[,1]<-1-NU[,1]
        }
        
    }
    
    
    ####Clayton ######
    if(cop=="Clayton"){
        #theta<-2*tau/(1-tau)
        theta<-iTau(copClayton, tau)
        cc <- claytonCopula(theta, dim = 2) 
        
        NU  <- rCopula(n, copula = cc)
    }
    
    if(cop=="Independent"){
        NU  <- cbind(runif(n),runif(n))
    }
    
    if(cop=="Dependent"){
        NU  <- runif(n)
        if(tau==1){
            NU <-cbind(NU,NU)
        }
        if(tau== -1){
            NU <-cbind(NU,1-NU)
        }
    }
    
    ########################
    ###Simulate U0, U1 #####
    ########################
    
    if(frail=="gamma"){
        # f <- function(x) pgamma(x, shape=1/var0,scale=var0)
        # f.inv <- inverse(f,lower=0,upper=qgamma(max(NU[,1]), shape=1/var0,scale=var0)+1)
        # U0<-sapply(NU[,1],function(x){f.inv(x)})
        
        U0<-sapply(NU[,1],function(x){qgamma(x, shape=1/var0,scale=var0)})
        
    }
    
    if(frail=="cpois"){
         cpoisdata<-(sapply(rpois(n,3/var0),function(x){ifelse(x>0,sum(rgamma(x,shape=1/2,scale=(2/3)*var0)),0)}))
        #U0<-cpoisdata[order(NU[,1])]
        U0<-quantile(cpoisdata,NU[,1])
        
    }
    
    if(frail=="pstable"){
       
        gamma<-g(alpha)
        U0<-sapply(NU[,1],function(x){tryCatch({qstable(x, alpha=alpha, beta=1, gamma=gamma, delta=0, pm=1)}, error=function(e){qstable(round(x,3), alpha=alpha, beta=1, gamma=gamma, delta=0, pm=1)})})
        
    }
    
    if(frail=="invgauss"){
       
        
        U0<-sapply(NU[,1],function(x){qinvgauss(x,1,1/var0)})    
    }
    
    
    U1<-sapply(NU[,2],function(x){qgamma(x, shape=HR^2/var1,scale=var1/HR)})
    
    NT<-runif(n,0,1)
    T0<-(-(60/U0)*log(NT))^(1/3)
    T1<-(-(60/(U1*U0))*log(NT))^(1/3)
    
    
    pA<-0.5
    A<-rbinom(n,1,pA)
    
    T<-A*T1+(1-A)*T0
    
    return(data.frame(A=A, T=T, T0=T0, T1=T1, U0=U0, U1=U1))
    
}

for(frail in c("gamma", "invgauss", "cpois")){
    #for(var0 in c(0.5,1,2)){
    # for(var1 in c(0.5,1,2)){
    
    
    dataH0<-simcop(n, cop='Independent', tau= -1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataB0<-simcop(n, cop='Independent', tau= 1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    dataHneg<-simcop(n, cop='Dependent', tau= -1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataHpos<-simcop(n, cop='Dependent', tau= 1, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
    dataBneg<-simcop(n, cop='Dependent', tau= -1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    dataBpos<-simcop(n, cop='Dependent', tau= 1, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
    
    U1<-dataH0$U1
    U0<-dataH0$U0
    T1<-dataH0$T1
    T0<-dataH0$T0   
    
    plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylim=c(0,4),ylab="Hazard ratio", xlab="t",col=clr5,type="l",lty=1,axes=F)
    #,main=paste(cop,'+',frail,sep=" ")
    
    U1<-dataHneg$U1
    U0<-dataHneg$U0
    T1<-dataHneg$T1
    T0<-dataHneg$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylim=c(0,4),ylab="Hazard ratio", xlab="t",main=paste(cop,'+',frail,sep=" "),col=clr5,type="l",lty=5)
    
    type<-4
    for(tau in c(-0.5, 0.5)){
        data<-simcop(n, cop=cop, tau= tau, frail=frail, var0=var0, HR=3, var1=var1, p1=0.05, a=0.5)
        
        U1<-data$U1
        U0<-data$U0
        T1<-data$T1
        T0<-data$T0
        #plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",type="l",col=clr5,ylim=c(0,4))
        lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr5,ylim=c(0,4),lty=type)
        type<-type-2
    }
    
    U1<-dataHpos$U1
    U0<-dataHpos$U0
    T1<-dataHpos$T1
    T0<-dataHpos$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr5,ylim=c(0,4),lty=3)
    
    U1<-dataB0$U1
    U0<-dataB0$U0
    T1<-dataB0$T1
    T0<-dataB0$T0 
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=1)
    
    U1<-dataBneg$U1
    U0<-dataBneg$U0
    T1<-dataBneg$T1
    T0<-dataBneg$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=5)
    
    type<-4
    for(tau in c(-0.5, 0.5)){
        data<-simcop(n, cop=cop, tau= tau, frail=frail, var0=var0, HR=1/3, var1=var1, p1=0.9, a=0.1)
        
        U1<-data$U1
        U0<-data$U0
        T1<-data$T1
        T0<-data$T0
        
        #plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",type="l",col=clr5,ylim=c(0,4))
        lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=type)
        type<-type-2
        
    }
    
    U1<-dataBpos$U1
    U0<-dataBpos$U0
    T1<-dataBpos$T1
    T0<-dataBpos$T0
    
    lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(t){mean((U1*U0)[which(T1>t)])/mean(U0[which(T0>t)])}),ylab="Hazard ratio",xlab="t",col=clr6,ylim=c(0,4),lty=3)
    
    if(frail=="gamma"){
        axis(side = 2,
             labels = TRUE)
        axis(side = 1,
             labels = TRUE)
    }
    else{
        axis(side = 2,
             labels = F)
        axis(side = 1,
             labels = TRUE)
    }
    
    
    box(which = "plot", bty = "l")
    
}
#}
#}

title(xlab = "t",
      ylab = "Marginal causal hazard ratio",
      outer = TRUE, line = 3)

legend(6, 3.25, c(-1,0.5,0,0.5,1),lty=c(1,2,3,4,5),seg.len=3,col="black",title="tau")
dev.off()
##########
###COX####
##########

library(survival)
tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/Fig8b.tex",width=2,height=3)

par(mfrow = c(1,1),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)



###Discrete + Gamma ####
LaplaceG0<-function(s,var0){(1+var0*s)^-(1/var0)}
dLaplaceG0<-function(s,c,var0){c*-(var0*s+1)^-((var0+1)/var0)}

OH_1<-function(t,HR,var0,var1,p1=0,a=0,p2=0,b=0){
    if(p2==0){
        p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
        b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
        if(p1+p2>1|p2<0|b<0){ print('Unvalid input')}
    }
    else{
        p1<- (-1 + HR + p2 - b*p2)^2/(1+var1-2*HR+HR^2-p2+2*b*p2-b^2*p2)
        a<- (var1-HR+HR^2+b*p2-b^2*p2)/(-1+HR+p2-b*p2)
        if(p1+p2>1|p1<0|a<0){ print('Unvalid input')}
    }
    
    s<-t^3/60
    return(
        (-(p1*dLaplaceG0(a*s,a,var0)+(1-p1-p2)*dLaplaceG0(1*s,1,var0)+p2*dLaplaceG0(b*s,b,var0))/(p1*LaplaceG0(a*s,var0)+(1-p1-p2)*LaplaceG0(1*s,var0)+p2*LaplaceG0(b*s,var0)))
        /(-dLaplaceG0(s,1,var0)/LaplaceG0(s,var0)))
    #)
}

OH_1f<-function(t,HR0,theta0){
    HR0*(1/theta0+t^3/60)/(1/theta0+t^3/(60/HR0))
}

#var1=1
plot(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=1,var1=1,p1=0.05,a=0.5)}),type="l",lty=1,col=clr5c,ylim=c(1,3),ylab="Hazard ratio",xlab="t",axes=F)
#lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=2,var1=1,p1=0.05,a=0.5)}),type="l",lty=2,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
#lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=3,var0=0.5,var1=1,p1=0.05,a=0.5)}),type="l",lty=3,col=clr5,ylim=c(0,3),ylab="Hazard",xlab="Time")
abline(h=3,lwd=1,col=clr5b)
# lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=1,var1=1,p1=0.9,a=0.1)}),type="l",lty=1,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
# lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=2,var1=1,p1=0.9,a=0.1)}),type="l",lty=2,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
# lines(seq(0.1,10,0.1),sapply(seq(0.1,10,0.1),function(x){OH_1(x,HR=1/3,var0=0.5,var1=1,p1=0.9,a=0.1)}),type="l",lty=3,col=clr6,ylim=c(0,3),ylab="Hazard",xlab="Time")
#abline(h=1/3,lwd=2,col=clr6b)
abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))

count<-0
for(k in c(0,2,1,0.5)){
    count<-count+1
    HR<-3
    #HR<-1/3
    var1<-1
    p1<-0.05
    #p1<-0.9
    a<-0.5
    #a<-0.1
    p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
    b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)    
    
    U1<-sample(c(a,b,1),n,prob=c(p1,p2,1-p1-p2),replace=TRUE)
    var0<-1
    
    U0<-rgamma(n,shape=1/var0,scale=var0)
    
    NT<-runif(n,0,1)
    T0<-(-(60/U0)*log(NT))^(1/3)
    T1<-(-(60/(U0*U1))*log(NT))^(1/3)
    
    
    pA<-0.5
    A<-rbinom(n,1,pA)
    
    T<-A*T1+(1-A)*T0
    
    if(k==0){
        C<-rep(0,n)
    }
    else{
        TC<-rexp(n,1/(k*min(mean(T1),mean(T0))))
        
        C<-(TC<T)
    }
    
    COXn<-function(FT){
        a<-(mean(sapply(T[which(T<=FT & !C)],function(x){log(OH_1(x,HR=3,var0=1,var1=1,p1=0.05,a=0.5))})))
        #b<-as.numeric(exp(coef(coxph(Surv(T, E) ~ A, data = data.frame(T=T, E=as.numeric((T<=FT & !C)), A=A),init=a))))
        #return(c(exp(a),b))
        return(exp(a))
        
    }
    
    #test<-cbind(sapply(seq(2,10,0.5),function(x){COXn(x)}))
    #lines(seq(2,10,0.5),(test[2,]),type="l",col=clr3,lty=count)
    #lines(seq(2,10,0.5),(test[1,]),type="l",col=clr4,lty=count)
    
    lines(seq(1,10,0.5),sapply(seq(1,10,0.5),function(x){COXn(x)}),type="l",col=clr4,lty=count)
}

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = TRUE)
box(which = "plot", bty = "l")

title(xlab = "Time to follow-up",
      ylab = "Estimand Cox",
      outer = TRUE, line = 3)

legend(5.25,1.55,c("-", "0.5 E[T1]","E[T1]","2  E[T1]"),lty=c(1,2,3,4),seg.len=4,title="E[Tc]",col=clr4, cex=0.5)
dev.off()

####################################
#### Survival curves: Appendix #####
####################################

LaplaceG<-function(s,var0){(1+var0*s)^-(1/var0)}
LaplaceIG<-function(s,var0){exp((1-sqrt(1+2*var0*s))/var0)}
LaplaceCP<-function(s,var0){exp((-3/var0)*(1-(((3/2)/var0)/((3/2)/var0+s))^(0.5)))}


#    s<-t^3/60
LaplaceGD<-function(s,HR,var0,var1,p1=0,a=0,p2=0,b=0){
    if(p2==0){
        p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
        b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
        if(p1+p2>1|p2<0|b<0){ print('Unvalid input')}
    }
    else{
        p1<- (-1 + HR + p2 - b*p2)^2/(1+var1-2*HR+HR^2-p2+2*b*p2-b^2*p2)
        a<- (var1-HR+HR^2+b*p2-b^2*p2)/(-1+HR+p2-b*p2)
        if(p1+p2>1|p1<0|a<0){ print('Unvalid input')}
    }
    return(
        p1*dLaplaceG(a*s,var0)+(1-p1-p2)*dLaplaceG(1*s,var0)+p2*dLaplaceG(b*s,var0))
    #)
}

LaplaceIGD<-function(s,HR,var0,var1,p1=0,a=0,p2=0,b=0){
    if(p2==0){
        p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
        b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
        if(p1+p2>1|p2<0|b<0){ print('Unvalid input')}
    }
    else{
        p1<- (-1 + HR + p2 - b*p2)^2/(1+var1-2*HR+HR^2-p2+2*b*p2-b^2*p2)
        a<- (var1-HR+HR^2+b*p2-b^2*p2)/(-1+HR+p2-b*p2)
        if(p1+p2>1|p1<0|a<0){ print('Unvalid input')}
    }
    return(
        p1*dLaplaceIG(a*s,var0)+(1-p1-p2)*dLaplaceIG(1*s,var0)+p2*dLaplaceIG(b*s,var0))
    #)
}

LaplaceCPD<-function(s,HR,var0,var1,p1=0,a=0,p2=0,b=0){
    if(p2==0){
        p2<- (-1 + HR + p1 - a*p1)^2/(1+var1-2*HR+HR^2-p1+2*a*p1-a^2*p1)
        b<- (var1-HR+HR^2+a*p1-a^2*p1)/(-1+HR+p1-a*p1)
        if(p1+p2>1|p2<0|b<0){ print('Unvalid input')}
    }
    else{
        p1<- (-1 + HR + p2 - b*p2)^2/(1+var1-2*HR+HR^2-p2+2*b*p2-b^2*p2)
        a<- (var1-HR+HR^2+b*p2-b^2*p2)/(-1+HR+p2-b*p2)
        if(p1+p2>1|p1<0|a<0){ print('Unvalid input')}
    }
    return(
        p1*dLaplaceCP(a*s,var0)+(1-p1-p2)*dLaplaceCP(1*s,var0)+p2*dLaplaceCP(b*s,var0))
    #)
}

tikz("D:/Documents/PhD/Causal Hazard Ratio/Examples/TikzFigures/SurvivalCurves1.tex",width=6,height=3)

par(mfrow = c(1,3),
    oma = c(5,4,0,0) + 0.1,
    mar = c(0,0,1,1) + 0.1)
#Hazard ratio

#Gamma#
OH_1f<-function(t,HR0,theta0){
    HR0*(1/theta0+t^3/60)/(1/theta0+t^3/(60/HR0))
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylab="Hazard ratio",xlab="Time",lty=2,col=clr1)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
abline(h=3,lwd=2,col=clr1b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylab="Hazard ratio",xlab="Time",col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylab="Hazard ratio",xlab="Time",lty=2,col=clr2)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
abline(h=1/3,lwd=2,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = TRUE)
box(which = "plot", bty = "l")

legend(6,2.75,legend=c(3,0.33),lty=c(1,1), col=c(clr1,clr2),title="CHR")


#InvGau#

OH_1f<-function(t,HR,theta0){
    HR*((1/theta0+2*t^3/60)/(1/theta0+2*t^3/(60/HR)))^(0.5)
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
abline(h=3,lwd=2,col=clr1b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
abline(h=1/3,lwd=2,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")

#Cpoi#


OH_1f<-function(t,HR,theta0){
    
    HR*((2*theta0*(t^3/60)+3)/(2*theta0*(t^3/(60/HR))+3))^(3/2)
}

plot(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr1,axes=F)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr1)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr1)
abline(h=3,lwd=2,col=clr1b)

abline(h=1,lwd=2,col=rgb(220/255,220/255,220/255,0.2))


lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,1)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,2)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=2,col=clr2)
#lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
lines(seq(0,10,0.1),sapply(seq(0,10,0.1),function(x){OH_1f(x,1/3,0.5)}),type="l",ylim=c(0,3),ylab="Hazard ratio",xlab="Time",lty=3,col=clr2)
abline(h=1/3,lwd=2,col=clr2b)

axis(side = 1,
     labels = TRUE)

axis(side = 2,
     labels = FALSE)
box(which = "plot", bty = "l")



title(xlab = "t",
      ylab = "Marginal causal hazard ratio",
      outer = TRUE, line = 3)

legend(6,2.75,legend=c(0.5,1,2),lty=c(3,1,2),title="var(U0)")
dev.off()