
x <- ( rnorm(1000,mean=80,sd=30) )^2

y <- -1*ifelse(x<10000,x,10000)/1000 + rnorm(1000,mean=130,sd=5)

plot(x,y,xlab="Number of Steps per day",ylab="systolic blood pressure")

abline(lm(y~x),cex=2,col="blue")

ll <- vector()


tau = seq(0,30000,100)

for (i in 1:length(tau)) {
	tempx <- ifelse(x>tau[i],0,x-tau[i])
	ll[i] <- logLik( lm(y~tempx) )	
}


LR <- 2*(ll - logLik( lm(y~x) ) ) 
tau[which.max(ll)]

plot(tau,LR,type="l",xlab="threshold",ylab="G")

abline(h=max3.84)


max(ll)
tau_est = tau[which.max(ll)]


abline(v=tau_est)


plot(x,y,xlab="Number of Steps per day",ylab="systolic blood pressure")

pred <- predict(lm(y~ifelse(x>tau_est,0,x-tau_est)))
lines(sort(x),pred[order(x)],cex=2,col="blue")



write.csv(data.frame(x=x,y=y),"/Users/rjt1/Documents/AdvBiostatsCourse/steps.csv",row.names=F)

