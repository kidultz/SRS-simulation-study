x <- seq(-3,10,0.005)



############ 1    Scenario 1
set.seed(1)
dat1 <- c(rnorm(40,5,1),rnorm(10,7.5,0.5))
y1 <- 0.8*dnorm(x,5,1)+0.2*dnorm(x,7.5,0.5)
kde1 <- density(dat1,kernel = "gaussian")
matr1 <- as.matrix(dat1)
KNN1 <- knnDE(matr1, x, k=7)
hist(dat1,breaks = 0:25*kde1$bw,freq = F,ylim = c(0,0.75),
     col = "grey", border = "grey",
     main = "Density estimation for Scenario 1")
lines(x,y1,lwd=2)
lines(kde1,lwd=1.5,col="red")
lines(x,KNN1,lwd=1,col="blue1")
legend(6.25, 0.775, 
       legend=c("True Density", "Kernel Estimation","Knn Estimation"),
       col=c("black", "red","blue1"), 
       lty=c(1,1,1),lwd=c(2,1.5,1.5),cex=0.75)

############ 2    Scenario 2
set.seed(1)
dat2 <- c(rnorm(40,5,1),rt(10,10))
y2 <- 0.8*dnorm(x,5,1)+0.2*dt(x,10)
kde2 <- density(dat2,kernel = "gaussian")
matr2 <- as.matrix(dat2)
KNN2 <- knnDE(matr2, x, k=7)
hist(dat2,breaks = -10:20*kde2$bw,freq = F,ylim = c(0,0.6),
     col="grey", border = "grey",main = "Density estimation for Scenario 2")
lines(x,y2,lwd=2)
lines(kde2,lwd=1.5,col="red")
lines(x,KNN2,lwd=1.5,col="blue1")
legend(-6,0.625,
       legend=c("True Density", "Kernel Estimation","Knn Estimation"),
       col=c("black", "red","blue1"), 
       lty=c(1,1,1),lwd=c(2,1.5,1.5),cex=0.75)


############ 3    Scenario 3
set.seed(1)
dat3 <- c(rt(40,10),rlogis(10,2,0.1))
y3 <- 0.8*dt(x,10)+0.2*dlogis(x,2,0.1)
kde3 <- density(dat3,kernel = "gaussian")
matr3 <- as.matrix(dat3)
KNN3 <- knnDE(matr3, x, k=7)
hist(dat3,breaks = -5:10*kde3$bw, ylim = c(0,1),xlim = c(-2,4),
     freq = F,col="grey",border = "grey",main = "Density estimation for Scenario 3")
lines(x,y3,lwd=2)
lines(kde3,lwd=1.5,col="red")
lines(x,KNN3,lwd=1.5,col="blue1")
legend(1.15, 1.025, 
       legend=c("True Density", "Kernel Estimation","Knn Estimation"),
       col=c("black", "red","blue1"), 
       lty=c(1,1,1),lwd=c(2,1.5,1.5),cex=0.75)


#########################   1 simulation1 sceniario 1 ###################

R <- 1000
n <- 250
ISE11 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rnorm(0.2*n,7.5,0.5))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dnorm(x,5,1)+0.2*dnorm(x,7.5,0.5)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE11[r] <- integrate(splxy, lower = min(dat), upper = max(dat))
}
plot(1:1000,ISE11,type = "l",xlab = "Simulations",ylab="ISE - Scenario 1",main = "ISE for n=250")

R <- 1000
n <- 500
ISE12 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rnorm(0.2*n,7.5,0.5))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dnorm(x,5,1)+0.2*dnorm(x,7.5,0.5)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE12[r] <- integrate(splxy, lower = min(dat), upper = max(dat))
}
plot(1:1000,ISE12,type = "l",xlab = "Simulations",ylab=NA,main = "ISE for n=500")



R <- 1000
n <- 1000
ISE13 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rnorm(0.2*n,7.5,0.5))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dnorm(x,5,1)+0.2*dnorm(x,7.5,0.5)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE13[r] <- integrate(splxy, lower = min(dat), upper = max(dat),)
}
plot(1:1000,ISE13,type = "l",xlab = "Simulations",ylab=NA,main = "ISE for n=1000")



#########################   2 simulation 2 scenario 2 ###################
par(mfrow=c(1,3))
R <- 1000
n <- 250
ISE21 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rt(0.2*n,10))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dnorm(x,5,1)+0.2*dt(x,10)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE21[r] <- integrate(splxy, lower = min(dat), upper = max(dat))
}
plot(1:1000,ISE21,type = "l",xlab = "Simulations",ylab="ISE - Scenario 2",main = "ISE for n=250")

R <- 1000
n <- 500
ISE22 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rt(0.2*n,10))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dnorm(x,5,1)+0.2*dt(x,10)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE22[r] <- integrate(splxy, lower = min(dat), upper = max(dat))
}
plot(1:1000,ISE22,type = "l",xlab = "Simulations",ylab=NA,main = "ISE for n=500")



R <- 1000
n <- 1000
ISE23 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rt(0.2*n,10))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dnorm(x,5,1)+0.2*dt(x,10)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE23[r] <- integrate(splxy, lower = min(dat), upper = max(dat))
}
plot(1:1000,ISE23,type = "l",xlab = "Simulations",ylab=NA,main = "ISE for n=1000")






#########################   3 simulation 3 scenario 3 ###################
par(mfrow=c(1,3))
R <- 1000
n <- 250
ISE31 <- numeric(R)
for (r in 1:R) {
  dat <- c(rt(0.8*n,10),rlogis(0.2*n,2,0.1))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dt(x,10)+0.2*dlogis(x,2,0.1)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE31[r] <- integrate(splxy, lower = min(dat), upper = max(dat))
}
plot(1:1000,ISE31,type = "l",xlab = "Simulations",ylab="ISE - Scenario 3",main = "ISE for n=250")

R <- 1000
n <- 500
ISE32 <- numeric(R)
for (r in 1:R) {
  dat <- c(rt(0.8*n,10),rlogis(0.2*n,2,0.1))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dt(x,10)+0.2*dlogis(x,2,0.1)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE32[r] <- integrate(splxy, lower = min(dat), upper = max(dat))
}
plot(1:1000,ISE32,type = "l",xlab = "Simulations",ylab=NA,main = "ISE for n=500")



R <- 1000
n <- 1000
ISE33 <- numeric(R)
for (r in 1:R) {
  dat <- c(rt(0.8*n,10),rlogis(0.2*n,2,0.1))
  kde <- density(dat,kernel = "gaussian")
  x <- kde$x
  ybar <- kde$y
  y <- 0.8*dt(x,10)+0.2*dlogis(x,2,0.1)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE33[r] <- integrate(splxy, lower = min(dat), upper = max(dat))
}
plot(1:1000,ISE33,type = "l",xlab = "Simulations",ylab=NA,main = "ISE for n=1000")






