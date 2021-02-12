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
  ISE11[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE11)



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
  ISE12[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE12)


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
  ISE13[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE13)


#########################   2 simulation 2 scenario 2 ###################
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
  ISE21[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE21)


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
  ISE22[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE22)


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
  ISE23[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE23)





#########################   3 simulation 3 scenario 3 ###################
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
  ISE31[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE31)



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
  ISE32[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE32)




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
  ISE33[r] <- integrate(splxy, lower = min(dat), upper = max(dat))$value
}
mean(ISE33)



























#############################       knn        #######################


#########################   1 simulation1 sceniario 1 ###################
x <- seq(0,10,0.005)
R <- 1000
n <- 250
ISE11 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rnorm(0.2*n,7.5,0.5))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dnorm(x,5,1)+0.2*dnorm(x,7.5,0.5)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE11[r] <- integrate(splxy, lower = 0, upper = 10, subdivisions=2000)$value
}
mean(ISE11)



R <- 1000
n <- 500
ISE12 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rnorm(0.2*n,7.5,0.5))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dnorm(x,5,1)+0.2*dnorm(x,7.5,0.5)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE12[r] <- integrate(splxy, lower = 0,upper = 10, subdivisions=2000)$value
}
mean(ISE12)




R <- 1000
n <- 1000
ISE13 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rnorm(0.2*n,7.5,0.5))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dnorm(x,5,1)+0.2*dnorm(x,7.5,0.5)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE13[r] <- integrate(splxy, lower = 0, upper = 10,subdivisions = 2000)$value
}
mean(ISE13)




#########################   2 simulation 2 scenario 2 ###################
x <- seq(-3,8,0.005)
R <- 1000
n <- 250
ISE21 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rt(0.2*n,10))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dnorm(x,5,1)+0.2*dt(x,10)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE21[r] <- integrate(splxy, lower = -3, upper = 8, subdivisions = 3000)$value
}
mean(ISE21)


R <- 1000
n <- 500
ISE22 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rt(0.2*n,10))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dnorm(x,5,1)+0.2*dt(x,10)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE22[r] <- integrate(splxy, lower = -3, upper = 8, subdivisions = 3000)$value
}
mean(ISE22)


R <- 1000
n <- 1000
ISE23 <- numeric(R)
for (r in 1:R) {
  dat <- c(rnorm(0.8*n,5,1),rt(0.2*n,10))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dnorm(x,5,1)+0.2*dt(x,10)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE23[r] <- integrate(splxy, lower = -3, upper = 8, subdivisions = 3000)$value
}
mean(ISE23)





#########################   3 simulation 3 scenario 3 ###################
x <- seq(-5,5,0.005)
R <- 1000
n <- 250
ISE31 <- numeric(R)
for (r in 1:R) {
  dat <- c(rt(0.8*n,10),rlogis(0.2*n,2,0.1))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dt(x,10)+0.2*dlogis(x,2,0.1)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE31[r] <- integrate(splxy, lower = -5, upper = 5, subdivisions = 2000)$value
}
mean(ISE31)



R <- 1000
n <- 500
ISE32 <- numeric(R)
for (r in 1:R) {
  dat <- c(rt(0.8*n,10),rlogis(0.2*n,2,0.1))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dt(x,10)+0.2*dlogis(x,2,0.1)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE32[r] <- integrate(splxy, lower = -5, upper = 5, subdivisions = 2000)$value
}
mean(ISE32)




R <- 1000
n <- 1000
ISE33 <- numeric(R)
for (r in 1:R) {
  dat <- c(rt(0.8*n,10),rlogis(0.2*n,2,0.1))
  matr <- as.matrix(dat)
  ybar <- knnDE(matr, x, sqrt(n))
  y <- 0.8*dt(x,10)+0.2*dlogis(x,2,0.1)
  # Create approx func obj and integrate()
  splxy = splinefun(x, (y - ybar)^2)
  ISE33[r] <- integrate(splxy, lower = -5, upper = 5, subdivisions = 2000)$value
}
mean(ISE33)




