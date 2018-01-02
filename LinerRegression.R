factor1 <- c(41.9, 43.4, 43.9, 44.5, 47.3, 47.5, 47.9, 50.2, 52.8, 53.2, 56.7, 57.0, 63.5, 65.3, 71.1, 77.0,77.8)
factor2 <- c(seq(29.1,29.9, by=0.2),seq(30.3,30.7,by=0.2), 30.8,30.9,31.5, 31.7, 31.9, 32.0, 32.1, 32.5, 32.9)

yield <- c(251.3, 251.3, 248.3, 267.5, 273, 276.5, 270.3, 274.9, 285, 290, 297, 302.5, 304.5, 309.3, 321.7, 330.7, 349)



mydata <- data.frame(factor1,factor2,yield)
# liner regression
lm.sol <- lm(yield ~ factor1 + factor2,data = mydata)
summary(lm.sol)

# test for factor1 = 45.3 factor2 = 268.4
test <- data.frame(factor1 = 45.3,factor2 = 268.4)
lm.pred <- predict(lm.sol,test ,type = "prediction")


# Compute F0
tmp <- rep(1,17)
cmb <- c(tmp, factor1,factor2)
X <- matrix(cmb, nc = 3, byrow = FALSE)
y <- matrix(yield)

X1 <- t(X)      #  transpose of the matrix x
y1 <- t(y)     #  transpose of the matrix y
H <- X %*% solve(X1 %*% X) %*% X1
J <- matrix(rep(1,289), nr = 17)
J1 <- 1/17 * J
h <- H - J1
I <- diag(17)

SSR <- y1 %*% h %*% y
MSR <- SSR / 2
SSE <- y1 %*% (I - H) %*% y
MSE <- SSE / (17-(2+1))

F0 <- MSR / MSE






