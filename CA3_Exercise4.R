df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

nll_lm <- function(par, data){
  beta <- matrix(c(par[1], par[2], par[3], par[4]), ncol = 1)
  s2 <- par[5]
  X <- as.matrix(cbind(rep(1, nrow(data)), data[,-c(1)]))
  
  llik <- dnorm(data$y, mean = X %*% beta, sd = sqrt(s2), log = TRUE)
  
  return(-sum(llik))
}


init <- c(mean(df$y), 2, 3, 4, 0.5)
fit <- optim(par = init, fn = nll_lm, data = df, method="L-BFGS-B", lower=rep(-4, 5), upper=c(Inf, Inf, Inf, Inf, Inf))

fit$par[1:4]
sqrt(fit$par[5])

X <- as.matrix(cbind(rep(1, nrow(df)), df[,-c(1)]))
colnames(X)[1] <- "Intercept"
Y <- df$y
beta_hat <- solve(crossprod(X)) %*% t(X) %*% Y
beta_hat


e <- df$y - X %*% beta_hat
sigma_hat <- sqrt(crossprod(e)/(nrow(df)- 4))
sigma_hat


fit2 <- optim(par = init, fn = nll_lm, data = df, method="L-BFGS-B", 
              lower=rep(-5, 5), upper=c(Inf, Inf, Inf, Inf, Inf), 
              hessian = TRUE)
sd <- sqrt(diag(solve(fit2$hessian)))
sd[1:4]


#Ex4
fit_ex4 <- lm(y ~ x1 + x2 + x3, data = df)
summary(fit_ex4)
fit_ex4$coefficients
