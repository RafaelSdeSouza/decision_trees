# Logit vs Random forest

require("plot3D")

set.seed(1056)                                        # set seed to replicate example
nobs= 1000                                            # number of obs in model 
x1 <- runif(nobs,0,7)                                 # random uniform variable
x2 <- runif(nobs,0,7) 
eta <- 0.2 - 2.5*x1 + 2.5*x2-0.25*x1^2 - 0.05*x1^3 + 0.15*x2^3 # linear predictor, xb
p <- 1/(1+exp(-eta))                                  # inverse-logit link
y <- rbinom(nobs,size=1, prob = p)                    # create y as adjusted random variate


fit  <- gam(y ~ s(x1,bs="cr",k=12) + s(x2,bs="cr",k=12),family= binomial(link="logit"),method="REML")

#fit <- glm(y~x1+x2,family = "binomial")               # Compute the linear regression 
summary(fit)                                          # check numerical results

# predict values on regular xy grid
grid.lines = 30
x1.pred <- seq(1.01*min(x1), 0.99*max(x1), length.out = grid.lines)
x2.pred <- seq(1.01*min(x2), 0.99*max(x2), length.out = grid.lines)
x1x2 <- expand.grid( x1 = x1.pred, x2 = x2.pred)

y.pred <- matrix(predict(fit, newdata = x1x2,type="response"), 
                 nrow = grid.lines, ncol = grid.lines)


# fitted points for droplines to surface
fitpoints <- predict(fit,type="response")

scatter3D(x1, x2, y,   cex = 0.5, cex.lab=1.5,type="p",pch = 19,alpha=0.5,
          theta = 100, phi = 35, ticktype = "detailed",col="orange",bty = "b2",
          xlab="x1",
          ylab="x2",
          zlab="y", 
          surf = list(col="purple",x = x1.pred, y = x2.pred, z = y.pred, facets=F,
                      alpha=0.7, 
                      lwd=1.25,lty=2),colkey = FALSE)


tree_model <- rpart(y~x1+x2)
yt.pred <- matrix(predict(tree_model, newdata = x1x2), 
                 nrow = grid.lines, ncol = grid.lines)

scatter3D(x1, x2, y,  cex = 0.5, cex.lab=1.5,type="p",pch = 19,alpha=0.5,
          theta = 45, phi = 35, ticktype = "detailed",col="orange",bty = "b2",
          xlab="x1",
          ylab="x2",
          zlab="y", 
          surf = list(col="purple",x = x1.pred, y = x2.pred, z = yt.pred,facets=F,
                      lwd=0.5,lty=1),colkey = FALSE)