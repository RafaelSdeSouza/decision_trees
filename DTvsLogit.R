# Logit vs Random forest

require("plot3D")
require(viridis)
require(msm)
require(mgcv)
require(rpart)
require(plotmo)
set.seed(1056)                                        # set seed to replicate example
nobs= 1000                                            # number of obs in model 
x1 <- runif(nobs,0,7)                                 # random uniform variable
x2 <- runif(nobs,0,7) 
eta <-  sin(2*x1) - cos(x2-2) - 0.5*log(x1^2*x2^2) 
#eta <- 0.2 - 2.5*x1 + 2.5*x2-0.25*x1^2 - 0.05*x1^3 + 0.15*x2^3 # linear predictor, xb
p <- 1/(1+exp(-eta))                                  # inverse-logit link
y <- rbinom(nobs,size=1, prob = p)                    # create y as adjusted random variate


fit  <- gam(y ~ s(x1,bs="cr",k=12) + s(x2,bs="cr",k=12),family= binomial(link="logit"),method="REML")

#fit <- glm(y~x1+x2,family = "binomial")               # Compute the linear regression 
summary(fit)                                          # check numerical results

# predict values on regular xy grid
grid.lines = 100
x1.pred <- seq(1.01*min(x1), 0.99*max(x1), length.out = grid.lines)
x2.pred <- seq(1.01*min(x2), 0.99*max(x2), length.out = grid.lines)
x1x2 <- expand.grid( x1 = x1.pred, x2 = x2.pred)

y.pred <- matrix(predict(fit, newdata = x1x2,type="response"), 
                 nrow = grid.lines, ncol = grid.lines)


# fitted points for droplines to surface
fitpoints <- predict(fit,type="response")

scatter3D(x1, x2, y,   cex = 0.5, cex.lab=1.5,type="p",pch = 19,alpha=0.5,
          theta = 80, phi = 30, ticktype = "detailed",col="red",bty = "b2",
          xlab="x1",
          ylab="x2",
          zlab="y", 
          expand =0.6, 
          surf = list(x = x1.pred, y = x2.pred, z = y.pred, col = rev(viridis(50)), shade = 0.3,
                      lwd=1.25,lty=2),colkey = FALSE)


tree_model <- rpart(y~x1+x2)
yt.pred <- matrix(predict(tree_model, newdata = x1x2), 
                 nrow = grid.lines, ncol = grid.lines)

scatter3D(x1, x2, p,  cex = 1, cex.lab=1.5,type="p",pch = 19,alpha=0.5,
          theta = 80, phi = 30, ticktype = "detailed",col="red",bty = "u",
          xlab="x1",
          ylab="x2",
          zlab="y", 
          expand =0.6, 
          colkey = FALSE,
            surf = list(x = x1.pred, y = x2.pred, z = yt.pred,col = viridis(200), shade = 0.35,
                      lwd = 0.5,lty = 1))


plotmo(tree_model, type2="image")

plotmo(tree_model,type = "prob", nresponse = "y", # right graph
       type2 = "image", ngrid2 = 200,              # type2 = "image" for an image plot
       pt.col = ifelse("y" == 1, "red", "lightblue"))
