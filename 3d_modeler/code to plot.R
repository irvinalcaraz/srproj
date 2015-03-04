# create sample dataset - you have this already,,,
set.seed(1)   # for reproducible example
df <- data.frame(x=sample(1:50,50)/50,y=sample(1:50,50)/50)
df$z <- with(df,exp(4*x + 2*y - 6*x*y + 6)+rnorm(50,sd=500))
fit <- lm(log(z) ~ x*y,df)   # bad, bad, bad - don't do this!!!

# you start here...
df$pred <- predict(fit)
# set up matrix of z-values
x <- seq(min(df$x),max(df$x),len=100)
y <- seq(min(df$y),max(df$y),len=100)
plot.df <- expand.grid(x=x,y=y)
plot.df$z <- predict(fit,newdata=plot.df)
library(reshape2)
z <- dcast(plot.df,x~y,value.var="z")[-1]

# plot the points, the fitted surface, and droplines
library(rgl)
colors <- 2.5+0.5*sign(residuals(fit))
open3d(scale=c(1,1,0.2))
points3d(df$x,df$y,log(df$z),col=colors)
surface3d(x,y,as.matrix(z),col="blue",alpha=.2)
apply(df,1,function(row)lines3d(rep(row[1],2),rep(row[2],2),c(log(row[3]),row[4]),col=colors))
axes3d()
title3d(xlab="X",ylab="Y",zlab="log(Z)")
