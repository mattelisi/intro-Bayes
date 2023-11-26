savage.dickey.bf <- function (x, x_0 = 0, prior.mean = 0, prior.sd = 1, plot = F, breaks=30) {
  require(polspline)
  fit.posterior <- logspline(x)
  posterior_w <- dlogspline(x_0, fit.posterior)
  if (plot) {
    R <- (fit.posterior$range[2] - fit.posterior$range[1])/3
    hist(x, xlab = "parameter value", ylab = "density", breaks=breaks,
         col="grey", border="white", freq=FALSE,
         xlim = c(fit.posterior$range[1] - R, fit.posterior$range[2] + 
                    R), main="")
    plot(fit.posterior, xlab = "parameter value", ylab = "density", 
         lwd = 2, xlim = c(fit.posterior$range[1] - R, fit.posterior$range[2] + 
                             R), add=T)
    
    x <- seq(fit.posterior$range[1] - R, fit.posterior$range[2] + 
               R, length.out = 500)
    lines(x, dnorm(x, mean = prior.mean, sd = prior.sd), 
          col = "red", lwd = 2)
    abline(v = x_0, lty = 2)
    points(x_0, posterior_w, pch = 19, col = "black")
    points(x_0, dnorm(x_0, prior.mean, prior.sd), pch = 19, 
           col = "red")
    legend("topright", c("posterior", "prior"), lwd = 2, 
           col = c("black", "red"), pch = 19, bty = "n", inset = 0.02)
  }
  cat(paste0("Approximate BF (Savage-Dickey) in favor of null x=",
            x_0, " : ", round(posterior_w/dnorm(x_0, prior.mean,
                                                prior.sd), digits = 2), "\n"))
  invisible(posterior_w/dnorm(x_0, prior.mean, prior.sd))
}