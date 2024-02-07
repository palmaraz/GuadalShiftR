
# This is a modified function from the 'bayesdfa' package
plot_hmm_regimes = function(model, probs = c(0.05, 0.95), regime_prob_threshold = 0.9, years){

  gamma_tk <- rstan::extract(model$model, pars = "gamma_tk")[[1]]
  mu_k <- rstan::extract(model$model, pars = "mu_k")[[1]]
  l <- apply(gamma_tk, 2:3, quantile, probs = probs[[1]])
  u <- apply(gamma_tk, 2:3, quantile, probs = probs[[2]])
  med <- apply(gamma_tk, 2:3, quantile, probs = 0.5)
  range01 <- function(x) (x - min(x))/(max(x) - min(x))
  mu_k_low <- apply(mu_k, 2, quantile, probs = probs[[1]])
  mu_k_high <- apply(mu_k, 2, quantile, probs = probs[[2]])
  mu_k <- apply(mu_k, 2, median)
  confident_regimes <- apply(gamma_tk, 2:3, function(x) {
    mean(x > 0.5) > regime_prob_threshold
  })
  regime_indexes <- apply(confident_regimes, 1, function(x) {
    w <- which(x)
    if (length(w) == 0)
      NA
    else w
  })

  plot(x=years,y=as.numeric(HMM_model_DFA_2trends$y), col = "red", pch = 15,
       ylab = "Major DFA trend value", xlab = "Year")
  if (!all(is.na(regime_indexes))) {
    for (i in seq_along(regime_indexes)) {
      segments(x0 = years[i] - 0.5, x1 = years[i] + 0.5, y0 = mu_k[regime_indexes[i]],
               y1 = mu_k[regime_indexes[i]])
      polygon(c(years[i] - 0.5, years[i] - 0.5, years[i] + 0.5, years[i] + 0.5),
              c(mu_k_low[regime_indexes[i]], mu_k_high[regime_indexes[i]],
                mu_k_high[regime_indexes[i]], mu_k_low[regime_indexes[i]]),
              border = NA, col = "#00000050")
    }
  }

}

# Function for prior vs. posterior distribution

prior_vs_posterior <- function(data,
                               alpha=2, beta=8,
                               mean=0, sd=10,
                               bins = 50, xlim = c(-10, 10),
                               title = "Prior vs. posterior distribution",
                               dist = "normal",
                               pos.legend="center") {

  data = as.data.frame(data)

  hist_data <- hist(data[,1], breaks = bins, plot = FALSE)
  hist_height <- max(hist_data$density)

  # Build an histogram
  hist(data[,1], breaks = bins, main = title,
       xlab = "Probability of interaction", ylab = "Density",
       col = "#6666FF", border = "#330099",
       ylim = c(0, hist_height), xlim = xlim, prob = TRUE)

  if (dist == "normal") {
    x <- seq(xlim[1],xlim[2], length = 1000)
    y <- dnorm(x, mean = mean, sd = sd)
    y <- y * hist_height / max(y)
  }

  if (dist == "spike-slab") {
    x <- seq(xlim[1],xlim[2], length = 1000)
    y <- dnorm(x, mean = mean, sd = sd) # This is the slab, a normal distribution
    y <- y * hist_height / max(y)
  }

  if (dist == "beta") {
    x <- seq(xlim[1],xlim[2], length = 1000)
    y <- dbeta(x, shape1 = alpha, shape2 = beta)
    y <- y * hist_height / max(y)
  }

  # Continuous distribution
  lines(x, y, col = "red", lwd = 2, xlim = xlim)

  if (dist == "spike-slab") {

    abline(v = 0, col = "red", lwd = 2, lty = 2) # This is the spike, a Dirac's delta at 0

    legend(pos.legend, legend = c("Posterior", "Prior slab", "Prior spike"),
           col = c("#6666FF", "red", "red"), lwd = c(1, 2, 2), bty = "n",
           x.intersp = 0.5, lty = c(1,1,2))

  }

  # Legend
  else{
  legend(pos.legend, legend = c("Posterior", "Prior"),
         col = c("#6666FF", "red"), lwd = c(1, 2), bty = "n",
         x.intersp = 0.5, lty = 1)
  }


}
