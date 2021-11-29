getPATEEstimate.bart.ppd = function(samples.cate, weights, sigma, samples.obs, samples.cf, n.obs){
  est <- mean(samples.cate)

  sd <- sqrt(var(samples.cate) +
               if (!is.null(sigma))
                 2 * mean(sigma^2) / n.obs
             else {
               samples.var <- samples.obs * (1 - samples.obs) + samples.cf * (1 - samples.cf)
               if (!is.null(weights))
                 mean(apply(weights * samples.var, 2L, sum) / n.obs)
               else
                 mean(apply(samples.var, 2L, mean) / n.obs)
             })
  estimate <- c(estimate = est, sd = sd)
  lci <- estimate[["estimate"]] - estimate[["sd"]] * qnorm(.975)
  names(lci) <- 'ci.lower'
  uci <- estimate[["estimate"]] + estimate[["sd"]] * qnorm(.975)
  names(uci) <- 'ci.upper'
  n <- n.obs
  names(n) <- 'n'
  out <- c(estimate, lci, uci, n)
  return(out)
}


fit_blocked_pate <- function(.model, block){
  block <- list(block)
  block <- unlist(lapply(block, interaction))
  pates <- list()
  for (i in 1:length(unique(block))) {
    index <- unique(block)[i]
    sigma <- .model$fit.rsp$sigma
    weights <- .model$data.rsp@weights
    samples.obs <- bartCause::extract(.model, 'mu.obs', 'all')
    samples.cf <-  bartCause::extract(.model, 'mu.cf', 'all')
    samples.obs <- samples.obs[, block == index]
    samples.cf <- samples.cf[, block == index]
    icate <- bartCause::extract(.model, 'icate')
    icate <- icate[, block == index]
    samples.cate<- apply(icate, 1, mean)

    pates[[i]] <- getPATEEstimate.bart.ppd(samples.cate = samples.cate,
                                           weights = weights,
                                           sigma = sigma,
                                           samples.obs = samples.obs,
                                           samples.cf = samples.cf,
                                           n.obs = length(block[block == index]))

  }
  pates <- bind_rows(pates)

  return(pates)

}
#
block <- dat[, c('u74', 'u75')]
fit_blocked_pate(.model, block = block)
#
# fit_blocked_pate <- function(.model, block){
#   getPATEEstimate.bart.ppd = function(samples.cate, weights, sigma, samples.obs, samples.cf, n.obs){
#     est <- mean(samples.cate)
#
#     sd <- sqrt(var(samples.cate) +
#                  if (!is.null(sigma))
#                    2 * mean(sigma^2) / n.obs
#                else {
#                  samples.var <- samples.obs * (1 - samples.obs) + samples.cf * (1 - samples.cf)
#                  if (!is.null(weights))
#                    mean(apply(weights * samples.var, 2L, sum) / n.obs)
#                  else
#                    mean(apply(samples.var, 2L, mean) / n.obs)
#                })
#     estimate <- c(estimate = est, sd = sd)
#     lci <- estimate[["estimate"]] - estimate[["sd"]] * qnorm(.975)
#     names(lci) <- 'ci.lower'
#     uci <- estimate[["estimate"]] + estimate[["sd"]] * qnorm(.975)
#     names(uci) <- 'ci.upper'
#     n <- n.obs
#     names(n) <- 'n'
#     out <- c(estimate, lci, uci, n)
#     return(out)
#   }
#
#
#   block <- unlist(lapply(block, interaction))
#     pates <- list()
#     for (i in 1:length(unique(block))) {
#       index <- unique(block)[i]
#       sigma <- .model$fit.rsp$sigma
#       weights <- .model$data.rsp@weights
#       samples.obs <- bartCause::extract(.model, 'mu.obs', 'all')
#       samples.cf <-  bartCause::extract(.model, 'mu.cf', 'all')
#       samples.obs <- samples.obs[, block == index]
#       samples.cf <- samples.cf[, block == index]
#       icate <- bartCause::extract(.model, 'icate')
#       icate <- icate[, block == index]
#       samples.cate<- apply(icate, 1, mean)
#
#       pates[[i]] <- getPATEEstimate.bart.ppd(samples.cate = samples.cate,
#                                weights = weights,
#                                samples.obs = samples.obs,
#                                samples.cf = samples.cf,
#                                sigma = sigma,
#                                n.obs = length(block[block == index))
#     }
#     pates <- bind_rows(pates)
#
#     return(pates)
# }
#
#   block <- list(dat$u74, dat$u75)
#
# estimate <- getPATEEstimate.bart.ppd(samples.cate = samples.cate, weights = weights, sigma = sigma, samples.obs = samples.obs, samples.cf = samples.cf, n.obs =ncol(samples.obs))
# lci <- estimate[["estimate"]] - estimate[["sd"]] * qnorm(.975)
# names(lci) <- 'ci.lower'
# uci <- estimate[["estimate"]] + estimate[["sd"]] * qnorm(.975)
# names(uci) <- 'ci.upper'
#
# out <- c(estimate, lci, uci)
# return(out)
# }
#
# block <- list(dat[, c('u74', 'u75')])
# fit_blocked_pate(.model, block)
# split()
# block <- dat$u74
#
