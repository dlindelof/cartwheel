  #' The positive part function
  pos <- function(x) pmax(x, 0)

#' The daily energy model, without the noise component
#' 
#' @param dailyMeans A vector of mean daily temperatures
#' @param U The building's total U-value
#' @param baseTemp The building's base temperature
#' @param DHW The building's base load
dailyEnergyModel <- function(dailyMeans, U = 20, baseTemp = 12, DHW = 100)
  U * pos(baseTemp - dailyMeans) + DHW

#' The energy model for a reporting period
#' 
#' Parameters are as for dailyEnergyModel()
periodEnergyModel <- function(...)
  sum(dailyEnergyModel(...))

#' Chi-square for a single datum
#' 
#' @param response The value predicted by the model given the data and the parameters
#' @param model The model that predicts the response given data and parameters
#' @param sigma The standard deviation that should be used for computing the Chi-square
#' @param ... Extra arguments passed to the model
chisquare1 <- function(response, model, sigma, ...) {
  ((model(...) - response) / sigma) ^ 2
}

#' Chi-square for several reporting periods
#' 
#' @param periods A data frame with two elements: `Energy`, a vector of the energy
#' recorded during each period, and `DailyMeans`, a list of mean daily temperature
#' vectors for each reporting period.
#' @param U, baseTemp, DHW, sigma As above
chisquare <- function(periods, Kpre, baseTempPre, DHW, sigmaPre, Kpost, baseTempPost, sigmaPost) {
  stopifnot("Energy" %in% names(periods))
  stopifnot("DailyMeans" %in% names(periods))
  if (! "When" %in% names(periods)) periods$When <- "PRE"
  periods <- split(periods, periods$When)
  chisquaresPre <- if (is.null(periods$PRE))
                     0 else apply(periods$PRE, MARGIN = 1,
                              function(period) chisquare1(period$Energy,
                                                          periodEnergyModel,
                                                          sqrt(length(period$DailyMeans)) * sigmaPre,
                                                          period$DailyMeans, Kpre, baseTempPre, DHW))
  chisquaresPost <- if (is.null(periods$POST))
                    0 else apply(periods$POST, MARGIN = 1,
                               function(period) chisquare1(period$Energy,
                                                           periodEnergyModel,
                                                           sqrt(length(period$DailyMeans)) * sigmaPost,
                                                           period$DailyMeans, Kpost, baseTempPost, DHW))
  sum(chisquaresPre) + sum(chisquaresPost)
}

#' The log-likelihood of the data given the parameters
#' 
#' @param periods, U, baseTemp, DHW, sigma As above
loglikelihood <- function(periods, Kpre, baseTempPre, DHW, sigmaPre, Kpost, baseTempPost, sigmaPost) {
  if (sigmaPre <= 0 || sigmaPost <= 0) {
    return(-Inf)
  }
  if (DHW < 0) {
    message("Inf returned")
  }
  periodLengths <- sapply(periods$DailyMeans, length)
  -0.5 * sum(log(periodLengths)) - (1 + nrow(subset(periods, When == "PRE"))) * log(sigmaPre) - (1 + nrow(subset(periods, When == "POST"))) * log(sigmaPost) - chisquare(periods, Kpre, baseTempPre, DHW, sigmaPre, Kpost, baseTempPost, sigmaPost) / 2
}

#' Estimate building parameters and their uncertainties from the data
#' 
#' @param periods The energy and mean outdoor temperature, as above
#' @return A list of two elements: `params`, a list of estimated parameters, 
#' and `covariance`, the covariance matrix
estimateParameters <- function(periods, par = c(10, 20, 100, 10, 20, 100), ...) {
  # Find parameters that maximise log-likelihood
  objective <- function(params, data) {
    curried <- function(...) - loglikelihood(data, ...)
    params <- c(params[1:2], 0, params[3:6])
    do.call(curried, as.list(params))
  }
  optimal <- optim(par = par, # initial guess
                   fn = objective,
                   gr = NULL,
                   periods,
                   ...)
  optimal <- optim(par = optimal$par, # restart from previous maximum
                   fn = objective,
                   gr = NULL,
                   periods,
                   ...)
  optimal <- optim(par = optimal$par, # restart from previous maximum
                   fn = objective,
                   gr = NULL,
                   periods,
                   ..., 
                   hessian = TRUE) # for covariance matrix
  params = list(Kpre = 0, baseTempPre = 0, sigmaPre = 0, Kpost = 0, baseTempPost = 0, sigmaPost = 0)
  params[] <- optimal$par
  params$DHW <- 0
  list(params = params,
       covariance = solve(optimal$hessian))
}