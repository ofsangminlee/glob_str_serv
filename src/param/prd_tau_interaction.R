## prd_tau_interaction.R
# The case where there are interactions between productivity and trade costs.
# Note: although I estimate both (a) trade costs -> productivity and (b) productivity -> trade costs. For the counterfactual exercise I only use (a).

## Load the libraries and functions
library("plm")
library("tidyverse")

## Load trade volume and trade costs and calculate the average trade costs for the country
load("./output/cleaned_data/trade_matrix.RData", verbose = TRUE)
load("./output/params/tau.RData", verbose = TRUE)

list.country <- read.csv("./output/cleaned_data/list_country.csv")
list.country <- list.country$code
list.country <- list.country[order(list.country)]
list.reclass <- c("g", "bts", "hts")

## Function to calculate weighted average trade costs for a country.
calc.tau.cty <- function(dat.trade, dat.tau, list.reclass) {
  tau.country.weighted <- data.frame()
  for (i.ind in list.reclass) {
    for (i.year in 1995:2018) {
      ## Weights = yearly trade data
      temp.trade <- dat.trade[[i.ind]][[i.year - 1994]]
      temp.tau <- dat.tau[[i.ind]][[i.year - 1994]]
      for (i.country in list.country) {
        temp.num <- sum(temp.tau[temp.tau$country == i.country, setdiff(list.country, i.country)] * temp.trade[temp.trade$country == i.country, setdiff(list.country, i.country)]) + sum(temp.tau[temp.tau$country != i.country, i.country] * temp.trade[temp.trade$country != i.country, i.country])
        temp.denom <- sum(temp.trade[temp.trade$country == i.country, setdiff(list.country, i.country)]) + sum(temp.trade[temp.trade$country != i.country, i.country])
        temp <- data.frame(i.country, i.year, i.ind, temp.num / temp.denom)
        colnames(temp) <- c("country", "year", "ind", "tau.avg")
        tau.country.weighted <- rbind(tau.country.weighted, temp)
      }
    }
  }
  return(tau.country.weighted)
}

dat.tau <- calc.tau.cty(icio.trade.ind, tau, list.reclass)
dat.tau$tau <- dat.tau$tau.avg
dat.tau$tau.avg <- NULL

check <- dat.tau[dat.tau$year %in% c(1995, 2018) & dat.tau$ind == "g", c("country", "year", "tau")]
check <- pivot_wider(check, names_from = year, values_from = tau)
sum(check[, "2018", drop = TRUE] < check[, "1995", drop = TRUE]) # Trade costs have decreased for all countries except one from 1995 to 2018.
check[check[, "2018", drop = TRUE] > check[, "1995", drop = TRUE], ]


## Load the productivity data and merge the data for estimation.
dat.prd <- read.csv("./output/params/prd.csv")
dat.prd$year <- as.integer(substr(dat.prd$year, 2, 5))

dat <- merge(dat.tau, dat.prd, by = c("country", "year", "ind"))
stopifnot(nrow(dat) == nrow(dat.tau))
stopifnot(nrow(dat) == nrow(dat.prd))

dat$ltau <- log(dat$tau)
dat$lprd <- log(dat$prd)


## Blundell-Bond (1998) estimator (aka system GMM) for dynamic panel data models.
## Why not Arellano-Bond? Productivities and trade costs are likely be highly persistent.
## Input: industry code (g, hts, bts)
## Output: list of models (1. tau->prd & 2. prd->tau) and data for the counterfactual where tau's are fixed at 1995 levels. For the generation of data, only Model 1 is used.

bb.ind <- function(i.ind) {
  dat.ind <- subset(dat, ind == i.ind)
  dat.ind$ind <- NULL
  pdat.ind <- pdata.frame(dat.ind, index = c("country", "year"))

  ## Tau -> prd
  model.1 <- pgmm(
    formula = lprd ~ lag(lprd, 1) + ltau |
      lag(lprd, 2) + lag(ltau, 2),
    data = pdat.ind,
    effect = "twoways", # Individual + time fixed effects
    transformation = "ld", # d = Arellano-Bond, ld = Blundell-Bond
    model = "twosteps", # Two‐step GMM estimator
    collapse = TRUE, # Collapse instrument matrix for efficiency
    robust = TRUE # Use robust standard errors
  )

  ## Prd->tau
  model.2 <- pgmm(
    formula = ltau ~ lag(ltau, 1) + lprd |
      lag(ltau, 2) + lag(lprd, 2),
    data = pdat.ind,
    effect = "twoways",
    transformation = "ld",
    model = "twosteps",
    collapse = TRUE,
    robust = TRUE #
  )

  ## Coefficients
  b1 <- coef(model.1)[1]
  b2 <- coef(model.1)[2]
  fe.time <- data.frame(year = 1996:2018, fe.time = c(coef(model.1)["(Intercept)"], (coef(model.1)[as.character(1997:2018)] + coef(model.1)["(Intercept)"])))
  rownames(fe.time) <- NULL

  ## Sanity check + backing out individual fixed effects
  # For Blundell-Bond (system GMM), fitted() returns a matrix with fitted values
  fit.all <- fitted(model.1)
  fit.diff <- fit.all[1:22, ] # 1997 to 2018 for differenced equation
  fit.level <- fit.all[23:45, ] # 1996 to 2018 for levels equation

  # Convert fit.level matrix to data frame with year, country, value columns
  fit.level <- data.frame(
    year = rep(as.numeric(rownames(fit.level)), ncol(fit.level)),
    country = rep(colnames(fit.level), each = nrow(fit.level)),
    value = as.vector(fit.level)
  )

  # Difference and level predictions from data
  temp <- dat.ind[, c("country", "year", "lprd", "ltau")]
  temp <- temp %>%
    arrange(country, year) %>% # make sure data is ordered properly
    group_by(country) %>%
    mutate(lprd.lg1 = lag(lprd, 1), lprd.lg2 = lag(lprd, 2), ltau.lg1 = lag(ltau, 1))

  temp <- merge(temp, fe.time, by = "year", all.x = TRUE)
  temp <- temp %>% arrange(country, year)

  temp <- temp %>% mutate(level.pred = b1 * lprd.lg1 + b2 * ltau + fe.time) # Level prediction without individual fixed effects but with time fixed effects.

  fit.level <- merge(fit.level, temp[, c("country", "year", "lprd", "level.pred")], by = c("country", "year"), all.x = TRUE)

  stopifnot(all.equal(fit.level$value, fit.level$level.pred)) # Sanity check

  # Although the individual fixed effects are differenced out (for the A-B moment conditions) and not included for moment conditions for (B-B moment conditions), we are backing them out as averages.
  fe.country <- fit.level %>%
    group_by(country) %>%
    summarise(fe.country = mean(lprd - level.pred))

  dat.ind <- merge(dat.ind, fe.country, by = "country", all.x = TRUE)
  dat.ind <- merge(dat.ind, fe.time, by = "year", all.x = TRUE)

  ## Prediction. Baseline vs counterfactual
  # What if \tau remains in the initial year?
  dat.ind$lprd.pred <- NA
  dat.ind$lprd.resid <- NA
  dat.ind$lprd.count <- NA

  for (i.cty in list.country) {
    # Initial year
    dat.ind[dat.ind$country == i.cty & dat.ind$year == 1995, "lprd.pred"] <- dat.ind[dat.ind$country == i.cty & dat.ind$year == 1995, "lprd"]
    dat.ind[dat.ind$country == i.cty & dat.ind$year == 1995, "lprd.count"] <- dat.ind[dat.ind$country == i.cty & dat.ind$year == 1995, "lprd"]
    for (i.yr in 1996:2018) {
      # Baseline
      value <- dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "fe.country"] + dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "fe.time"] + (b1 * dat.ind[dat.ind$country == i.cty & dat.ind$year == (i.yr - 1), "lprd"] + b2 * dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "ltau"])
      dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "lprd.pred"] <- value
      dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "lprd.resid"] <-
        dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "lprd"] -
        dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "lprd.pred"]
      # Counterfactual: Keep tau fixed at 1995 levels
      value2 <- dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "fe.country"] + dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "fe.time"] + (b1 * dat.ind[dat.ind$country == i.cty & dat.ind$year == (i.yr - 1), "lprd"] + b2 * dat.ind[dat.ind$country == i.cty & dat.ind$year == 1995, "ltau"]) + dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "lprd.resid"]
      dat.ind[dat.ind$country == i.cty & dat.ind$year == i.yr, "lprd.count"] <- value2
    }
  }

  dat.ind$ind <- i.ind

  return(list(model.1, model.2, dat.ind))
}

res.g <- bb.ind("g")
res.hts <- bb.ind("hts")
res.bts <- bb.ind("bts")

all_summaries <- capture.output({
  print("Results for G, tau -> prd")
  print(summary(res.g[[1]])) ## G tau->prd ## Arellano–Bond test for autocorrelation in the first‐differenced residuals, AR(1) ideally significant & AR(2) ideally not significant. Hansen-Sargan test is insignificant, indicating that the instruments are valid.
  print("Results for G, prd -> tau")
  print(summary(res.g[[2]])) ## G prd->tau
  print("Results for HTS, tau -> prd")
  print(summary(res.hts[[1]])) ## HTS tau->prd
  print("Results for HTS, prd -> tau")
  print(summary(res.hts[[2]])) ## HTS prd->tau
  print("Results for BTS, tau -> prd")
  print(summary(res.bts[[1]])) ## BTS tau->prd
  print("Results for BTS, prd -> tau")
  print(summary(res.bts[[2]])) ## BTS prd->tau
})
cat(all_summaries, sep = "\n")

writeLines(all_summaries, "./doc/numbers/ab_model_summaries.txt")

res.prd <- rbind(res.g[[3]], res.hts[[3]], res.bts[[3]])
stopifnot(all(!is.na(res.prd$lprd.count))) # No NA values
res.prd$prd.count <- exp(res.prd$lprd.count)

write.csv(res.prd, "./output/params/prd_tau_interaction.csv", row.names = FALSE)
