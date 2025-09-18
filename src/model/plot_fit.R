## plot_fit.R
# Plot the fit of the model

# Loading required libraries
library("tidyverse")

# Prices
list.inds <- c("g", "bts", "hts")
fit.price <- read.csv("./output/model/model_fit_price.csv")
fit.price <- pivot_longer(fit.price, cols = c(paste0("p.", list.inds), "w"))
fit.price <- pivot_wider(fit.price, names_from = "type", values_from = "value")

fit.price$ind.alp <- ifelse(fit.price$name == "p.g", "(i) Goods",
  ifelse(fit.price$name == "p.bts", "(iii) BTS",
    ifelse(fit.price$name == "p.hts", "(ii) HTS",
      ifelse(fit.price$name == "w", "(iv) Value Added", "")
    )
  )
)

fit.price$ind.alp <- factor(fit.price$ind.alp, levels = c("(i) Goods", "(ii) HTS", "(iii) BTS", "(iv) Value Added"))

fit.price$ind <- ifelse(fit.price$name == "p.g", "Goods",
  ifelse(fit.price$name == "p.bts", "Barely tradable services",
    ifelse(fit.price$name == "p.hts", "Highly tradable services",
      ifelse(fit.price$name == "w", "Value added", "")
    )
  )
)

fit.price$ind <- factor(fit.price$ind, levels = c("Goods", "Highly tradable services", "Barely tradable services", "Value added"))

plot.fit <- function(dat, x.lab, y.lab) {
  ggplot(data = dat, aes(x = log(data), y = log(baseline))) +
    geom_point(alpha = 0.05, size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    xlab(x.lab) +
    ylab(y.lab) +
    facet_wrap(~ind.alp, scale = "free", ncol = 2) +
    theme_bw() +
    theme(text = element_text(size = 40), axis.text = element_text(size = 30), strip.text = element_text(size = 40, margin = margin(0, 0, 10, 0)), strip.background = element_blank())
}

png(file = "./doc/figures/model_fit_price.png", width = 1200, height = 1200)
plot.fit(fit.price, "Log price (data)", "Log price (baseline)")
dev.off()

# Import shares
fit.sh <- read.csv("./output/model/model_fit_share.csv")
list.country <- unique(fit.sh$country)
fit.sh <- pivot_longer(fit.sh, cols = all_of(list.country))
fit.sh <- pivot_wider(fit.sh, names_from = "type", values_from = "value")
fit.sh$ind.alp <- ifelse(fit.sh$ind == "g", "(i) Goods",
  ifelse(fit.sh$ind == "bts", "(iii) BTS",
    ifelse(fit.sh$ind == "hts", "(ii) HTS", "")
  )
)
fit.sh$ind.alp <- factor(fit.sh$ind.alp, levels = c("(i) Goods", "(ii) HTS", "(iii) BTS"))

fit.sh$ind <- ifelse(fit.sh$ind == "g", "Goods",
  ifelse(fit.sh$ind == "bts", "Barely tradable services",
    ifelse(fit.sh$ind == "hts", "Highly tradable services", "")
  )
)
fit.sh$ind <- factor(fit.sh$ind, levels = c("Goods", "Highly tradable services", "Barely tradable services"))

fit.sh.2 <- subset(fit.sh, data > 0) # log(0) is -Inf

png(file = "./doc/figures/model_fit_share.png", width = 1200, height = 1200)
plot.fit(
  fit.sh.2,
  "Log import share (data)",
  "Log import share (baseline)"
)
dev.off()
