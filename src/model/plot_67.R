## plot_67.R
# Plot the results for 66 countries + ROW
# Generate tables for results as well.

# Loading required libraries and functions
library("tidyverse")
library("latex2exp")
library("grid")
library("gridExtra")
library("cowplot")
library("xtable")
source("./src/common_functions.R")

# Misc.
list.inds <- c("g", "bts", "hts")
list.levels <- c("BTS", "HTS", "G") # for plotting, this is the order that I want.

list.inds.alt <- c("g", "cs", "ps")
list.levels.alt <- c("CS", "PS", "G")

list.inds.gs <- c("g", "s")
list.levels.gs <- c("S", "G")

# Load the results
clean.res <- function(loc.file, list.inds, list.levels) {
  res <- read.csv(file = loc.file)
  res <- res[, c("year", "country", "type", paste0("va.", list.inds))]
  res <- pivot_longer(res, cols = paste0("va.", list.inds), names_to = "ind", values_to = "share")
  res$ind <- toupper(substr(res$ind, 4, nchar(res$ind)))
  res$ind <- factor(res$ind, levels = list.levels)
  return(res)
}

# Baseline
res <- clean.res("./output/model/str_chg.csv", list.inds, list.levels)

# Head and Ries trade costs
res.hs <- clean.res("./output/model/str_chg_hs.csv", list.inds, list.levels)

# Model without services trade
res.no.s.trade <- clean.res("./output/model/str_chg_no_s_trade.csv", list.inds, list.levels)

# Model with A tau interaction
res.interaction <- clean.res("./output/model/str_chg_interaction.csv", list.inds, list.levels)

# Robustness cases
res.alt <- clean.res("./output/model/str_chg_alt.csv", list.inds.alt, list.levels.alt)
res.gs <- clean.res("./output/model/str_chg_gs.csv", list.inds.gs, list.levels.gs)
res.bts <- clean.res("./output/model/str_chg_bts.csv", list.inds, list.levels)
res.high <- clean.res("./output/model/str_chg_high.csv", list.inds, list.levels)
res.low <- clean.res("./output/model/str_chg_low.csv", list.inds, list.levels)
res.nest <- clean.res("./output/model/str_chg_nest.csv", list.inds, list.levels)
res.nx <- clean.res("./output/model/str_chg_nx.csv", list.inds, list.levels)
res.wedge <- clean.res("./output/model/str_chg_wedge.csv", list.inds, list.levels)
res.orig <- clean.res("./output/model/str_chg_orig.csv", list.inds, list.levels)





## Results for 67 countries
# Load proportionality index
index <- read.csv(file = "./output/params/index.csv")

index.hs <- read.csv(file = "./output/params/index_hs.csv")

index.interaction <- index # Interaction case: it's the same as the baseline. It's not robust to define proportionality index for the interaction case, but this is the best I can do.

index.alt <- read.csv(file = "./output/params/index_alt.csv")
index.gs <- read.csv(file = "./output/params/index_gs.csv")
index.high <- read.csv(file = "./output/params/index_high.csv")
index.low <- read.csv(file = "./output/params/index_low.csv")
index.orig <- read.csv(file = "./output/params/index_orig.csv")

# Full result in a table
res.full <- index

# Developing or advnaced countries?
list.hl <- read.csv("./data/others/low_high_income.csv", skip = 3)
list.hl <- list.hl[, c("Country", "IMF")]
list.hl$Dev <- ifelse(list.hl$IMF == 1, "N", "Y")
list.hl$IMF <- NULL
colnames(list.hl) <- tolower(colnames(list.hl))

add.dev <- function(res.full) merge(res.full, list.hl, by = "country", all.x = TRUE)

res.full <- add.dev(res.full)

res.full <- res.full[, c("country", "dev", "glre.g", "glre.hts", "index")]

# Results for data, baseline, and three counterfactuals
res.2 <- res
res.2$ind <- factor(res.2$ind, levels = c("G", "HTS", "BTS"))
res.2 <- res.2[, c("year", "country", "type", "ind", "share")]
res.2 <- subset(res.2, year %in% c(1995, 2018))
res.2$type <- factor(res.2$type, levels = c("data", "baseline", "count_only_s", "count_only_g", "count_no"))

res.2 <- pivot_wider(res.2, values_from = "share", names_from = c("type", "year", "ind"), names_sort = TRUE)

res.full <- merge(res.full, res.2, by = "country", all.x = TRUE)

# Rounding the digits for printing
res.table <- res.full
res.table[, c("glre.g", "glre.hts", "index")] <- round(res.table[, c("glre.g", "glre.hts", "index")], 2)
ind.eqs <- grepl("data|baseline|count_", colnames(res.table))
res.table.r1 <- res.table
res.table.r2 <- res.table
res.table[, ind.eqs] <- round(res.table[, ind.eqs] * 100, 0)
res.table.r2[, ind.eqs] <- round(res.table.r1[, ind.eqs] * 100, 2) # Rounded to 2 decimal points.

clean.res.table <- function(res.table) {
  cols.bts <- grepl("BTS", colnames(res.table))
  res.table[, cols.bts] <- NULL
  ## res.table[, ]

  ## Remove 1995 info for counterfactuals (they are the same with the baseline anyway.)
  cols.rm <- unlist(lapply(paste0("count_", c("only_s", "only_g", "no"), "_1995"), function(x) paste0(x, c("_G", "_HTS"))))
  prod(cols.rm %in% colnames(res.table))
  res.table[, cols.rm] <- NULL
  return(res.table)
}

res.table <- clean.res.table(res.table)
res.table[, 6:19] <- lapply(res.table[, 6:19], as.integer)

res.table.r2 <- clean.res.table(res.table.r2)

# Add GDP information
gdp <- read.csv("./output/cleaned_data/gdp_per_capita.csv")
gdp <- subset(gdp, year == "X2018")

res.all <- merge(res.table, gdp[, c("country", "gdp")], by = "country", all.x = TRUE)
res.all$gdp <- round(res.all$gdp / 1e6, 1) # unit: trillion dollars
res.all <- res.all[order(-res.all$gdp), ]
res.all <- res.all[, c(1, ncol(res.all), 2:(ncol(res.all) - 1))]
res.all <- rbind(subset(res.all, country != "ROW"), subset(res.all, country == "ROW"))
rownames(res.all) <- NULL

sink(file = "./doc/tables/full_result.tex")
print(xtable(res.all, floating = FALSE, digits = c(0, 0, 1, 0, 2, 2, 2, rep(0, 14))))
sink()

res.intro <- subset(res.full, country %in% c("CHN", "IND", "VNM", "LTU", "USA"))

write.csv(res.intro, "./doc/tables/five_country_result.csv", row.names = FALSE)

# Get effect measured in percentage points
get.abs.effect <- function(res, index, name.eq = "count_no", name.index = "index") {
  res.18 <- subset(res, type %in% c("baseline", name.eq) & year == 2018)
  res.18 <- pivot_wider(res.18, names_from = "type", values_from = "share")
  res.18$abs.effect <- (res.18$baseline - res.18[, name.eq, drop = TRUE]) * 100 # In percentage points.
  res.18 <- merge(res.18, index[, c("country", name.index)], by = "country", all = TRUE)
  res.18 <- res.18[, c("country", "ind", "abs.effect", name.index)]
  return(res.18)
}

res.18 <- get.abs.effect(res, index)

res.18.s <- get.abs.effect(res, index, name.eq = "count_only_g", name.index = "glre.hts")
res.18.s <- add.dev(res.18.s)

res.18.g <- get.abs.effect(res, index, name.eq = "count_only_s", name.index = "glre.g")
res.18.g <- add.dev(res.18.g)

# Table for top 10 countries
gdp <- read.csv("./output/cleaned_data/gdp_per_capita.csv")
gdp <- subset(gdp, year == "X2018")

res.top10 <- merge(res.table.r2, gdp[, c("country", "gdp")], by = "country", all.x = TRUE)
res.top10$gdp <- round(res.top10$gdp / 1e6, 1) # unit: trillion dollars
res.top10 <- res.top10[order(-res.top10$gdp), ]
res.top10 <- subset(res.top10, country != "ROW")

cols.kp <- unlist(lapply(paste0("count_", c("only_s", "only_g", "no"), "_2018"), function(x) paste0(x, c("_G", "_HTS"))))

res.top10 <- res.top10[1:10, c(
  "country", "gdp", "dev", "glre.g", "glre.hts", "index",
  "baseline_1995_G", "baseline_1995_HTS", "baseline_2018_G", "baseline_2018_HTS", cols.kp
)]
row.names(res.top10) <- NULL

write.csv(res.top10, "./doc/tables/result_top10.csv", row.names = FALSE)

writeLines(paste("For top 10 countries in GDP, the gap in goods share of GDP in 2018 across the baseline and counterfactual 3 are as follows:", paste(res.top10$baseline_2018_G - res.top10$count_no_2018_G, collapse = ", ")), "./doc/numbers/top_10_gap_g.txt")

write.csv(subset(res.top10, country == "CHN"), "./doc/numbers/result_chn.csv", row.names = FALSE)

sink(file = "./doc/tables/result_top10.tex")
print(xtable(res.top10, floating = FALSE, digits = c(0, 0, 1, 0, 2, 2, 2, rep(1, 10))), include.rownames = FALSE)
sink()





## Main plots
# There are outliers!
head(res.18[order(-abs(res.18$abs.effect)), ], n = 12)
head(res.18[order(-abs(res.18$index)), ], n = 12)

# Xlim Ylim for main plots
x.lim.num <- 1.19
y.lim.num <- 21.5
x.lim <- c(-x.lim.num, x.lim.num)
y.lim <- c(-y.lim.num, y.lim.num)

# Function to check outliers
check.outliers <- function(res.18, x.lim.num, y.lim.num) {
  outliers <- subset(res.18, (abs(abs.effect) > y.lim.num) | (abs(index) > x.lim.num))
  return(unique(outliers$country))
}

outl <- check.outliers(res.18, x.lim.num, y.lim.num) # Outliers: SAU, BRN, LUX

# Plotting without any arrows first.
plot.index.no.arrow <- function(res.18, x, i.color, x.lim, y.lim, paper = FALSE, x.lab = "Index", x.var = "index", lm.line = TRUE, outlier.countries = c("SAU", "BRN", "LUX")) {
  themes <- list(theme(text = element_text(size = 15), plot.margin = unit(c(5.5, 5.5, 30, 80), "pt")))
  if (paper == TRUE) {
    themes <- append(list(theme_bw(), theme(plot.title = element_text(hjust = 0.5, size = 15))), themes)
  }

  pp <- ggplot(data = subset(res.18, ind == x & !(country %in% outlier.countries)), aes(x = !!sym(x.var), y = abs.effect)) +
    geom_text(aes(label = country), color = i.color) +
    coord_cartesian(xlim = x.lim, ylim = y.lim, clip = "off") +
    scale_x_continuous(limits = x.lim, expand = c(0, 0)) +
    scale_y_continuous(limits = y.lim, expand = c(0, 0)) +
    xlab(x.lab) +
    ylab(expression(Effect[i]^3 ~ "(p.p.)")) +
    themes +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted")

  if (lm.line) {
    pp <- pp +
      geom_smooth(method = "lm", alpha = 0.3, linewidth = 0.5, color = i.color, fullrange = TRUE)
  }

  return(pp)
}

# Draw arrows
draw.arrows <- function(start.x, start.y, finish.x, finish.y) {
  annotation_custom(segmentsGrob(x0 = start.x, y0 = start.y, x1 = finish.x, y1 = finish.y, gp = gpar(lwd = 1, fill = "black"), arrow = arrow(angle = 15, length = unit(5, "mm"), type = "closed")))
}

draw.arrows.four <- function(x.dist.mid, x.dist.end, y.loc, y.dist.mid, y.dist.end, x.loc) {
  list(
    draw.arrows(0.5 + x.dist.mid, y.loc, 1 - x.dist.end, y.loc),
    draw.arrows(0.5 - x.dist.mid, y.loc, 0 + x.dist.end, y.loc),
    draw.arrows(x.loc, 0.5 + y.dist.mid, x.loc, 1 - y.dist.end),
    draw.arrows(x.loc, 0.5 - y.dist.mid, x.loc, 0 + y.dist.end)
  )
}

# Annotate arrows for x-axis
annotate.arrow.x <- function(x.left, x.right, ffsize = 14) {
  list(annotation_custom(textGrob(TeX(r"($\Delta(\tau)$ \textbf{weakens} $CA_i^g$)"), x = x.right[1], y = x.right[2], hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(r"($\Delta(\tau)$ \textbf{strengthens} $CA_i^g$)"), x = x.left[1], y = x.left[2], hjust = 0, gp = gpar(fontsize = ffsize))))
}

# Annotate arrows for y-axis
annotate.arrow.y <- function(x.loc, y.loc, da.v.ad, from.v.to, i.sec, ffsize = 14) {
  dd <- "decelerates"
  aa <- "accelerates"
  if (da.v.ad == "da") {
    da.1 <- dd
    da.2 <- aa
  } else {
    da.1 <- aa
    da.2 <- dd
  }

  arrow.inner <- function(da) {
    paste0(
      "r\"(\\overset{\\normalsize{$\\Delta(\\tau)$}}{\\overset{\\normalsize{\\textbf{",
      da,
      "}}}{\\overset{\\normalsize{str. trans.}}{\\normalsize{",
      from.v.to,
      " $",
      i.sec,
      "$}}}})\""
    )
  }

  arrow.up <- arrow.inner(da.1)
  arrow.down <- arrow.inner(da.2)

  list.arrows <- list(annotation_custom(textGrob(TeX(eval(parse(text = arrow.up))), x = x.loc, y = 0.5 + y.loc, hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(eval(parse(text = arrow.down))), x = x.loc, y = 0.5 - y.loc, hjust = 0, gp = gpar(fontsize = ffsize))))

  return(list.arrows)
}

# Arrow location for plots
pp.x.dist.mid <- 0.06
pp.x.dist.end <- 0.05
pp.y.loc <- -0.115

pp.y.dist.mid <- 0.2
pp.y.dist.end <- 0.05
pp.x.loc <- -0.068 # -0.06

# X arrow annotation location
pp.xarrow.left <- c(0.15, -0.16)
pp.xarrow.right <- c(0.66, -0.16)

# Y arrow annotation location
pp.yarrow.x.loc <- -0.19
pp.yarrow.y.loc <- 0.27

draw.arrows.paper <- draw.arrows.four(pp.x.dist.mid, pp.x.dist.end, pp.y.loc, pp.y.dist.mid, pp.y.dist.end, pp.x.loc)

base.plot <- function(res.18, lm.line.2 = TRUE, outlier.countries.2 = c("SAU", "BRN", "LUX")) {
  p1 <- plot.index.no.arrow(res.18, "G", "black", x.lim, y.lim, paper = TRUE, lm.line = lm.line.2, outlier.countries = outlier.countries.2) + ggtitle("(a) Goods") +
    draw.arrows.paper +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
    annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "da", "from", "G", ffsize = 12)

  p2 <- plot.index.no.arrow(res.18, "HTS", "red", x.lim, y.lim, paper = TRUE, lm.line = lm.line.2, outlier.countries = outlier.countries.2) + ggtitle("(b) Highly Tradable Services") +
    draw.arrows.paper +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
    annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "HTS", ffsize = 12)

  p3 <- plot.index.no.arrow(res.18, "BTS", "blue", x.lim, y.lim, paper = TRUE, lm.line = lm.line.2, outlier.countries = outlier.countries.2) + ggtitle("(c) Barely Tradable Services") +
    draw.arrows.paper +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
    annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "BTS", ffsize = 12)

  plot_grid(p1, p2, p3, ncol = 1)
}

pdf(file = "./doc/figures/res_base.pdf", width = 10, height = 13)
base.plot(res.18)
dev.off()

write.csv(res.18, "./doc/figures/res_base.csv", row.names = FALSE)

# Function to write outlier information to file
write.outliers <- function(outlier_countries) {
  if (length(outlier_countries) == 0) {
    outlier_text <- "No outliers identified for the plots."
  } else {
    country_list <- paste0(seq_along(outlier_countries), ". ", outlier_countries, collapse = "\n")
    outlier_text <- paste0(
      length(outlier_countries), " countries are identified as outliers and excluded from the plots:\n\n",
      country_list, "\n\n",
      "These countries are excluded from the index vs. absolute effect plots due to their extreme values that fall outside the specified plot limits for either the index or absolute effect variables."
    )
  }
}

writeLines(write.outliers(outl), "./doc/figures/res_base_outliers.txt")





## Other plots
# Plots for Head and Ries trade costs
res.18.hs <- get.abs.effect(res.hs, index.hs)
pdf(file = "./doc/figures/res_hs.pdf", width = 10, height = 13)
base.plot(res.18.hs, lm.line.2 = FALSE, outlier.countries.2 = c(""))
dev.off()

write.csv(res.18.hs, "./doc/figures/res_hs.csv", row.names = FALSE)

outl.hs <- check.outliers(res.18.hs, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.hs), "./doc/figures/res_hs_outliers.txt")

# Plot for the interaction case
res.18.interaction <- get.abs.effect(res.interaction, index.interaction)

pdf(file = "./doc/figures/res_interaction.pdf", width = 10, height = 13)
base.plot(res.18.interaction)
dev.off()

write.csv(res.18.interaction, "./doc/figures/res_interaction.csv", row.names = FALSE)

outl.interaction <- check.outliers(res.18.interaction, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.interaction), "./doc/figures/res_interaction_outliers.txt")





## Summary statistics table to compare Baseline, Non-tradable-services case, and HS case
get.sum.table <- function(res.18) {
  res.18$ind <- factor(res.18$ind, levels = c("G", "HTS", "BTS"))
  sum.table.res <- res.18 %>%
    group_by(ind) %>%
    reframe(
      stats = c("Mean", "Q1", "Q2", "Q3"),
      value = c(mean(abs(abs.effect)), quantile(abs(abs.effect), c(0.25, 0.5, 0.75)))
    )
  sum.table.res <- pivot_wider(sum.table.res, names_from = "stats", values_from = "value")
  sum.table.res$Sector <- c("Goods", "Highly tradable services", "Barely tradable services")[match(sum.table.res$ind, c("G", "HTS", "BTS"))]
  return(sum.table.res)
}

sum.table.res <- get.sum.table(res.18)
sum.table.res$Observations <- as.integer(67)

list.sum.stats <- c("Mean", "Q1", "Q2", "Q3")

sum.table.res.print <- sum.table.res[, c("Sector", "Observations", list.sum.stats)]
sum.table.res.print <- data.frame(sum.table.res.print)

# Print the result
xt <- xtable(sum.table.res.print, align = "rlccccc", digits = 1)
add_to_row <- list(pos = list(-1), command = c("\\hline ")) # Define where to add the extra \hline

sink(file = "./doc/tables/summary_effect.tex")
print(xt, include.rownames = FALSE, floating = FALSE, add.to.row = add_to_row) # Print the table with the extra \hline
sink()

# Table with 3 specifications
sum.table.res.hs <- get.sum.table(res.18.hs)

res.18.no.s.trade <- get.abs.effect(res.no.s.trade, index)
res.18.no.s.trade$index <- NULL
sum.table.res.no.s <- get.sum.table(res.18.no.s.trade)

sum.table.all <- merge(sum.table.res.print, merge(sum.table.res.no.s, sum.table.res.hs, by = "Sector", suffixes = c(".no.s", ".hs")), by = "Sector")
sum.table.all <- sum.table.all[c(2, 3, 1), ]
sum.table.all[, c("ind.hs", "ind.no.s")] <- NULL
sum.table.all$Sector
sum.table.all$Sector <- c("G", "HTS", "BTS")

sink(file = "./doc/tables/summary_effect_all.tex")
print(xtable(sum.table.all, digits = 1), include.rownames = FALSE, floating = FALSE)
sink()

# Non-tradable services case: Table for each country
tab.no.s <- res.18.no.s.trade[, c("country", "ind", "abs.effect")]
tab.no.s <- pivot_wider(tab.no.s, values_from = "abs.effect", names_from = "ind")
tab.no.s <- tab.no.s[order(tab.no.s$country), ]
tab.no.s <- rbind(subset(tab.no.s, country != "ROW"), subset(tab.no.s, country == "ROW"))
tab.no.s <- tab.no.s[, c("country", "G", "HTS", "BTS")]
summary(abs(tab.no.s$G))
summary(abs(tab.no.s$HTS))
summary(abs(tab.no.s$BTS))
tab.no.s.before.r <- tab.no.s
tab.no.s[, c("G", "HTS", "BTS")] <- lapply(tab.no.s[, c("G", "HTS", "BTS")], function(x) sprintf("%.1f", round(x, 1)))

cbind.emp <- function(...) {
  args <- list(...)
  max_length <- max(sapply(args, nrow))
  args <- lapply(args, function(x) as.data.frame(lapply(x, function(col) c(col, rep("", max_length - length(col))))))
  return(do.call(cbind, args))
}

# Here, split the rows to fit in the paper width.
tab.no.s <- cbind.emp(tab.no.s[1:20, ], tab.no.s[21:40, ], tab.no.s[41:60, ], tab.no.s[61:nrow(tab.no.s), ])

sink(file = "./doc/tables/summary_no_serv_trd.tex")
print(xtable(tab.no.s), include.rownames = FALSE, floating = FALSE)
sink()

# Model without services trade discussion
# What countries are the exceptions?
dat.exception <- subset(res.18.no.s.trade, (abs.effect > 0 & ind == "G") | (abs.effect < 0 & ind == "HTS") | (abs.effect < 0 & ind == "BTS"))
count.bigger.bts <- sum(abs(tab.no.s.before.r$BTS) > abs(tab.no.s.before.r$HTS))

writeLines(paste0("Excluding ", paste(unique(dat.exception$country), collapse = ", "), " in the model without tradable services, globalization accelerated structural transformation of all countries. Furthermore, the max gap between the baseline and no globalization scenario in p.p. was ", max(abs(dat.exception$abs.effect)), ". ", "The absolute value of the effect on BTS was larger than HTS in the following number of countries: ", count.bigger.bts), "./doc/numbers/model_without_tradable_services_exception.txt")





## Plots for robustness checks
# Alternative sectoral definition: G PS CS
res.18.alt <- get.abs.effect(res.alt, index.alt)

outl.alt <- check.outliers(res.18.alt, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.alt), "./doc/figures/res_alt_outliers.txt")
res.18.alt <- subset(res.18.alt, !country %in% outl.alt)

p1 <- plot.index.no.arrow(res.18.alt, "G", "black", x.lim, y.lim, paper = TRUE) + ggtitle("(a) Goods") +
  draw.arrows.paper +
  annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
  annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "da", "from", "G", ffsize = 12)

p2 <- plot.index.no.arrow(res.18.alt, "PS", "orange", x.lim, y.lim, paper = TRUE) + ggtitle("(b) Producer services") +
  draw.arrows.paper +
  annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
  annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "PS", ffsize = 12)

p3 <- plot.index.no.arrow(res.18.alt, "CS", "green4", x.lim, y.lim, paper = TRUE) + ggtitle("(c) Consumer services") +
  draw.arrows.paper +
  annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
  annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "CS", ffsize = 12)

pdf(file = "./doc/figures/res_alt.pdf", width = 10, height = 13)
plot_grid(p1, p2, p3, ncol = 1)
dev.off()

# Two-sector version
res.18.gs <- get.abs.effect(res.gs, index.gs)

outl.gs <- check.outliers(res.18.gs, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.gs), "./doc/figures/res_gs_outliers.txt")
res.18.gs <- subset(res.18.gs, !country %in% outl.gs)

p1 <- plot.index.no.arrow(res.18.gs, "G", "black", x.lim, y.lim, paper = TRUE) + ggtitle("(a) Goods") +
  draw.arrows.paper +
  annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
  annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "da", "from", "G", ffsize = 12)

p2 <- plot.index.no.arrow(res.18.gs, "S", "purple", x.lim, y.lim, paper = TRUE) + ggtitle("(b) Services") +
  draw.arrows.paper +
  annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12) +
  annotate.arrow.y(pp.yarrow.x.loc, pp.yarrow.y.loc, "ad", "to", "S", ffsize = 12)

pdf(file = "./doc/figures/res_gs.pdf", width = 10, height = 8.6666)
plot_grid(p1, p2, ncol = 1)
dev.off()

# Non-tradable BTS
res.18.bts <- get.abs.effect(res.bts, index)

outl.bts <- check.outliers(res.18.bts, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.bts), "./doc/figures/res_bts_outliers.txt")
res.18.bts <- subset(res.18.bts, !country %in% outl.bts)

pdf(file = "./doc/figures/res_bts.pdf", width = 10, height = 13)
base.plot(res.18.bts)
dev.off()

# High trade elasticity for services
res.18.high <- get.abs.effect(res.high, index.high)

outl.high <- check.outliers(res.18.high, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.high), "./doc/figures/res_high_outliers.txt")
res.18.high <- subset(res.18.high, !country %in% outl.high)

pdf(file = "./doc/figures/res_high.pdf", width = 10, height = 13)
base.plot(res.18.high)
dev.off()

# Low trade elasticity for services
res.18.low <- get.abs.effect(res.low, index.low)

outl.low <- check.outliers(res.18.low, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.low), "./doc/figures/res_low_outliers.txt")
res.18.low <- subset(res.18.low, !country %in% outl.low)

pdf(file = "./doc/figures/res_low.pdf", width = 10, height = 13)
base.plot(res.18.low)
dev.off()

# Wedges approach
res.18.wedge <- get.abs.effect(res.wedge, index.orig)

outl.wedge <- check.outliers(res.18.wedge, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.wedge), "./doc/figures/res_wedge_outliers.txt")
res.18.wedge <- subset(res.18.wedge, !country %in% outl.wedge)

pdf(file = "./doc/figures/res_wedge.pdf", width = 10, height = 13)
base.plot(res.18.wedge)
dev.off()

# Role of NX
res.18.nx <- get.abs.effect(res.nx, index)

outl.nx <- check.outliers(res.18.nx, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.nx), "./doc/figures/res_nx_outliers.txt")
res.18.nx <- subset(res.18.nx, !country %in% outl.nx)

pdf(file = "./doc/figures/res_nx.pdf", width = 10, height = 13)
base.plot(res.18.nx)
dev.off()

# Nested production function
res.18.nest <- get.abs.effect(res.nest, index)

outl.nest <- check.outliers(res.18.nest, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.nest), "./doc/figures/res_nest_outliers.txt")
res.18.nest <- subset(res.18.nest, !country %in% outl.nest)

pdf(file = "./doc/figures/res_nest.pdf", width = 10, height = 13)
base.plot(res.18.nest)
dev.off()

# No adjustments for (Trade costs < 1)
res.18.orig <- get.abs.effect(res.orig, index.orig)
outl.orig <- check.outliers(res.18.orig, x.lim.num, y.lim.num)
writeLines(write.outliers(outl.orig), "./doc/figures/res_orig_outliers.txt")
res.18.orig <- subset(res.18.orig, !country %in% outl.orig)

pdf(file = "./doc/figures/res_orig.pdf", width = 10, height = 13)
base.plot(res.18.orig)
dev.off()





## Developing vs Advanced countries, Goods vs Services

# Arrow locations
gs.x.dist.mid <- 0.06
gs.x.dist.end <- 0.05
gs.y.loc <- -0.16

gs.y.dist.mid <- 0.2
gs.y.dist.end <- 0.05
gs.x.loc <- -0.06

# X arrow annotation location for paper plots
gs.xarrow.left <- c(0.15, -0.2)
gs.xarrow.right <- c(0.66, -0.2)

# Y arrow annotation location for paper plots
gs.yarrow.x.loc <- -0.19
gs.yarrow.y.loc <- 0.27

draw.arrows.gs <- draw.arrows.four(gs.x.dist.mid, gs.x.dist.end, gs.y.loc, gs.y.dist.mid, gs.y.dist.end, gs.x.loc)

# Annotate arrows for x-axis

annotate.arrow.x.gs <- function(which.ind, x.left, x.right, ffsize = 14) {
  if (which.ind == "g") {
    list(annotation_custom(textGrob(TeX(r"($\Delta(\tau^g)$ \textbf{weakens} $CA_i^g$)"), x = x.right[1], y = x.right[2], hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(r"($\Delta(\tau^g)$ \textbf{strengthens} $CA_i^g$)"), x = x.left[1], y = x.left[2], hjust = 0, gp = gpar(fontsize = ffsize))))
  } else {
    list(annotation_custom(textGrob(TeX(r"($\Delta(\tau^{hts})$ \textbf{strengthens} $CA_i^g$)"), x = x.right[1], y = x.right[2], hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(r"($\Delta(\tau^{hts})$ \textbf{weakens} $CA_i^g$)"), x = x.left[1], y = x.left[2], hjust = 0, gp = gpar(fontsize = ffsize))))
  }
}

## Annotate arrows for y-axis
annotate.arrow.y.gs <- function(x.loc, y.loc, da.v.ad, from.v.to, i.sec, ffsize = 14, gs = "G") {
  dd <- "decelerates"
  aa <- "accelerates"
  if (da.v.ad == "da") {
    da.1 <- dd
    da.2 <- aa
  } else {
    da.1 <- aa
    da.2 <- dd
  }

  arrow.inner <- function(da) {
    paste0(
      "r\"(\\overset{\\normalsize{$\\Delta(\\tau^{",
      gs,
      "})$}}{\\overset{\\normalsize{\\textbf{",
      da,
      "}}}{\\overset{\\normalsize{str. trans.}}{\\normalsize{",
      from.v.to,
      " $",
      i.sec,
      "$}}}})\""
    )
  }

  arrow.up <- arrow.inner(da.1)
  arrow.down <- arrow.inner(da.2)

  list.arrows <- list(annotation_custom(textGrob(TeX(eval(parse(text = arrow.up))), x = x.loc, y = 0.5 + y.loc, hjust = 0, gp = gpar(fontsize = ffsize))), annotation_custom(textGrob(TeX(eval(parse(text = arrow.down))), x = x.loc, y = 0.5 - y.loc, hjust = 0, gp = gpar(fontsize = ffsize))))

  return(list.arrows)
}

# Xlim Ylim
x.lim.g <- 2.6 * c(-1, 1)
y.lim.g <- 42.4 * c(-1, 1)

base.plot.g.hts <- function(res.18, which.ind, x.lab.inner) {
  x.var.inner <- paste0("glre.", which.ind)
  p1 <- plot.index.no.arrow(res.18, "G", "black", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.inner, x.var = x.var.inner) + ggtitle("(a) Goods") +
    draw.arrows.gs +
    annotate.arrow.x.gs(which.ind, gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
    annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "da", "from", "G", ffsize = 12, gs = which.ind)

  p2 <- plot.index.no.arrow(res.18, "HTS", "red", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.inner, x.var = x.var.inner) + ggtitle("(b) Highly Tradable Services") +
    draw.arrows.gs +
    annotate.arrow.x.gs(which.ind, gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
    annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "HTS", ffsize = 12, gs = which.ind)

  p3 <- plot.index.no.arrow(res.18, "BTS", "blue", x.lim.g, y.lim.g, paper = TRUE, x.lab = x.lab.inner, x.var = x.var.inner) + ggtitle("(c) Barely Tradable Services") +
    draw.arrows.gs +
    annotate.arrow.x.gs(which.ind, gs.xarrow.left, gs.xarrow.right, ffsize = 12) +
    annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "BTS", ffsize = 12, gs = which.ind)

  plot_grid(p1, p2, p3, ncol = 1)
}

pdf(file = "./doc/figures/res_da_g.pdf", width = 10, height = 13)
base.plot.g.hts(res.18.g, "g", "Growth rate of relative export trade costs (goods)")
dev.off()

pdf(file = "./doc/figures/res_da_hts.pdf", width = 10, height = 13)
base.plot.g.hts(res.18.s, "hts", "Growth rate of relative export trade costs (highly tradable services)")
dev.off()

# Relationship with GDP.
plot.index.gdp <- function(res.18, i.ind, i.color, x.lim, y.lim, paper = FALSE, outlier.countries = c("SAU", "BRN", "LUX")) {
  themes <- list(theme(text = element_text(size = 15), plot.margin = unit(c(5.5, 5.5, 30, 80), "pt")))
  if (paper == TRUE) {
    themes <- append(list(theme_bw(), theme(plot.title = element_text(hjust = 0.5, size = 15))), themes)
  }

  pp <- ggplot(data = subset(res.18, ind == i.ind & !(country %in% outlier.countries)), aes(x = log(gdppc), y = abs.effect)) +
    geom_text(aes(label = country), color = i.color) +
    coord_cartesian(xlim = x.lim, ylim = y.lim, clip = "off") +
    scale_x_continuous(limits = x.lim, expand = c(0, 0)) +
    scale_y_continuous(limits = y.lim, expand = c(0, 0)) +
    geom_smooth(method = "lm", alpha = 0.3, linewidth = 0.5, color = i.color, fullrange = TRUE) +
    xlab("Log(GDP per capita in 1995)") +
    ylab("Effect (p.p.)") +
    themes +
    geom_hline(yintercept = 0, linetype = "dotted")

  return(pp)
}

draw.arrows.two <- function(y.dist.mid, y.dist.end, x.loc) {
  list(
    draw.arrows(x.loc, 0.5 + y.dist.mid, x.loc, 1 - y.dist.end),
    draw.arrows(x.loc, 0.5 - y.dist.mid, x.loc, 0 + y.dist.end)
  )
}

draw.arrows.gdp <- draw.arrows.two(gs.y.dist.mid, gs.y.dist.end, gs.x.loc)

gdp <- read.csv("./output/cleaned_data/gdp_per_capita.csv")
gdp <- subset(gdp, year == "X1995")[, c("country", "gdppc")]

add.gdp <- function(res) merge(res, gdp, by = "country", all.x = TRUE)

res.18 <- add.gdp(res.18)

res.18.g <- add.gdp(res.18.g)

res.18.s <- add.gdp(res.18.s)

x.lim.gdp <- c(5.93, 11.64)
y.lim.gdp <- 40 * c(-1, 1)

exception.gdp <- c("BRN")

base.plot.gdp <- function(res.18) {
  p1 <- plot.index.gdp(res.18, "G", "black", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(i) Goods") +
    draw.arrows.gdp +
    annotate.arrow.y(gs.yarrow.x.loc, gs.yarrow.y.loc, "da", "from", "G", ffsize = 12)

  p2 <- plot.index.gdp(res.18, "HTS", "red", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(ii) Highly Tradable Services") +
    draw.arrows.gdp +
    annotate.arrow.y(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "HTS", ffsize = 12)

  p3 <- plot.index.gdp(res.18, "BTS", "blue", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(iii) Barely Tradable Services") +
    draw.arrows.gdp +
    annotate.arrow.y(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "BTS", ffsize = 12)

  plot_grid(p1, p2, p3, ncol = 1)
}

pdf(file = "./doc/figures/res_gdp_all.pdf", width = 10, height = 20)
base.plot.gdp(res.18)
dev.off()

base.plot.gs.gdp <- function(res.18, g.s = "g") {
  p1 <- plot.index.gdp(res.18, "G", "black", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(i) Goods") +
    draw.arrows.gdp +
    annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "da", "from", "G", ffsize = 12, gs = g.s)

  p2 <- plot.index.gdp(res.18, "HTS", "red", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(ii) Highly Tradable Services") +
    draw.arrows.gdp +
    annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "HTS", ffsize = 12, gs = g.s)

  p3 <- plot.index.gdp(res.18, "BTS", "blue", x.lim.gdp, y.lim.gdp, paper = TRUE, outlier.countries = exception.gdp) + ggtitle("(iii) Barely Tradable Services") +
    draw.arrows.gdp +
    annotate.arrow.y.gs(gs.yarrow.x.loc, gs.yarrow.y.loc, "ad", "to", "BTS", ffsize = 12, gs = g.s)

  plot_grid(p1, p2, p3, ncol = 1)
}

pdf(file = "./doc/figures/res_gdp_g.pdf", width = 10, height = 20)
base.plot.gs.gdp(res.18.g)
dev.off()

summary(lm(abs.effect ~ gdppc, subset(res.18.g, ind == "G")))
summary(lm(abs.effect ~ gdppc, subset(res.18.g, ind == "HTS")))
summary(lm(abs.effect ~ gdppc, subset(res.18.s, ind == "G")))
summary(lm(abs.effect ~ gdppc, subset(res.18.s, ind == "HTS")))

pdf(file = "./doc/figures/res_gdp_hts.pdf", width = 10, height = 20)
base.plot.gs.gdp(res.18.s, g.s = "hts")
dev.off()

# Three plots together to include in the paper
plot.index.gdp.2 <- function(res.18, i.ind, i.color, x.lim, y.lim, paper = FALSE, outlier.countries = c("SAU", "BRN", "LUX"), effect.num) {
  ylab.inside <- bquote(Effect[i]^.(effect.num) ~ "(p.p.)")

  themes <- list(theme(text = element_text(size = 15), plot.margin = unit(c(5.5, 6, 5.5, 5.5), "pt")))

  if (paper == TRUE) {
    themes <- append(list(theme_bw(), theme(plot.title = element_text(hjust = 0.5, size = 15))), themes)
  }

  pp <- ggplot(data = subset(res.18, ind == i.ind & !(country %in% outlier.countries)), aes(x = log(gdppc), y = abs.effect)) +
    geom_text(aes(label = country), color = i.color) +
    coord_cartesian(xlim = x.lim, ylim = y.lim, clip = "off") +
    scale_x_continuous(limits = x.lim, expand = c(0, 0)) +
    scale_y_continuous(limits = y.lim, expand = c(0, 0)) +
    geom_smooth(method = "lm", alpha = 0.3, linewidth = 0.5, color = i.color, fullrange = TRUE) +
    ylab(ylab.inside) +
    xlab(NULL) +
    themes +
    geom_hline(yintercept = 0, linetype = "dotted")

  return(pp)
}

x.lim.gdp.2 <- c(5.6, 12.0)
y.lim.gdp.2 <- 41 * c(-1, 1)

base.plot.gdp.2 <- function(res.18, num.effect.i, out.title) {
  # Create individual plots with titles
  p1 <- plot.index.gdp.2(res.18, "G", "black", x.lim.gdp.2, y.lim.gdp.2, paper = TRUE, outlier.countries = exception.gdp, num.effect.i) + ggtitle("(i) Goods")
  p2 <- plot.index.gdp.2(res.18, "HTS", "red", x.lim.gdp.2, y.lim.gdp.2, paper = TRUE, outlier.countries = exception.gdp, num.effect.i) + ggtitle("(ii) HTS")
  p3 <- plot.index.gdp.2(res.18, "BTS", "blue", x.lim.gdp.2, y.lim.gdp.2, paper = TRUE, outlier.countries = exception.gdp, num.effect.i) + ggtitle("(iii) BTS")

  return(plot_grid(p1, p2, p3, ncol = 1))
}

# Run the function with your inputs
pg1 <- base.plot.gdp.2(res.18.g, 1)
pg2 <- base.plot.gdp.2(res.18.s, 2)
pg3 <- base.plot.gdp.2(res.18, 3)

pg4 <- plot_grid(pg1, pg2, pg3, nrow = 1)

# Add individual titles for pg1, pg2, and pg3
x.title.adj <- 0.031

final_plot <- ggdraw() +
  draw_label("(a) Effect 1: G", x = 0.17 + x.title.adj, y = 1, vjust = 1.5, size = 16) +
  draw_label("(b) Effect 2: S", x = 0.5 + x.title.adj, y = 1, vjust = 1.5, size = 16) +
  draw_label("(c) Effect 3: G & S ", x = 0.83 + x.title.adj, y = 1, vjust = 1.5, size = 16) +
  draw_plot(pg4, 0, 0.04, 0.99, 0.92) +
  draw_label("Log(GDP per capita in 1995)", x = 0.531, y = 0, vjust = -2, size = 14)

pdf(file = "./doc/figures/res_gdp.pdf", width = 10, height = 13)
final_plot
dev.off()





## Relative measures instead of absoluate measures
# Get effect measured in %
get.rel.effect <- function(res, index, name.eq = "count_no", name.index = "index") {
  res.18 <- subset(res, type %in% c("baseline", name.eq) & year %in% c(1995, 2018))
  res.18$type <- ifelse(res.18$type == "baseline", "b", "c")
  res.18 <- pivot_wider(res.18, names_from = c("type", "year"), values_from = "share")
  res.18$rel.effect <- ((res.18$b_1995 - res.18$b_2018) - (res.18$c_1995 - res.18$c_2018)) / (res.18$b_1995 - res.18$b_2018) * 100 # In %
  res.18 <- merge(res.18, index[, c("country", name.index)], by = "country", all = TRUE)
  res.18 <- res.18[, c("country", "ind", "rel.effect", name.index)]
  return(res.18)
}

# Regenerate the income level result
res.rel.g <- get.rel.effect(res, index, name.eq = "count_only_s", name.index = "glre.g")
res.rel.g <- add.dev(res.rel.g)
res.rel.g <- add.gdp(res.rel.g)

res.rel.s <- get.rel.effect(res, index, name.eq = "count_only_g", name.index = "glre.hts")
res.rel.s <- add.dev(res.rel.s)
res.rel.s <- add.gdp(res.rel.s)

res.rel <- get.rel.effect(res, index)
res.rel <- add.dev(res.rel)
res.rel <- add.gdp(res.rel)

outlier.rel <- function(res.rel) {
  temp.1 <- length(res.rel$rel.effect)
  temp.2 <- length(res.rel$rel.effect[abs(res.rel$rel.effect) < 1000])
  temp.3 <- paste0(res.rel$country[abs(res.rel$rel.effect) > 1000], "-", res.rel$ind[abs(res.rel$rel.effect) > 1000])
  temp.5 <- paste0("Among ", temp.1, " country-sector observations, ", temp.2, " exhibited relative effects of more than 1000%. The list of the observations with relative effect greater than 1000% is as follows: ", paste(temp.3, collapse = ", "), ". ", "Brunei is excluded due to extremely high GDP per capita.")
  return(temp.5)
}

# Write down outliers.
writeLines(outlier.rel(res.rel.g), "./doc/figures/res_gdp_rel_outliers_g.txt")
writeLines(outlier.rel(res.rel.s), "./doc/figures/res_gdp_rel_outliers_s.txt")
writeLines(outlier.rel(res.rel), "./doc/figures/res_gdp_rel_outliers_gs.txt")

res.rel.g <- subset(res.rel.g, abs(rel.effect) < 1000 & country != "BRN")
res.rel.s <- subset(res.rel.s, abs(rel.effect) < 1000 & country != "BRN")
res.rel <- subset(res.rel, abs(rel.effect) < 1000 & country != "BRN")

# Three plots together to include in the paper
plot.index.gdp.3 <- function(res.18, i.ind, i.color, x.lim, y.lim, effect.num) {
  ylab.inside <- bquote(RelEff[i]^.(effect.num) ~ "(%)")

  themes <- list(theme(text = element_text(size = 15), plot.margin = unit(c(5.5, 6, 5.5, 5.5), "pt")))
  themes <- append(list(theme_bw(), theme(plot.title = element_text(hjust = 0.5, size = 15))), themes)

  pp <- ggplot(data = subset(res.18, ind == i.ind), aes(x = log(gdppc), y = rel.effect)) +
    geom_text(aes(label = country), color = i.color) +
    coord_cartesian(xlim = x.lim, ylim = y.lim, clip = "off") +
    scale_x_continuous(limits = x.lim, expand = c(0, 0)) +
    scale_y_continuous(limits = y.lim, expand = c(0, 0)) +
    geom_smooth(method = "lm", alpha = 0.3, linewidth = 0.5, color = i.color, fullrange = TRUE) +
    ylab(ylab.inside) +
    xlab(NULL) +
    themes +
    geom_hline(yintercept = 0, linetype = "dotted")

  return(pp)
}

x.lim.gdp.3 <- c(5.6, 12.0)
y.lim.gdp.3 <- 1100 * c(-1, 1)

base.plot.gdp.3 <- function(res.18, num.effect.i, out.title) {
  # Create individual plots with titles
  p1 <- plot.index.gdp.3(res.18, "G", "black", x.lim.gdp.3, y.lim.gdp.3, num.effect.i) + ggtitle("(i) Goods")
  p2 <- plot.index.gdp.3(res.18, "HTS", "red", x.lim.gdp.3, y.lim.gdp.3, num.effect.i) + ggtitle("(ii) HTS")
  p3 <- plot.index.gdp.3(res.18, "BTS", "blue", x.lim.gdp.3, y.lim.gdp.3, num.effect.i) + ggtitle("(iii) BTS")

  return(plot_grid(p1, p2, p3, ncol = 1))
}

# Run the function with your inputs
pg1 <- base.plot.gdp.3(res.rel.g, 1)
pg2 <- base.plot.gdp.3(res.rel.s, 2)
pg3 <- base.plot.gdp.3(res.rel, 3)

pg4 <- plot_grid(pg1, pg2, pg3, nrow = 1)

# Add individual titles for pg1, pg2, and pg3
x.title.adj <- 0.031

final_plot <- ggdraw() +
  draw_label("(a) Effect 1: G", x = 0.17 + x.title.adj, y = 1, vjust = 1.5, size = 16) +
  draw_label("(b) Effect 2: S", x = 0.5 + x.title.adj, y = 1, vjust = 1.5, size = 16) +
  draw_label("(c) Effect 3: G & S ", x = 0.83 + x.title.adj, y = 1, vjust = 1.5, size = 16) +
  draw_plot(pg4, 0, 0.04, 0.99, 0.92) +
  draw_label("Log(GDP per capita in 1995)", x = 0.531, y = 0, vjust = -2, size = 14)

pdf(file = "./doc/figures/res_gdp_rel.pdf", width = 10, height = 13)
final_plot
dev.off()







# Regenerate the main result
res.rel <- get.rel.effect(res, index)

# There are outliers
summary(res.rel$rel.effect)
temp.1 <- length(res.rel$rel.effect)
temp.2 <- length(res.rel$rel.effect[abs(res.rel$rel.effect) < 1000 & abs(res.rel$index) <= x.lim.num])
temp.3 <- paste0(res.rel$country[abs(res.rel$rel.effect) > 1000], "-", res.rel$ind[abs(res.rel$rel.effect) > 1000])
temp.4 <- paste0(res.rel$country[abs(res.rel$index) > x.lim.num], "-", res.rel$ind[abs(res.rel$index) > x.lim.num])
temp.5 <- paste0("Among ", temp.1, " country-sector observations, ", temp.2, " exhibited relative effects of more than 1000% or outlier index values. The list of the observations with relative effect greater than 1000% is as follows: ", paste(temp.3, collapse = ", "), ". ", "The list of observations with outlier index values are as follows: ", paste(temp.4, collapse = ", "), ".")

writeLines(temp.5, "./doc/figures/res_rel_outliers.txt")

res.rel.plot <- subset(res.rel, abs(rel.effect) < 1e3 & abs(index) < x.lim.num)

y.lim.2 <- c(-1e3 - 100, 1e3 + 100)

# Plotting without any arrows first.
plot.index.no.arrow.2 <- function(res.18, x, i.color, x.lim, y.lim, paper = FALSE, x.lab = "Index", x.var = "index", lm.line = TRUE) {
  themes <- list(theme(text = element_text(size = 15), plot.margin = unit(c(5.5, 5.5, 30, 80), "pt")))
  if (paper == TRUE) {
    themes <- append(list(theme_bw(), theme(plot.title = element_text(hjust = 0.5, size = 15))), themes)
  }

  pp <- ggplot(data = subset(res.18, ind == x), aes(x = !!sym(x.var), y = rel.effect)) +
    geom_text(aes(label = country), color = i.color) +
    coord_cartesian(xlim = x.lim, ylim = y.lim, clip = "off") +
    scale_x_continuous(limits = x.lim, expand = c(0, 0)) +
    scale_y_continuous(limits = y.lim, expand = c(0, 0)) +
    xlab(x.lab) +
    ylab(expression(RelEff[i]^3 ~ "(%)")) +
    themes +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted")

  if (lm.line) {
    pp <- pp +
      geom_smooth(method = "lm", alpha = 0.3, linewidth = 0.5, color = i.color, fullrange = TRUE)
  }

  return(pp)
}

# Arrow location for plots
## pp.x.loc.2 <- -0.093

draw.arrows.xaxis <- function(x.dist.mid, x.dist.end, y.loc) {
  list(
    draw.arrows(0.5 + x.dist.mid, y.loc, 1 - x.dist.end, y.loc),
    draw.arrows(0.5 - x.dist.mid, y.loc, 0 + x.dist.end, y.loc)
  )
}

draw.arrows.paper.2 <- draw.arrows.xaxis(pp.x.dist.mid, pp.x.dist.end, pp.y.loc)


# Y arrow annotation location
pp.yarrow.x.loc.2 <- -0.23
pp.yarrow.y.loc.2 <- 0.27


base.plot.2 <- function(res.18, lm.line.2 = TRUE) {
  p1 <- plot.index.no.arrow.2(res.18, "G", "black", x.lim, y.lim.2, paper = TRUE, lm.line = lm.line.2) + ggtitle("(a) Goods") +
    draw.arrows.paper.2 +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12)

  p2 <- plot.index.no.arrow.2(res.18, "HTS", "red", x.lim, y.lim.2, paper = TRUE, lm.line = lm.line.2) + ggtitle("(b) Highly Tradable Services") +
    draw.arrows.paper.2 +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12)

  p3 <- plot.index.no.arrow.2(res.18, "BTS", "blue", x.lim, y.lim.2, paper = TRUE, lm.line = lm.line.2) + ggtitle("(c) Barely Tradable Services") +
    draw.arrows.paper.2 +
    annotate.arrow.x(pp.xarrow.left, pp.xarrow.right, ffsize = 12)

  plot_grid(p1, p2, p3, ncol = 1)
}



pdf(file = "./doc/figures/res_rel.pdf", width = 10, height = 13)
base.plot.2(res.rel.plot)
dev.off()
