## plot_one.R
# Plot the results for a single country

# Loading required libraries and functions
library("tidyverse")
library("grid")
library("gridExtra")
library("cowplot")
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





## Plot one country's str change for one equilibrium
# Note: labels, scales, themes, you have to manually add.
plot.one <- function(res, ex.country, ex.type, text_where, ex.title) {
  ggplot() +
    geom_area(data = subset(res, country == ex.country & type == ex.type), aes(x = year, y = share, fill = ind), alpha = 0.5, color = "black", outline.type = "full") +
    scale_fill_manual(values = c("G" = "grey50", "HTS" = "red", "BTS" = "blue")) +
    geom_text(data = text_where, aes(x = xpos, y = ypos, label = name), size = 5) +
    ggtitle(ex.title)
}

# All four plots together for CHN, IND, VNM, LTU
plot.inner <- function(res, ex.country, text_where, inner.type, inner.name, loc) {
  p <- plot.one(res, ex.country, inner.type, text_where, inner.name) +
    theme(axis.title.x = element_blank(), legend.position = "none", axis.text.x = element_text(size = 10), text = element_text(size = 15), plot.title = element_text(hjust = 0.5, size = 11))

  if (loc == "l") {
    p <- p +
      ylab("Production shares") +
      scale_y_continuous(breaks = seq(0, 1, 0.1))
  }

  if (loc == "m") {
    p <- p +
      scale_y_continuous(breaks = seq(0, 1, 0.1)) +
      theme(axis.title.y = element_blank())
  }

  if (loc == "r") {
    p <- p +
      scale_y_continuous(breaks = seq(0, 1, 0.1), sec.axis = dup_axis()) +
      theme(axis.title.y = element_blank())
  }
  return(p)
}

plot.all <- function(res, ex.country, list.text.where, list.titles) {
  plot_grid(plot.inner(res, ex.country, list.text.where[[1]], "baseline", list.titles[[1]], "l"),
    plot.inner(res, ex.country, list.text.where[[2]], "count_only_s", list.titles[[2]], "m"),
    plot.inner(res, ex.country, list.text.where[[3]], "count_only_g", list.titles[[3]], "m"),
    plot.inner(res, ex.country, list.text.where[[4]], "count_no", list.titles[[4]], "r"),
    nrow = 1, rel_widths = c(1.05, 1, 1, 1.07)
  )
}

short.name.inds <- c("BTS", "HTS", "G")

list.titles.paper <- list("Baseline (G, HTS, BTS)", "Counterfactual 1 (HTS & BTS)", "Counterfactual 2 (G)", "Counterfactual 3 (none)")

# Text position for China
xpos.mid <- rep(2006.5, 3)
text.where.china <- rep(list(data.frame(xpos = xpos.mid, ypos = c(0.9, 0.62, 0.2), name = short.name.inds)), 4)

plot.all.country <- function(res, ex.country, text.where, list.titles) {
  pp <- plot.all(res, ex.country, text.where, list.titles)
  x.grob <- textGrob("Year", gp = gpar(fontsize = 15))
  grid.arrange(arrangeGrob(pp, bottom = x.grob))
}

# Baseline
pdf(file = "./doc/figures/single_chn.pdf", width = 10, height = 6)
plot.all.country(res, "CHN", text.where.china, list.titles.paper)
dev.off()

# H-S case
pdf(file = "./doc/figures/single_chn_hs.pdf", width = 10, height = 6)
plot.all.country(res.hs, "CHN", text.where.china, list.titles.paper)
dev.off()

# India
text.where.india <- rep(list(data.frame(xpos = xpos.mid, ypos = c(0.85, 0.57, 0.2), name = short.name.inds)), 4)

pdf(file = "./doc/figures/single_ind.pdf", width = 10, height = 6)
plot.all.country(res, "IND", text.where.india, list.titles.paper)
dev.off()

# Vietname and Lithuania
# Function to denote locations of labels
make.diff.loc.inner <- function(loc) data.frame(xpos = xpos.mid, ypos = loc, name = short.name.inds)
make.diff.loc <- function(list.locs) lapply(list.locs, make.diff.loc.inner)

text.where.vietnam <- make.diff.loc(list(
  c(0.87, 0.65, 0.3),
  c(0.87, 0.55, 0.2),
  c(0.87, 0.7, 0.3),
  c(0.87, 0.62, 0.3)
))

pdf(file = "./doc/figures/single_vnm.pdf", width = 10, height = 6)
plot.all.country(res, "VNM", text.where.vietnam, list.titles.paper)
dev.off()

text.where.lithuania <- make.diff.loc(list(
  c(0.85, 0.5, 0.15),
  c(0.85, 0.37, 0.05),
  c(0.85, 0.61, 0.3),
  c(0.85, 0.5, 0.2)
))

pdf(file = "./doc/figures/single_ltu.pdf", width = 10, height = 6)
pp <- plot.all.country(res, "LTU", text.where.lithuania, list.titles.paper)
dev.off()
