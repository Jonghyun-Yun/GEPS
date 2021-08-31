## each line contains
## (eta,  xi,  pi) values for all schools
## some rows have extra space at the end of line. igore warnings
library(magrittr)
library(coda)
## library(dplyr)
## library(stringr)
library(bayesplot)
library(ggplot2)
sim_ms  = readr::read_delim("RESULT/sim_e1.log", delim=" ",  col_names=FALSE)
if (ncol(sim_ms) %% 3 != 0)
  stop("the # of columns are not a multiple of 3.")

nschool = ncol(sim_ms) / 3
niter = nrow(sim_ms)
cname = matrix(c(
  paste0("eta_", 1:nschool),
  paste0("xi_", 1:nschool),
  paste0("pi_", 1:nschool)),
  byrow = F, nrow = nschool) %>% t() %>% c()
colnames(sim_ms) = cname

## mcmc_ms = mcmc(sim_ms)

pdf("TRACE/eta.pdf")
color_scheme_set("mix-blue-pink")
for (a in 1:nschool) {
  p <- mcmc_trace(sim_ms,
    pars = paste0("eta_", a),
    transformations = "log",
    facet_args = list(nrow = 2, labeller = label_parsed)
  )
  print(p <- p + facet_text(size = 15) +
          ggtitle(paste0("eta trace for school ", a, ".")))
}
dev.off(which = dev.cur())

pdf("TRACE/xi.pdf")
dd = data.frame(sim_ms[, grepl("xi", cname)]) %>% reshape2::melt()
names(dd) = c("school", "xi")
dd[, 1] = as.numeric(dd[, 1])
boxp <- ggplot(dd, aes(x=school,y=xi,fill=factor(school))) +
  geom_boxplot() + theme(legend.position = "none") + ylim(0, 1)
boxp
dev.off(which = dev.cur())

dd = data.frame(sim_ms[, grepl("pi", cname)])
apply(dd,  2,  mean)
