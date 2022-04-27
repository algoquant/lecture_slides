# old code

symbols <- c("VTI", "IEF", "DBC")
retsp <- rutils::etfenv$returns[, symbols]
retsp <- na.omit(retsp)

endp <- rutils::calc_endpoints(retsp, interval="months")
# endp <- endp[-1]
nrows <- NROW(endp)
dates <- zoo::index(retsp)[endp]

look_back <- 7
startp <- c(rep_len(0, look_back-1),
            endp[1:(nrows - look_back + 1)])
look_backs <- cbind(startp, endp)
colnames(look_backs) <- c("start", "end")
look_backs <- look_backs[-NROW(look_backs), ]
look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
look_fwds <- look_fwds[-NROW(look_fwds), ]
colnames(look_fwds) <- c("start", "end")

objfun <- function(retsp) sum(retsp)/sd(retsp)
# objfun <- function(retsp) prod(1+retsp)/sd(retsp)

perfstat <- apply(look_backs, 1, function(ep) {
  sapply(retsp[ep[1]:ep[2]], objfun)
})  # end sapply
perfstat <- t(perfstat)
perfstat[is.na(perfstat)] <- 0
weightv <- perfstat
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv[is.na(weightv)] <- 0
# weightv <- weightv[-NROW(weightv), ]
sum(is.na(weightv))

retsos <- apply(look_fwds, 1, function(ep) {
  sapply(retsp[ep[1]:ep[2], ], sum)
})  # end sapply
retsos <- t(retsos)
retsos[is.na(retsos)] <- 0

pnls <- rowSums(weightv*retsos)

foo <- lapply(1:NROW(look_fwds), function(it) {
  ep <- look_fwds[it, ]
  retsp[ep[1]:ep[2], ] %*% weightv[it, ]
})  # end sapply
foo <- drop(do.call(rbind, foo))


# retsos <- rutils::lagit(retsos)
# pnls <- rutils::lagit(pnls)
retsmom <- pnls

weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- retsos %*% weightsaw

wealth <- xts::xts(cbind(all_weather, retsmom), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
sqrt(252)*sapply(wealth, 
                 function(x) c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
dyLegend(show="always", width=500)

retsmom <- foo
all_weather <- retsp %*% weightsaw
wealth <- xts::xts(cbind(all_weather, retsmom), order.by=zoo::index(retsp))
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)


########
# Backup
symbols <- c("VTI", "IEF", "DBC")
retsp <- rutils::etfenv$returns[, symbols]
retsp <- na.omit(retsp)

endp <- rutils::calc_endpoints(retsp, interval="months")
# endp <- endp[-1]
nrows <- NROW(endp)
dates <- zoo::index(retsp)[endp]

look_back <- 7
startp <- c(rep_len(0, look_back-1),
            endp[1:(nrows - look_back + 1)])
look_backs <- cbind(startp, endp)
colnames(look_backs) <- c("start", "end")
look_backs <- look_backs[-NROW(look_backs), ]
look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
look_fwds <- look_fwds[-NROW(look_fwds), ]
colnames(look_fwds) <- c("start", "end")

objfun <- function(retsp) sum(retsp)/sd(retsp)
# objfun <- function(retsp) prod(1+retsp)/sd(retsp)

perfstat <- apply(look_backs, 1, function(ep) {
  sapply(retsp[ep[1]:ep[2]], objfun)
})  # end sapply
perfstat <- t(perfstat)
perfstat[is.na(perfstat)] <- 0
weightv <- perfstat
weightv <- weightv/sqrt(rowSums(weightv^2))
weightv[is.na(weightv)] <- 0
# weightv <- weightv[-NROW(weightv), ]
sum(is.na(weightv))

retsos <- apply(look_fwds, 1, function(ep) {
  sapply(retsp[ep[1]:ep[2], ], sum)
})  # end sapply
retsos <- t(retsos)
retsos[is.na(retsos)] <- 0

pnls <- rowSums(weightv*retsos)
# retsos <- rutils::lagit(retsos)
# pnls <- rutils::lagit(pnls)
retsmom <- pnls

weightsaw <- c(0.30, 0.55, 0.15)
all_weather <- retsos %*% weightsaw

wealth <- xts::xts(cbind(all_weather, retsmom), order.by=dates)
colnames(wealth) <- c("All-Weather", "Momentum")
cor(wealth)
dygraphs::dygraph(cumsum(wealth), main="Monthly Momentum Strategy vs All-Weather") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)


