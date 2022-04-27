# new code

npts <- NROW(endp)

pnlsn <- lapply(1:(npts-1), function(it) {
  # Select the look-back returns
  startp <- endp[max(1, it-look_back+1)]
  retslb <- retsp[startp:(endp[it]), ]
  # Calculate the best performing stocks in-sample
  perfstat <- sapply(retslb, objfun)
  perfstat[!is.finite(perfstat)] <- 0
  weightv <- drop(perfstat)
  weightv <- weightv/sqrt(sum(weightv^2))
  weightv[!is.finite(weightv)] <- 0
  # Calculate the out-of-sample portfolio returns
  retsos <- retsp[(endp[it]+1):endp[it+1], ] %*% weightv
  retsos
})  # end lapply
pnlsn <- rutils::do_call(c, pnlsn)

all.equal(pnlsn, foo)

tail(foo)
tail(pnlsn)
