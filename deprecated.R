# Define predictor as a rolling mean
nagg <- 5
predm <- HighFreq::roll_mean(matrix(retp), nagg)
# Define predictor matrix for forecasting
predm <- sapply(1+nagg*(0:dimax), rutils::lagit, input=predm)
predm <- cbind(rep(1, nrows), predm)
# Calculate the forecasts as function of the AR order
fcasts <- lapply(2:NCOL(predm), function(ordern) {
  predinv <- MASS::ginv(predm[insample, 1:ordern])
  coeff <- drop(predinv %*% respv[insample])
  drop(predm[outsample, 1:ordern] %*% coeff)
})  # end lapply
names(fcasts) <- paste0("n=", 2:NCOL(predm))
# Calculate the out-of-sample PnLs
pnls <- sapply(fcasts, function(x) {
  cumsum(sign(x)*retp[outsample])
})  # end sapply
colnames(pnls) <- names(fcasts)
pnls <- xts::xts(pnls, datev[outsample])
# Plot dygraph of out-of-sample PnLs
colorv <- colorRampPalette(c("red", "blue"))(NCOL(pnls))
dygraphs::dygraph(pnls[endd],
  main="Autoregressive Strategies Using Rolling Average Predictor") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=300)
# Calculate the PnLs using the average of past forecasts
nagg <- 5
pnls <- sapply(fcasts, function(x) {
  x <- HighFreq::roll_mean(matrix(x), nagg)
  cumsum(sign(x)*retp[outsample])
})  # end sapply
colnames(pnls) <- names(fcasts)
pnls <- xts::xts(pnls, datev[outsample])
# Plot dygraph of out-of-sample PnLs
dygraphs::dygraph(pnls[endd],
  main="Autoregressive Strategies Using Rolling Average Forecasts") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(width=300)
