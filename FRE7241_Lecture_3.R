price_s <- na.omit(rutils::etf_env$price_s$VTI)
# Define look-back window and a half window
win_dow <- 11
# Calculate time series of medians
medi_an <- TTR::runMedian(price_s, n=win_dow)
# Calculate time series of z-scores
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)

x11(width=6, height=5)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Plot histogram of z-scores
histo_gram <- hist(z_scores, col="lightgrey",
  xlab="z-scores", breaks=50, xlim=c(-4, 4),
  ylab="frequency", freq=FALSE, main="Z-scores histogram")
lines(density(z_scores, adjust=1.5), lwd=3, col="blue")

# Calculate one-sided Hampel z-scores
medi_an <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)
# Calculate two-sided Hampel z-scores
half_window <- win_dow %/% 2
medi_an <- rutils::lag_it(medi_an, lagg=-half_window)
ma_d <- rutils::lag_it(ma_d, lagg=-half_window)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
tail(z_scores, win_dow)
range(z_scores)

# Define discrimination threshold value
thresh_old <- 3
# Calculate number of prices classified as bad data
is_bad <- (abs(z_scores) > thresh_old)
sum(is_bad)
# Add 200 random price jumps into price_s
set.seed(1121)
n_bad <- 200
is_jump <- logical(NROW(price_s))
is_jump[sample(x=NROW(is_jump), size=n_bad)] <- TRUE
price_s[is_jump] <- price_s[is_jump]*
  sample(c(0.95, 1.05), size=n_bad, replace=TRUE)
# Plot prices and medians
dygraphs::dygraph(cbind(price_s, medi_an), main="VTI median") %>%
  dyOptions(colors=c("black", "red"))
# Calculate time series of z-scores
medi_an <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_scores <- (price_s - medi_an)/ma_d
z_scores[1:win_dow, ] <- 0
# Calculate number of prices classified as bad data
is_bad <- (abs(z_scores) > thresh_old)
sum(is_bad)

# Calculate confusion matrix
table(actual=!is_jump, forecast=!is_bad)
sum(is_bad)
# FALSE positive (type I error)
sum(!is_jump & is_bad)
# FALSE negative (type II error)
sum(is_jump & !is_bad)

# Confusion matrix as function of thresh_old
con_fuse <- function(actu_al, z_scores, thresh_old) {
    confu_sion <- table(!actu_al, !(abs(z_scores) > thresh_old))
    confu_sion <- confu_sion / rowSums(confu_sion)
    c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
  }  # end con_fuse
con_fuse(is_jump, z_scores, thresh_old=thresh_old)
# Define vector of thresholds
threshold_s <- seq(from=0.2, to=5.0, by=0.2)
# Calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
  actu_al=is_jump, z_scores=z_scores)  # end sapply
error_rates <- t(error_rates)
rownames(error_rates) <- threshold_s
error_rates <- rbind(c(1, 0), error_rates)
error_rates <- rbind(error_rates, c(0, 1))
# Calculate area under ROC curve (AUC)
true_pos <- (1 - error_rates[, "typeII"])
true_pos <- (true_pos + rutils::lag_it(true_pos))/2
false_pos <- rutils::diff_it(error_rates[, "typeI"])
abs(sum(true_pos*false_pos))

# Plot ROC curve for Hampel classifier
plot(x=error_rates[, "typeI"],
     y=1-error_rates[, "typeII"],
     xlab="FALSE positive rate",
     ylab="TRUE positive rate",
     xlim=c(0, 1), ylim=c(0, 1),
     main="ROC Curve for Hampel Classifier",
     type="l", lwd=3, col="blue")
abline(a=0.0, b=1.0, lwd=3, col="orange")

NA

App setup code that runs only once at startup.
n_data <- 1e4
std_dev <- 1.0

Define the user interface
inter_face <- shiny::fluidPage(
  # Create numeric input for the number of data points.
  numericInput('n_data', "Number of data points:", value=n_data),
  # Create slider input for the standard deviation parameter.
  sliderInput("std_dev", label="Standard deviation:",
        min=0.1, max=3.0, value=std_dev, step=0.1),
  # Render plot in a panel.
  plotOutput("plo_t", height=300, width=500)
)  # end user interface

Define the server function
ser_ver <- function(input, output) {
  output$plo_t <- shiny::renderPlot({
    # Simulate the data
    da_ta <- rnorm(input$n_data, sd=input$std_dev)
    # Plot the data
    par(mar=c(2, 4, 4, 0), oma=c(0, 0, 0, 0))
    hist(da_ta, xlim=c(-4, 4), main="Histogram of Random Data")
  })  # end renderPlot
}  # end ser_ver

# Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)

Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("VWAP Moving Average"),
  # Create single row of widgets with two slider inputs
  fluidRow(
    # Input stock symbol
    column(width=3, selectInput("sym_bol", label="Symbol",
                          choices=sym_bols, selected=sym_bol)),
    # Input look-back interval
    column(width=3, sliderInput("look_back", label="Lookback interval",
                          min=1, max=150, value=11, step=1))
  ),  # end fluidRow
  # Create output plot panel
  mainPanel(dygraphs::dygraphOutput("dy_graph"), width=12)
)  # end fluidPage interface

Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {
  # Get the close and volume data in a reactive environment
  clos_e <- shiny::reactive({
    # Get the data
    oh_lc <- get(input$sym_bol, data_env)
    clos_e <- log(quantmod::Cl(oh_lc))
    vol_ume <- quantmod::Vo(oh_lc)
    # Return the data
    cbind(clos_e, vol_ume)
  })  # end reactive code

  # Calculate the VWAP indicator in a reactive environment
  v_wap <- shiny::reactive({
    # Get model parameters from input argument
    look_back <- input$look_back
    # Calculate the VWAP indicator
    clos_e <- clos_e()[, 1]
    vol_ume <- clos_e()[, 2]
    v_wap <- HighFreq::roll_sum(se_ries=clos_e*vol_ume, look_back=look_back)
    volume_rolling <- HighFreq::roll_sum(se_ries=vol_ume, look_back=look_back)
    v_wap <- v_wap/volume_rolling
    v_wap[is.na(v_wap)] <- 0
    # Return the plot data
    da_ta <- cbind(clos_e, v_wap)
    colnames(da_ta) <- c(input$sym_bol, "VWAP")
    da_ta
  })  # end reactive code

  # Return the dygraph plot to output argument
  output$dy_graph <- dygraphs::renderDygraph({
    col_names <- colnames(v_wap())
    dygraphs::dygraph(v_wap(), main=paste(col_names[1], "VWAP")) %>%
dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")
  })  # end output plot
})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)

Define the server function
ser_ver <- shiny::shinyServer(function(input, output) {

  # Create an empty list of reactive values.
  value_s <- reactiveValues()

  # Get input parameters from the user interface.
  n_rows <- reactive({
    # Add n_rows to list of reactive values.
    value_s$n_rows <- input$n_rows
    input$n_rows
  })  # end reactive code

  # Broadcast a message to the console when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    cat("Input button pressed\n")
  })  # end observeEvent

  # Send the data when the button is pressed.
  da_ta <- eventReactive(eventExpr=input$but_ton, valueExpr={
    # eventReactive() executes on input$but_ton, but not on n_rows() or input$n_rows.
    cat("Sending", n_rows(), "rows of data\n")
    da_ta <- head(mtcars, input$n_rows)
    value_s$mpg <- mean(da_ta$mpg)
    da_ta
  })  # end eventReactive
  #   da_ta

  # Draw table of the data when the button is pressed.
  observeEvent(eventExpr=input$but_ton, handlerExpr={
    da_ta <- da_ta()
    cat("Received", value_s$n_rows, "rows of data\n")
    cat("Average mpg = ", value_s$mpg, "\n")
    cat("Drawing table\n")
    output$tabl_e <- renderTable(da_ta)
  })  # end observeEvent

})  # end server code

Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
