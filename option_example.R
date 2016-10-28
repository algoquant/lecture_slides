# ============================
# Implicit Volatility using BS closed formula
# Example: UBS options expired on 18th January 2014
# GFE, 17/11/2013
# http://gfoix.com/wp-content/uploads/2013/11/20131119_ImplicitVolatility_R.txt
# ============================


# setwd("C:/Develop/R/FRE6811")

# === Step 1 ===
# Read and summarize data
UBS_Op18Nov <-(data.frame(read.table("C:/Develop/R/FRE6811/option_data.csv",header=TRUE, sep=",")))
summary(UBS_Op18Nov)
attach(UBS_Op18Nov)
n <- dim(UBS_Op18Nov)[1]


S <- 18.53  # UBS stock 18th Nov13
mat <- 62/365  # From 18th Nov13 until 18th Jan14
r <- 0.008  # T-bill 3 month 15th Nov13


# === Step 2 ===
# Write required functions

# 1) Black-Scholes closed formula
BS <- function(S, K, mat, r, vol, Type)
{
  d1 <- (log(S/K) + (r + vol^2/2)*mat) / (vol*sqrt(mat))
  d2 <- d1 - vol*sqrt(mat)
  if(Type=="Call"){
    Pr <- S*pnorm(d1) - K*exp(-r*mat)*pnorm(d2)
  }
  if(Type=="Put"){
    Pr <- K*exp(-r*mat)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(Pr)
}


# 2) We can use Bisection Method to calculate the implicit volatility assuming BS closed formula (1).
ImpliVol_BS <- function(S, K, mat, r, Pr, Type)
{
  vol <- 0.20
  vol.up <- 1
  vol.down <- 0.001
  count <- 0
  error <- BS(S, K, mat, r, vol, Type) - Pr  # Difference between real prices and BS prices.
  
  repeat until error is sufficiently small or counter hits 1000
  while(abs(error) > 0.00001 && count<1000){
    if(error < 0){
      vol.down <- vol
      vol <- (vol.up + vol)/2
    }else{
      vol.up <- vol
      vol <- (vol.down + vol)/2
    }
    error <- BS(S, K, mat, r, vol, Type) - Pr
    count <- count + 1
  }
  
  return NA if counter hit 1000
  if(count==1000){
    return(NA)
  }else{
    return(vol)
  }
}


# === Step 3 ===
# Implicit Volatility using functions written above

ImplVol_Call <- rep(0,n)
ImplVol_Put <- rep(0,n)

for(i in 1:n)
{
  ImplVol_Call[i] <- ImpliVol_BS(S, Strike[i], mat, r, Price[i], "Call")
  ImplVol_Put[i] <- ImpliVol_BS(S, Strike[i], mat, r, Price[i], "Put")
}


# === Step 4 ===
# Plot Outcomes

plot(Strike[Type=="Call"], ImplVol_Call[Type=="Call"], typ="l", col="black", xlim=c(8, 30), ylim= c(0.01, 0.9),
     main=c("UBS 18th Jan14 Call", "Volatility Smile"), xlab="Strike", ylab="Implicit Vol")
lines(Strike[Type=="Put"], ImplVol_Put[Type=="Put"], col="blue")
abline(v=S, col="red", lty=2)
legend("bottomleft", cex=0.9, c("S = 18.53", "Impl.Vol. Put", "Impl.Vol. Call"), lty=1, col=c("red","blue", "black"))
