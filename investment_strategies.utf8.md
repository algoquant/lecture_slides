% Define knitr options
% !Rnw weave=knitr
% Set global chunk options
<<knitr_setup,include=FALSE,cache=FALSE>>=
library(knitr)
opts_chunk$set(prompt=TRUE, eval=FALSE, tidy=FALSE, strip.white=FALSE, comment=NA, highlight=FALSE, message=FALSE, warning=FALSE, size='scriptsize', fig.width=4, fig.height=4)
options(width=60, dev='pdf')
options(digits=3)
thm <- knit_theme$get("acid")
knit_theme$set(thm)
@


% Define document options
\documentclass[10pt]{beamer}
\mode<presentation>
\usetheme{AnnArbor}
% \usecolortheme{whale}
% Uncover everything in a step-wise fashion
% \beamerdefaultoverlayspecification{<+->}
% mathtools package for math symbols
\usepackage{mathtools}
\usepackage[latin1]{inputenc}
\usepackage{hyperref}
\usepackage{fancybox}
\usepackage{url}
\usepackage[backend=bibtex,style=alphabetic]{biblatex} % bibstyle=numeric
% \bibliographystyle{amsalpha} % doesn't work
\addbibresource{FRE_lectures.bib}
% \addbibresource[location=remote]{http://www.citeulike.org/user/jerzyp}
\renewcommand\bibfont{\footnotesize}
\renewcommand{\pgfuseimage}[1]{\scalebox{0.75}{\includegraphics{#1}}} % scale bib icons
\setbeamertemplate{bibliography item}[text] % set bib icons
% \setbeamertemplate{bibliography item}{} % remove bib icons

% \usepackage{enumerate}
% \let\emph\textbf
% \let\alert\textbf
% Define colors for hyperlinks
\definecolor{links}{HTML}{2A1B81}
\hypersetup{colorlinks=true,linkcolor=,urlcolor=links}
% Make url text scriptsize
\renewcommand\UrlFont{\scriptsize}
% Make institute text italic and small
\setbeamerfont{institute}{size=\small,shape=\itshape,bg=red,fg=red}
\setbeamerfont{date}{size=\small}
\setbeamerfont{block title}{size=\normalsize} % shape=\itshape
\setbeamerfont{block body}{size=\footnotesize}



% Title page setup
\title[Investment Strategies]{Investment Strategies}
\subtitle{FRE6871 \& FRE7241, Fall 2016}
\institute[NYU Tandon]{NYU Tandon School of Engineering}
\titlegraphic{\includegraphics[scale=0.2]{image/tandon_long_color}}
\author[Jerzy Pawlowski]{Jerzy Pawlowski \emph{\href{mailto:jp3900@nyu.edu}{jp3900@nyu.edu}}}
% \email{jp3900@nyu.edu}
\date{\today}



%%%%%%%%%%%%%%%
\begin{document}


%%%%%%%%%%%%%%%
\maketitle



%%%%%%%%%%%%%%%
\section{Investor Risk Preferences and Portfolio Selection}


%%%%%%%%%%%%%%%
\subsection{Single Period Binary Gamble}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      Imagine a binary betting game (gamble) with the probability of winning equal to \texttt{p}, the payout odds in case of win equal to \texttt{b}, and the loss equal to \texttt{-a},
      \vskip1ex
      The investor makes no up-front payments, and can either win an amount \texttt{b}, or lose an amount \texttt{-a},
      \vskip1ex
      Assuming an investor makes decisions exclusively on the basis of the expected value (level) of future wealth, then she would choose to bet all her wealth on the gamble if its expected value is positive, and choose not to bet at all if its expected value is negative,
    \column{0.5\textwidth}
      <<results='asis',echo=FALSE>>=
library(xtable)
binbet_table <- data.frame(win=c("p", "b"), lose=c("q = 1 - p", "-a"))
rownames(binbet_table) <- c("probability", "payout")
# print(xtable(binbet_table), comment=FALSE, size="tiny")
print(xtable(binbet_table), comment=FALSE)
      @
      The expected value of the gamble is equal to: $ev=p \cdot b - q \cdot a$,
      \vskip1ex
      The variance of the gamble is equal to: $var=p \cdot q \cdot (a+b)^2$,
      \vskip1ex
      Without loss of generality we can assume $p=q=\frac{1}{2}$,\\
      $ev=0.5 \cdot (b-a)$,\\
      $var=0.25 \cdot (a+b)^2$,
      \vskip1ex
      The \emph{Sharpe} ratio of the gamble is then equal to:
      \begin{displaymath}
        S_{r}=\frac{ev}{sqrt(var)}=\frac{(b-a)}{(a+b)}
      \end{displaymath}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Investor Utility and Fractional Betting}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      The \emph{expected utility} hypothesis states that investor risk preferences are based on the expected value of the \emph{utility} of their wealth, instead of on the level of wealth,
      \vskip1ex
      In 1738 Daniel Bernoulli introduced the concept of \emph{logarithmic utility} in his work "\emph{Specimen Theoriae Novae de Mensura Sortis}" (New Theory of the Measurement of Risk),
      \vskip1ex
      \emph{Logarithmic utility} is defined as the logarithm of wealth, 
      \vskip1ex
      The expected value of \emph{logarithmic utility} for the binary gamble is equal to: $g(f)=p \cdot log(1+fb) + q \cdot log(1-fa)$,
      \vskip1ex
      Under \emph{logarithmic utility} investor preferences are based on the percentage change of their wealth, instead of the absolute change of their wealth,
      \vskip1ex
      An investor under \emph{logarithmic utility} doesn't bet either all her wealth or nothing at all, but instead bets only a certain fraction \texttt{f} of wealth, depending on the risk/return of the gamble,
    \column{0.5\textwidth}
      \vspace{-1em}
      <<util_plot,eval=FALSE,echo=(-1),fig.show='hide'>>=
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# define utility
utility <- function(frac, p=0.5, a=1, b=4) {
  p*log(1+frac*b) + (1-p)*log(1-frac*a)
}  # end utility
# plot utility
curve(expr=utility, xlim=c(0, 1), 
      ylim=c(-0.5, 0.3), xlab="betting fraction", 
      ylab="utility", main="", lwd=2)
title(main="logarithmic utility", line=-0.8)
      @
      \vspace{-2.5em}
    \includegraphics[width=0.5\paperwidth,valign=t]{figure/util_plot-1}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Optimal Fractional Betting and the Kelly Criterion}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      Given the odds and probability of winning, there's an optimal fraction \texttt{f} of wealth that the investor should risk on each bet, 
      \vskip1ex
      The betting fraction that maximizes the \emph{utility} can be found by equating the derivative of \emph{utility} to zero:
      \begin{displaymath}
        \frac{\mathrm{d}g(f)}{\mathrm{d}f}=\frac{p \cdot b}{1+fb} - \frac{q \cdot a}{1-fa} = 0
      \end{displaymath}
      \begin{displaymath}
        f=\frac{p}{a}-\frac{q}{b}=\frac{p \cdot b-q \cdot a}{a \cdot b}
      \end{displaymath}
      The optimal \texttt{f} is called the \emph{Kelly} fraction, and it depends on the parameters of the gamble,
      \vskip1ex
      The \emph{Kelly} fraction can be greater than \texttt{1} (leverage), or be negative (shorting),
      \vskip1ex
      If we assume $a=1$, then: $f=\frac{p(b+1)-1}{b}$
      \vskip1ex
      The \emph{Kelly} fraction is equal to the expected net winnings divided by the odds,
    \column{0.5\textwidth}
      \vspace{-1.5em}
      <<kelly_plot,eval=FALSE,echo=(-1),fig.show='hide'>>=
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# define and plot Kelly fraction
kelly <- function(b, p=0.5, a=1) {
  p/a - (1-p)/b
}  # end kelly
curve(expr=kelly, xlim=c(0, 5), 
      ylim=c(-2, 1), xlab="betting odds", 
      ylab="kelly", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="max Kelly fraction=0.5")
title(main="Kelly fraction", line=-0.8)
      @
      \vspace{-2.5em}
    \includegraphics[width=0.5\paperwidth,valign=t]{figure/kelly_plot-1}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Kelly Fractional Betting}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      Investors under \emph{logarithmic utility} are sensitive to the risk of ruin (losing all their wealth),
      \vskip1ex
      The loss amount \texttt{"a"} determines the risk of ruin, with larger values of \texttt{"a"} increasing the risk of ruin,
      \vskip1ex
      Therefore investors will choose a smaller betting fraction \texttt{f} for larger values of \texttt{"a"},
      \vskip1ex
      This means that even for huge odds in their favor, investors may choose not to bet all their wealth, because of the risk of ruin,
      \vskip1ex
      For example, if the betting odds are very large $b \to \infty$, then the \emph{Kelly} fraction: $f=\frac{p}{a}$,
      \vspace{-1em}
      <<kelly_max_plot,eval=FALSE,echo=(-1),fig.show='hide'>>=
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# plot several Kelly fractions
curve(expr=kelly, xlim=c(0, 5), 
      ylim=c(-1, 1.5), xlab="betting odds", 
      ylab="kelly", main="", lwd=2)
abline(h=0.5, lwd=2, col="red")
text(x=1.5, y=0.5, pos=3, cex=0.8, labels="a=1.0; max fraction=0.5")
kelly2 <- function(b) {kelly(b=b, a=0.5)}
curve(expr=kelly2, add=TRUE, main="", lwd=2)
abline(h=1.0, lwd=2, col="red")
text(x=1.5, y=1.0, pos=3, cex=0.8, labels="a=0.5; max fraction=1.0")
title(main="Kelly fraction", line=-0.8)
      @
    \column{0.5\textwidth}
    \vspace{-1em}
    \includegraphics[width=0.5\paperwidth,valign=t]{figure/kelly_max_plot-1}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Multiperiod Betting}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      Multiperiod betting consists of \texttt{n} rounds of betting, with random returns equal to $r_{i}$, 
      \vskip1ex
      Then after \texttt{n} rounds the wealth is equal to: $w=(1+r_{1}) (1+r_{2}) \ldots (1+r_{n})$,
      \vskip1ex
      The expected value of wealth after \texttt{n} rounds is equal to: $E[w]=(1+E[r_{i}])^n$ (since the $r_{i}$ are i.i.d. - independent and identically distributed),
      \vskip1ex
      If only a fraction \texttt{f} of wealth is bet each time, then the wealth is equal to: $w=(1+f \cdot r_{1}) (1+f \cdot r_{2}) \ldots (1+f \cdot r_{n})$, \\
      and the expected wealth is equal to: $E[w]=(1+f \cdot E[r_{i}])^n$,
      \vskip1ex
      The utility is equal to the logarithm of wealth: $g(w)=log(w)=\sum_{i=1}^{n} log(1+f \cdot r_{i})$, \\
      and the expected utility is equal to: $E[g(w)]=n \cdot E[log(1+f \cdot r_{i})]$,
      \vskip1ex
      The expected utility for multiperiod betting is \texttt{n} times the expected utility for single period betting,
      \vskip1ex
      The \emph{Kelly} fraction for multiperiod betting is the same as for single period betting,
    \column{0.5\textwidth}
      \vspace{-1em}
        <<multi_betting,eval=FALSE,echo=(-(1:2)),fig.show='hide'>>=
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
set.seed(1121)  # reset random number generator
# simulated wealth path
wealth_path <- cumprod(1+runif(1000, 
                      min=-0.1, max=0.1))
plot(wealth_path, type="l", 
     lty="solid", xlab="", ylab="")
title(main="wealth path", line=-1)
      @
      \vspace{-1em}
      \includegraphics[width=0.5\paperwidth,valign=t]{figure/multi_betting-1}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Binomial Multiperiod Betting}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      The \emph{Kelly} fraction for multiperiod betting can be found by maximizing the expected \emph{utility} of the final wealth distribution:
      \begin{multline*}
        g(f)=\sum_{k=0}^{n} \binom{n}{k} p^k q^{n-k} log((1+fb)^k (1-fa)^{n-k})\\
        =log(1+fb) \sum_{k=0}^{n} {\binom{n}{k} p^k q^{n-k} k} + \\
        log(1-fa) \sum_{k=0}^{n} {\binom{n}{k} p^k q^{n-k} (n-k)} \\
        =n \cdot p \cdot log(1+fb) + n \cdot q \cdot log(1-fa)
      \end{multline*}
      The above is just the single period \emph{utility} multiplied by the number of rounds of betting \texttt{n},
      \vskip1ex
      The \emph{Kelly} fraction \texttt{f} for multiperiod betting is the same as for single period betting:
      \begin{displaymath}
        f=\frac{p}{a}-\frac{q}{b}
      \end{displaymath}
    \column{0.5\textwidth}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Optimal Multiperiod Betting}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      In multiperiod betting the investor participates in \texttt{n} rounds of betting,
      \vskip1ex
      Each round of betting multiplies the wealth by either $(1+fb)$ or $(1-fa)$, 
      \vskip1ex
      If there were \texttt{i} wins and \texttt{j} losses, then the final wealth is equal to: 
      \begin{displaymath}
        w=(1+fb)^i \cdot (1-fa)^j
      \end{displaymath}
      The betting fraction that maximizes the final wealth is equal to:
      \begin{displaymath}
        f=\frac{pfreq}{a}-\frac{qfreq}{b}
      \end{displaymath}
      where $pfreq=\frac{i}{n}$ and $qfreq=1-pfreq$,
      \vskip1ex
      The most likely value of \texttt{pfreq} is equal to the probability of winning \texttt{p}, 
      \vskip1ex
      So the \emph{Kelly} fraction is the betting fraction that maximizes the final wealth in multiperiod betting, provided the fraction of wins is equal to the probability of winning \texttt{p},
    \column{0.5\textwidth}
      \vspace{-1em}
      <<kelly_multi,eval=FALSE,echo=(-1),fig.show='hide'>>=
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
# wealth of multiperiod binary betting
wealth <- function(f, b=2, a=1, n=100, i=51) {
  (1+f*b)^i * (1-f*a)^(n-i)
}  # end wealth
curve(expr=wealth, xlim=c(0, 1), 
      xlab="betting fraction", 
      ylab="wealth", main="", lwd=2)
title(main="wealth of multiperiod betting", line=0.1)
      @
    \vspace{-2em}
    \includegraphics[width=0.5\paperwidth,valign=t]{figure/kelly_multi-1}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Utility Function of Asset Returns}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      Let the returns over a short time period be equal to \texttt{r}, with probability distribution \texttt{p(r)},
      \vskip1ex
      The mean return $\bar{r}$, and variance ${\sigma}^2$ are:
      \begin{displaymath}
        \bar{r} = \int {r \, p(r) \, \mathrm{d}r} \; ; \quad
        {\sigma}^2 = \int {(r - \bar{r})^2 \, p(r) \, \mathrm{d}r}
      \end{displaymath}
      Since the returns are over a short time period, we have: $r \ll 1$ and $\bar{r} \ll \sigma$, so that we can replace $r - \bar{r}$ with \texttt{r} as follows:
      \begin{displaymath}
        \int {(r - \bar{r})^2 \, p(r) \, \mathrm{d}r} \approx \int {r^2 \, p(r) \, \mathrm{d}r}
      \end{displaymath}
      The logarithmic utility $g(w)$ is given by:
      \begin{multline*}
        g(w) = \int {log(1+f \cdot r) \, p(r) \, \mathrm{d}r} \\
        \hspace*{-1em}
        = \int {(f \cdot r - \frac{(f \cdot r)^2}{2} + 
        \frac{(f \cdot r)^3}{3} - \frac{(f \cdot r)^4}{4}) \, p(r) \, \mathrm{d}r}\\
        \hspace*{-2em}
        = f\bar{r} - \frac{f^2 {\sigma}^2}{2} + \frac{f^3 {\sigma}^3 \hat{s}}{3} - \frac{f^4 {\sigma}^4 \hat{k}}{4}
      \end{multline*}
      Where $\hat{s}$ is the skewness, and $\hat{k}$ is the kurtosis,
    \column{0.5\textwidth}
      \vspace{-2em}
      <<utility_returns,eval=TRUE,echo=(-1),fig.show='hide'>>=
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(PerformanceAnalytics)
ts_rets <- env_etf$re_turns[, "VTI"]
c(mean(ts_rets), sd(ts_rets))
utility <- function(frac, r=ts_rets) {
sapply(frac, function (fract) sum(log(1+fract*r)))
}  # end utility
curve(expr=utility, 
      xlim=c(0.1, 2*KellyRatio(R=ts_rets, method="full")), 
      xlab="kelly", ylab="utility", main="", lwd=2)
title(main="utility", line=-2)
      @
    \vspace{-3em}
    \includegraphics[width=0.5\paperwidth,valign=t]{figure/utility_returns-1}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Kelly Criterion for Asset Returns}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      The \emph{Kelly} fraction can be found by equating the derivative of \emph{utility} to zero:
      \begin{displaymath}
        \frac{\mathrm{d}g(f)}{\mathrm{d}f} = \bar{r} - f{\sigma}^2 + f^2 {\sigma}^3 \hat{s} - f^3 {\sigma}^4 \hat{k} = 0
      \end{displaymath}
      Assuming $\hat{s}$ and $\hat{k}$ are small and can be neglected, we get:
      \begin{displaymath}
        f = \frac{\bar{r}}{{\sigma}^2} = \frac{S_{r}}{\sigma} \; ; \quad g(w) = \frac{1}{2} \frac{{\bar{r}}^2}{{\sigma}^2} = \frac{1}{2} S_{r}^2
      \end{displaymath}
      Where $S_{r}$ is the \emph{Sharpe} ratio, 
      \vskip1ex
      The \emph{Kelly} fraction is equal to the \emph{Sharpe} ratio divided by the \emph{standard deviation},
      \vskip1ex
      The optimal utility $g(w)$ is equal to half the \emph{Sharpe} ratio squared,
      \vskip1ex
      The \emph{standard deviation} and \emph{Sharpe} ratio are calculated over the same time interval as the returns (not annualized),
    \column{0.5\textwidth}
    \vspace{-1em}
      <<eval=TRUE,echo=(-(1:3)),fig.show='hide'>>=
par(mar=c(5, 2, 2, 2), mgp=c(1.5, 0.5, 0), cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.5)
library(PerformanceAnalytics)
ts_rets <- env_etf$re_turns[, "VTI"]
KellyRatio(R=ts_rets, method="full")
      @
%    \vspace{-1em}
%    \includegraphics[width=0.5\paperwidth,valign=t]{figure/kelly_returns-1}
  \end{columns}
\end{block}

\end{frame}



%%%%%%%%%%%%%%%
\section{Active Investment Strategies}


%%%%%%%%%%%%%%%
\subsection{Moving Average Technical Indicators}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      The Volume-Weighted Average Price (\emph{VWAP}) is defined as the sum of prices multiplied by trading volumes, divided by the sum of volumes, 
      \vskip1ex
      Moving averages (such as \emph{VWAP}) are often used to define technical indicators (trading signals), 
      <<echo=(-(1:1)),eval=FALSE>>=
library(HighFreq)  # load package HighFreq
# calculate open, close, and lagged prices
open_prices <- Op(env_etf$VTI)
price_s <- Cl(env_etf$VTI)
prices_lag <- rutils::lag_xts(price_s)
# define lookback window and calculate VWAP
win_dow <- 150
VTI_vwap <- HighFreq::roll_vwap(env_etf$VTI, 
            win_dow=win_dow)
# calculate VWAP indicator
vwap_indic <- sign(price_s - VTI_vwap)
# determine dates right after VWAP has crossed prices
vwap_crosses <- 
  (rutils::diff_xts(vwap_indic) != 0)
trade_dates <- which(vwap_crosses) + 1
      @
    \column{0.5\textwidth}
      \vspace{-1em}
      \includegraphics[width=0.5\paperwidth,valign=t]{figure/vwap_indic.png}
      \vspace{-1em}
      <<echo=TRUE,eval=FALSE>>=
# plot prices and VWAP
chart_Series(x=price_s, 
  name="VTI prices", col='orange')
add_TA(VTI_vwap, on=1, lwd=2, col='blue')
legend("top", legend=c("VTI", "VWAP"), 
bg="white", lty=c(1, 1), lwd=c(2, 2), 
col=c('orange', 'blue'), bty="n")
      @
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Moving Average Crossover Strategy}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      In a trend-following \emph{Moving Average Crossover} strategy, when the current price crosses above the \emph{VWAP}, then the strategy switches its position to long risk, and vice versa, 
      \vskip1ex
      The strategy trades at the \texttt{Open} price in the next period after prices cross the \emph{VWAP}, to reflect that in practice it's impossible to trade immediately, 
      \vspace{-1em}
      <<echo=(-(1:1)),eval=FALSE>>=
library(HighFreq)  # load package HighFreq
# calculate positions, either: -1, 0, or 1
pos_ition <- NA*numeric(NROW(env_etf$VTI))
pos_ition[1] <- 0
pos_ition[trade_dates] <- vwap_indic[trade_dates]
pos_ition <- na.locf(pos_ition)
pos_ition <- xts(pos_ition, order.by=index((env_etf$VTI)))
position_lagged <- rutils::lag_xts(pos_ition)
# calculate periodic profits and losses
pn_l <- position_lagged*(price_s - prices_lag)
pn_l[trade_dates] <- position_lagged[trade_dates] * 
  (open_prices[trade_dates] - prices_lag[trade_dates]) +
  pos_ition[trade_dates] * 
  (price_s[trade_dates] - open_prices[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(pn_l)/sd(pn_l)/NROW(pn_l)
      @
    \column{0.5\textwidth}
      \vspace{-1em}
      \includegraphics[width=0.5\paperwidth,valign=t]{figure/vwap_strat.png}
      \vspace{-2em}
      <<echo=TRUE,eval=FALSE>>=
# plot prices and VWAP
pn_l <- xts(cumsum(pn_l), order.by=index((env_etf$VTI)))
chart_Series(x=(price_s-as.numeric(price_s[1, ])), name="VTI prices", col='orange')
add_TA(pn_l, on=1, lwd=2, col='blue')
add_TA(pos_ition > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(pos_ition < 0, on=-1,
       col="lightgrey", border="lightgrey")
legend("top", legend=c("VTI", "VWAP strategy"), 
bg="white", lty=c(1, 1), lwd=c(2, 2), 
col=c('orange', 'blue'), bty="n")
      @
  \end{columns}
\end{block}

\end{frame}



%%%%%%%%%%%%%%%
\section{Backtesting Active Investment Strategies}


%%%%%%%%%%%%%%%
\subsection{Backtesting Framework with Overlapping Aggregations}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      An interval aggregation can be specified by a vector of look-back \emph{windows} attached at \emph{endpoints} spanning fixed time \emph{intervals}, 
      \vskip1ex
      For example, we may wish to perform aggregations at weekly \emph{endpoints}, over overlapping 40-week look-back \emph{windows}, 
      \vskip1ex
      The variable \texttt{win\_dow} is equal to the number of end points in the look-back \emph{window}, while (\texttt{win\_dow-1}) is equal to the number of intervals in that window, 
      \vskip1ex
      The \emph{startpoints} are the \emph{endpoints} lagged by the number of window intervals (number of intervals in the window), 
    \column{0.5\textwidth}
      \vspace{-1em}
      <<echo=(-(1:1)),eval=FALSE>>=
# library(HighFreq)  # load package HighFreq
# define time interval for end points
re_balance <- "weeks"
# look-back window is multiple of re_balance
win_dow <- 41
# create index of rebalancing period end points
end_points <- xts::endpoints(env_etf$re_turns, 
                             on=re_balance)
# start_points are multi-period lag of end_points
len_gth <- length(end_points)
start_points <-  end_points[
  c(rep_len(1, win_dow-1), 
    1:(len_gth-win_dow+1))]
# create list of look-back intervals
inter_vals <- lapply(2:len_gth, 
    function(in_dex) {
      start_points[in_dex]:end_points[in_dex]
  })  # end lapply
      @
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Performing Overlapping Aggregations}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      An interval aggregation can be specified by a vector of look-back \emph{windows} attached at \emph{endpoints} spanning fixed time \emph{intervals}, 
      \vskip1ex
      For example, we may wish to perform aggregations at weekly \emph{endpoints}, over overlapping 40-week look-back \emph{windows}, 
      \vskip1ex
      The variable \texttt{win\_dow} is equal to the number of end points in the look-back \emph{window}, while (\texttt{win\_dow-1}) is equal to the number of intervals in that window, 
      \vskip1ex
      The \emph{startpoints} are the \emph{endpoints} lagged by the number of window intervals (number of intervals in the window), 
    \column{0.5\textwidth}
      \vspace{-1em}
      <<echo=(-(1:1)),eval=FALSE>>=
# library(HighFreq)  # load package HighFreq
# create list of symbols for model
sym_bols <- c("VTI", "IEF", "DBC")

# calculate risk&ret stats for some symbols, over a range of dates
# perform lapply() loop over inter_vals
risk_stats <- lapply(inter_vals, 
  function(inter_val) {
    x_ts <- 
      env_etf$re_turns[inter_val, sym_bols]
    t(sapply(x_ts, 
      function(col_umn)
        c(return=mean(col_umn), risk=mad(col_umn))
      ))  # end sapply
    })  # end lapply
# rbind list into single xts or matrix
# risk_stats <- rutils::do_call_rbind(risk_stats)
# head(risk_stats)
# calculate non-overlapping returns in interval
re_turns <-sapply(2:len_gth, 
    function(in_dex) {
    sapply(env_etf$re_turns[
      (end_points[in_dex-1]+1):end_points[in_dex], 
      sym_bols], sum)
  })  # end sapply
re_turns <- t(re_turns)
      @
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Performing Rolling Model Calibrations}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      An interval aggregation can be specified by a vector of look-back \emph{windows} attached at \emph{endpoints} spanning fixed time \emph{intervals}, 
      \vskip1ex
      The \emph{startpoints} are the \emph{endpoints} lagged by the number of window intervals (number of intervals in the window), 
      <<echo=TRUE,eval=FALSE>>=
# calculate list of portfolio weights
# perform lapply() loop over risk_stats
weight_s <- sapply(risk_stats, 
    function(risk_stat) {
      weight_s <- risk_stat[, 1]/risk_stat[, 2]
      weight_s <- weight_s - mean(weight_s)
      weight_s <- weight_s/sum(abs(weight_s))
    })  # end sapply
weight_s <- t(weight_s)
weights_xts <- xts(weight_s, 
  order.by=index(env_etf$re_turns[end_points]))
# plot weights
x11()
zoo::plot.zoo(weights_xts, xlab=NULL)
      @
    \column{0.5\textwidth}
    \vspace{-1em}
      \includegraphics[width=0.5\paperwidth,valign=t]{figure/portf_weights.png}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Performing Backtesting}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      An interval aggregation can be specified by a vector of look-back \emph{windows} attached at \emph{endpoints} spanning fixed time \emph{intervals}, 
      \vskip1ex
      For example, we may wish to perform aggregations at weekly \emph{endpoints}, over overlapping 40-week look-back \emph{windows}, 
      <<echo=TRUE,eval=FALSE>>=
# calculate pnls over all windows
pn_l <- rowSums(weight_s[-NROW(weight_s), ] * 
                  re_turns[-1, ])
# calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
cost_s <- bid_offer*abs(diff(weight_s))
cost_s <- rowSums(cost_s)
pn_l <- pn_l - cost_s
pn_l <- xts(cumsum(pn_l), 
  order.by=index(env_etf$re_turns[end_points[-(1:2)]]))
colnames(pn_l)[1] <- "pnl"
# plot strategy pnl
x11()
chart_Series(x=pn_l, name="Strategy PnL")
      @
    \column{0.5\textwidth}
    \vspace{-1em}
      \includegraphics[width=0.5\paperwidth,valign=t]{figure/backtest_pnl.png}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Strategy Market Beta}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      \vspace{-1em}
      <<echo=TRUE,eval=FALSE>>=
# calculate betas
beta_s <- c(1, env_etf$capm_stats[
  match(sym_bols[-1], 
        rownames(env_etf$capm_stats)), 
  "Beta"])
names(beta_s)[1] <- sym_bols[1]
# weights times betas
beta_s <- weight_s %*% beta_s
beta_s <- xts(beta_s, 
  order.by=index(
    env_etf$re_turns[end_points[-1]]))
colnames(beta_s) <- "portf_beta"
x11()
plot.zoo(cbind(beta_s, 
  env_etf$VTI[, 4])[index(beta_s)],
  main="betas & VTI", xlab="")
      @
    \column{0.5\textwidth}
    \vspace{-1em}
      \includegraphics[width=0.5\paperwidth,valign=t]{figure/backtest_betas.png}
  \end{columns}
\end{block}

\end{frame}


%%%%%%%%%%%%%%%
\subsection{Strategy Optimization}
\begin{frame}[fragile,t]{\subsecname}
\vspace{-1em}
\begin{block}{}
  \begin{columns}[T]
    \column{0.5\textwidth}
      \vspace{-1em}
      <<echo=TRUE,eval=FALSE>>=
# create trading function
run_strat <- function(win_dow) {
  start_points <-  end_points[
    c(rep_len(1, win_dow-1), 
      1:(len_gth-win_dow+1))]
  inter_vals <- lapply(2:len_gth, 
                       function(in_dex) {
                         start_points[in_dex]:end_points[in_dex]
                       })  # end lapply
  risk_stats <- lapply(inter_vals, 
                       function(inter_val) {
                         x_ts <- env_etf$re_turns[inter_val, sym_bols]
                         t(sapply(x_ts, 
                                  function(col_umn)
                                    c(return=mean(col_umn), risk=mad(col_umn))
                         ))  # end sapply
                       })  # end lapply
  weight_s <- sapply(risk_stats, 
                     function(risk_stat) {
                       weight_s <- risk_stat[, 1]/risk_stat[, 2]
                       weight_s <- weight_s - mean(weight_s)
                       weight_s <- weight_s/sum(abs(weight_s))
                     })  # end sapply
  weight_s <- t(weight_s)
  pn_l <- rowSums(weight_s[-NROW(weight_s), ] * re_turns[-1, ])
  cost_s <- bid_offer*abs(diff(weight_s))
  cost_s <- rowSums(cost_s)
  pn_l <- pn_l - cost_s
  sum(pn_l)
}  # end run_strat
      @
    \column{0.5\textwidth}
    \vspace{-1em}
      \includegraphics[width=0.5\paperwidth,valign=t]{figure/strat_profile}
    \vspace{-1em}
      <<echo=TRUE,eval=FALSE>>=
window_s <- 8*(1:7)
strat_profile <- sapply(window_s, run_strat)
plot(cbind(window_s, strat_profile), t="l", 
     main="Strategy PnL as function of window size", 
     xlab="window", ylab="pnl")
      @
  \end{columns}
\end{block}

\end{frame}


\end{document}
