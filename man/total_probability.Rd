% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/total_probability.R
\name{total_probability}
\alias{total_probability}
\title{Caculate nomogram total points}
\usage{
total_probability(totalpoints, digits = 6)
}
\arguments{
\item{totalpoints}{totalpoints after function nomoFormu_total_points}

\item{digits}{default is 6}
}
\value{
dataframe
}
\description{
caculate total probability
}
\examples{
\donttest{
library(rms)
set.seed(2018)
n <-2019
age <- rnorm(n,60,20)
sex <- factor(sample(c('female','male'),n,TRUE))
sex <- as.numeric(sex)
weight <- sample(50:100,n,replace = TRUE)
time <- sample(50:800,n,replace = TRUE)
units(time)="day"
death <- sample(c(1,0,0),n,replace = TRUE)
df <- data.frame(time,death,age,sex,weight)
ddist <- datadist(df)
options(datadist='ddist')
f <- cph(formula(Surv(time,death)~sex+age+weight),data=df,
         linear.predictors=TRUE,
         x=TRUE,y=TRUE,surv=TRUE,time.inc=3)
surv <- Survival(f)
nomo <- nomogram(f,
                 lp=TRUE,
                 fun=list(function(x) surv(365,x),
                          function(x) surv(365*2,x)),
                 funlabel=c("1-Year Survival Prob",
                            "2-Year Survival Prob"))
library(nomogramFormula)
formula_lp(nomo)
totalpoints <- total_points(lp=f$linear.predictors)
formula_probability(nomo) 
total_probability(totalpoints = totalpoints)
}
}
