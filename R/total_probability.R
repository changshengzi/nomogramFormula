#' @title Caculate nomogram total points
#' @description caculate total probability
#'
#' @param totalpoints totalpoints after function nomoFormu_total_points
#' @param digits default is 6
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \donttest{
#' library(rms)
#' set.seed(2018)
#' n <-2019
#' age <- rnorm(n,60,20)
#' sex <- factor(sample(c('female','male'),n,TRUE))
#' sex <- as.numeric(sex)
#' weight <- sample(50:100,n,replace = TRUE)
#' time <- sample(50:800,n,replace = TRUE)
#' units(time)="day"
#' death <- sample(c(1,0,0),n,replace = TRUE)
#' df <- data.frame(time,death,age,sex,weight)
#' ddist <- datadist(df)
#' options(datadist='ddist')
#' f <- cph(formula(Surv(time,death)~sex+age+weight),data=df,
#'          linear.predictors=TRUE,
#'          x=TRUE,y=TRUE,surv=TRUE,time.inc=3)
#' surv <- Survival(f)
#' nomo <- nomogram(f,
#'                  lp=TRUE,
#'                  fun=list(function(x) surv(365,x),
#'                           function(x) surv(365*2,x)),
#'                  funlabel=c("1-Year Survival Prob",
#'                             "2-Year Survival Prob"))
#' library(nomogramFormula)
#' formula_lp(nomo)
#' totalpoints <- total_points(lp=f$linear.predictors)
#' formula_probability(nomo) 
#' total_probability(totalpoints = totalpoints)
#' }
total_probability <- function(totalpoints,digits=6){
    options(digits=digits)
    formula=Formula_probability
    nomoF.matrix=as.matrix(formula)
    for (i in 1:nrow(nomoF.matrix)) {
        if (i==1) score=data.frame(row.names = 1:length(totalpoints))
        beta_for_x=nomoF.matrix[i,]
        x_for_beta=totalpoints
        for (j in 1:length(nomoF.matrix[i,])) {
            if (is.na(beta_for_x[j])) next(j)
            if (j==1){
                each.var.score=(beta_for_x[j])*(x_for_beta^(j-1))
            }else{
                each.var.score=each.var.score+(beta_for_x[j])*(x_for_beta^(j-1))
            }
        }
        score=cbind(score,each.var.score)
    }
    colnames(score)=rownames(formula)
    score
}
