#' Caculate nomogram total points
#'
#' @param data data must be with no NA
#' @param lp linear predictors
#' @param digits default is 6
#'
#' @return total Points
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
#'          x=TRUE,y=TRUE,surv=TRUE,time.inc=3)
#' surv <- Survival(f)
#' nomo <- nomogram(f,
#'                  lp=TRUE,
#'                  fun=list(function(x) surv(365,x),
#'                           function(x) surv(365*2,x)),
#'                  funlabel=c("1-Year Survival Prob",
#'                             "2-Year Survival Prob"))
#' library(nomogramFormula)
#' #useing raw data to caculate total points
#' formula_points(nomo)
#' total_points(data=df)
#' #using linear predictors to caculate total points
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
#' formula_lp(nomo)
#' total_points(lp=f$linear.predictors)
#' }
total_points <- function(data,lp,digits=6){
    options(digits=digits)
    #data
    if (!missing(data)){
        if (any(is.na(data))) {
            if (any(grepl("Chinese",sessionInfo()))){
                stop(tmcn::toUTF8("\u6570\u636E\u4E2D\u4E0D\u5E94\u6709\u7F3A\u5931\u503C"))
            }else{
                stop("data must not have NAs.")
            }
        }
        formula=Formula_points
        nomoF.matrix=as.matrix(formula)
        data1=data[,rownames(nomoF.matrix)]
        for (i in 1:ncol(data1)) {
            if (i==1) score=rep(0,nrow(data1))
            beta_for_x=nomoF.matrix[i,]
            x_for_beta=data1[,i]
            if (is.factor(x_for_beta)){
                x_for_beta=as.numeric(as.character(x_for_beta))
            }
            for (j in 1:length(nomoF.matrix[i,])) {
                if (is.na(beta_for_x[j])) next(j)
                if (j==1){
                    each.var.score=(beta_for_x[j])*(x_for_beta^(j-1))
                }else{
                    each.var.score=each.var.score+(beta_for_x[j])*(x_for_beta^(j-1))
                }
            }
            score=score+each.var.score
        }
        return(score)
    }
    #lp
    if (!missing(lp)){
        formula=Formula_lp
        for (j in 1:ncol(formula)) {
            if (is.na(formula[1,j])) next(j)
            if (j==1){
                score=(formula[1,j])*(lp^(j-1))
            }else{
                score=score+(formula[1,j])*(lp^(j-1))
            }
        }
        return(score)
    }
}
