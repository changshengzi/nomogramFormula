#' get probability for nomogram
#'
#' @param nomogram nomogram after nomogram command from rms package
#' @param power power after nomoFormu_probability_cat
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
#'          x=TRUE,y=TRUE,surv=TRUE,time.inc=3)
#' surv <- Survival(f)
#' nomo <- nomogram(f,
#'                  lp=TRUE,
#'                  fun=list(function(x) surv(365,x),
#'                           function(x) surv(365*2,x)),
#'                  funlabel=c("1-Year Survival Prob",
#'                             "2-Year Survival Prob"))
#' library(nomogramFormula)
#' nomoFormu_probability_get(nomogram = nomo,power = 1)
#' nomoFormu_probability_get(nomogram = nomo,power = 2)
#' nomoFormu_probability_get(nomogram = nomo,power = 3)
#' }
nomoFormu_probability_get <- function(nomogram,power){
    if (missing(power)) stop("please give a value to power")
    if (power<1) stop("power must not be less 1")
    sub.nom=sub("","",nomogram)
    location=c(unlist("x.real" %s=% sub.nom),unlist("which =" %s=% sub.nom))
    names(location)=NULL
    sub.nom.real2=nomogram[location[duplicated(location)]]
    for (i in 1:length(sub.nom.real2)) {
        if (i==1){
            nomo.reslut=data.frame()
        }
        ######get each 3 variables
        nomo.i=sub.nom.real2[i]
        var.i=names(nomo.i)
        #change name of nomo.i to a, to get list points
        names(nomo.i)="a"
        points.i=nomo.i$a$x
        names(points.i)=NULL
        value.i=nomo.i$a$x.real
        ######caculate
        formu=paste0('value.i~',inner_Add_Symbol(paste0("I(points.i^",1:power,")")))
        reg=lm(as.formula(formu))
        coef=reg$coefficients
        lm.result=data.frame(t(coef))
        rownames(lm.result)=var.i
        colnames(lm.result)=c("b0",paste0("x^",1:power))
        nomo.reslut=rbind(nomo.reslut,lm.result)
    }
    nomo.reslut
}

