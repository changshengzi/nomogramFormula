#' @title extra and cat formula for probability formula
#' @description explore the probability formula and get proper power.
#'
#' @param nomogram nomogram after nomogram command from rms package
#' @param power power, if missing, power will be choose automatically
#' @importFrom utils sessionInfo
#' @importFrom stats as.formula lm predict t.test
#' @importFrom graphics legend par plot points
#' @return list and picture
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
#' nomoFormu_probability_cat(nomogram = nomo,power = 1)
#' nomoFormu_probability_cat(nomogram = nomo,power = 2)
#' nomoFormu_probability_cat(nomogram = nomo,power = 3)
#' }
nomoFormu_probability_cat <- function(nomogram,power){
        sub.nom=sub("","",nomogram)
        location=c(unlist("x.real" %s=% sub.nom),unlist("which =" %s=% sub.nom))
        names(location)=NULL
        sub.nom.real2=nomogram[location[duplicated(location)]]
        if (missing(power)){
            #missing power : choose power automatically
            power = 0
            test=data.frame(R2=0.5)
            while (any(test$R2<1)) {
                if (power==100) {
                    if (any(grepl("Chinese",sessionInfo()))){
                        stop(tmcn::toUTF8("power\u503C\u5DF2\u8FBE\u5230\u6781\u9650100,\u8BF7\u624B\u52A8\u8BBE\u7F6Epower\u503C"))
                    }else{
                        stop("power gets limit 100, R2 still lower1, please choose power by hand.")
                    }
                }
                power=power+1
                for (i in 1:length(sub.nom.real2)) {
                    if (i==1){
                        nomo.reslut=data.frame()
                        test=data.frame()
                        real_fit_list=list()
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
                    fit.i=predict(reg)
                    value.i=value.i
                    diff=round(fit.i-value.i,5)
                    real_fit=t(data.frame(nomogram=value.i,fit=fit.i,diff))
                    colnames(real_fit)=round(points.i,6)
                    real_fit.i=list(real_fit)
                    names(real_fit.i)=var.i
                    real_fit_list=c(real_fit_list,real_fit.i)
                    R2=suppressWarnings(summary(reg)$r.squared)
                    RMSE=(mean(predict(reg)-value.i)^2)^(1/2)
                    test.i=data.frame(R2,RMSE)
                    test=rbind(test,test.i)
                    coef=reg$coefficients
                    lm.result=data.frame(t(coef))
                    rownames(lm.result)=var.i
                    colnames(lm.result)=c("b0",paste0("x^",1:power))
                    nomo.reslut=rbind(nomo.reslut,lm.result)
                }
            }
            cat("\n")
            cat(crayon::black$bgCyan("  "),"power chooses:",power,"\n")
        }else{
            #exist power
            if (power<1) stop("power must not be less 1")
            for (i in 1:length(sub.nom.real2)) {
                if (i==1){
                    nomo.reslut=data.frame()
                    test=data.frame()
                    real_fit_list=list()
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
                fit.i=predict(reg)
                value.i=value.i
                diff=fit.i-value.i
                real_fit=t(data.frame(nomogram=value.i,
                                          fit=fit.i,diff))
                colnames(real_fit)=round(points.i,6)
                real_fit.i=list(real_fit)
                names(real_fit.i)=var.i
                real_fit_list=c(real_fit_list,real_fit.i)
                R2=suppressWarnings(summary(reg)$r.squared)
                RMSE=(mean(predict(reg)-value.i)^2)^(1/2)
                test.i=data.frame(R2,RMSE)
                test=rbind(test,test.i)
                coef=reg$coefficients
                lm.result=data.frame(t(coef))
                rownames(lm.result)=var.i
                colnames(lm.result)=c("b0",paste0("x^",1:power))
                nomo.reslut=rbind(nomo.reslut,lm.result)
            }
        }
        rownames(test)=rownames(nomo.reslut)
        if (any(grepl("Chinese",sessionInfo()))){
            #diff
            #scat("\n")
            #cat(crayon::black$bgCyan("  "),crayon::red$bold(tmcn::toUTF8("\u6BD4\u8F83nomogram\u6982\u7387\u4E0E\u62DF\u5408\u6982\u7387,\u5DEE\u503C(diff)\u4FDD\u75595\u4F4D\u5C0F\u6570")),"\n")
            par(mfrow=c(length(real_fit_list),2))
            for (i in 1:length(real_fit_list)){
                predic.i=real_fit_list[i]
                #cat(crayon::black$bgWhite(names(predic.i)),"\n")
                #print(predic.i[[1]])
                length.check=length(predic.i[[1]]["nomogram",])
                cex.points=ifelse(length.check<100,1,
                                  ifelse(length.check<500,0.5,0.3))
                paired.test=t.test(predic.i[[1]]["nomogram",],predic.i[[1]]["fit",],paired = TRUE)
                plot(x=colnames(predic.i[[1]]),
                     y=predic.i[[1]]["nomogram",],
                     main = paste0(names(predic.i),"\n",
                                   "probability of nomogram and fit",
                                   "\np value of paired t-test:",round(paired.test$p.value,4)),
                     xlab = "points",
                     ylab="probability",
                     pch=16,cex=cex.points)
                points(x = colnames(predic.i[[1]]),
                       y=predic.i[[1]]["fit",],
                       pch=16,cex=cex.points,col="red")
                legend("top",legend = c("nomogram","fit"),
                       col=c("black","red"),pch=16,
                       box.col = "transparent",bg="transparent")
                plot(x = colnames(predic.i[[1]]),
                     y=predic.i[[1]]["diff",],
                     main = paste0(names(predic.i)," probability difference\nbebween nomogram and fit"),
                     xlab = "points",
                     ylab="P.fit - P.nomogram",
                     pch=16,cex=cex.points)
            }
            #formula
            cat("\n")
            cat(crayon::black$bgCyan("  "),crayon::red$bold(tmcn::toUTF8("\u5F97\u5230\u7684\u516C\u5F0F")),"\n")
            print(nomo.reslut)
            #r2 rmse
            cat("\n")
            cat(crayon::black$bgCyan("  "),crayon::red$bold(tmcn::toUTF8("R\u65B9\u548CRMSE")),"\n")
            print(test)
        }else{
            #diff
            #scat("\n")
            #cat(crayon::black$bgCyan("  "),crayon::red$bold(tmcn::toUTF8("\u6BD4\u8F83nomogram\u6982\u7387\u4E0E\u62DF\u5408\u6982\u7387,\u5DEE\u503C(diff)\u4FDD\u75595\u4F4D\u5C0F\u6570")),"\n")
            par(mfrow=c(length(real_fit_list),2))
            for (i in 1:length(real_fit_list)){
                predic.i=real_fit_list[i]
                #cat(crayon::black$bgWhite(names(predic.i)),"\n")
                #print(predic.i[[1]])
                length.check=length(predic.i[[1]]["nomogram",])
                cex.points=ifelse(length.check<100,1,
                                  ifelse(length.check<500,0.5,0.3))
                paired.test=t.test(predic.i[[1]]["nomogram",],predic.i[[1]]["fit",],paired = TRUE)
                plot(x=colnames(predic.i[[1]]),
                     y=predic.i[[1]]["nomogram",],
                     main = paste0(names(predic.i),"\n",
                                   "probability of nomogram and fit",
                                   "\np value of paired t-test:",
                                   round(paired.test$p.value,4)),
                     xlab = "points",
                     ylab="probability",
                     pch=16,cex=cex.points)
                points(x = colnames(predic.i[[1]]),
                       y=predic.i[[1]]["fit",],
                       pch=16,cex=cex.points,col="red")
                legend("top",legend = c("nomogram","fit"),
                       col=c("black","red"),pch=16,
                       box.col = "transparent",bg="transparent")
                plot(x = colnames(predic.i[[1]]),
                     y=predic.i[[1]]["diff",],
                     main = paste0(names(predic.i)," probability difference\nbebween nomogram and fit"),
                     xlab = "points",
                     ylab="P.fit - P.nomogram",
                     pch=16,cex=cex.points)
            }
            #formula
            cat("\n")
            cat(crayon::black$bgCyan("  "),crayon::red$bold("Formula"),"\n")
            print(nomo.reslut)
            #r2 rmse
            cat("\n")
            cat(crayon::black$bgCyan("  "),crayon::red$bold("R2 and RMSE"),"\n")
            print(test)
    }
}

