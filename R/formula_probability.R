#' @title explore formula for probability and total points formula
#' @description explore the probability formula to total points and get the best power.
#'
#' @param nomogram nomogram after nomogram command from rms package
#' @param power power, if missing, power will be choose automatically
#' @param digits default is 6
#' @importFrom utils sessionInfo
#' @importFrom stats as.formula lm predict t.test
#' @importFrom graphics legend par plot points
#' @return a global variable Formula_probability, the formula of probability and total points
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
#' nomoFormu_probability(nomogram = nomo,power = 1)
#' nomoFormu_probability(nomogram = nomo,power = 2)
#' nomoFormu_probability(nomogram = nomo,power = 3)
#' }
formula_probability <- function(nomogram,power,digits=6){
    options(digits=digits)
    #get probability part
    if ("lp" %in% names(nomogram)){
        lp_location=("lp" %==% names(nomogram))
        prob_part=nomogram[(lp_location+1):length(nomogram)]
    }else if("total.points" %in% names(nomogram)){
        ttp_location=("total.points" %==% names(nomogram))
        prob_part=nomogram[(ttp_location+1):length(nomogram)]
    }
    ChiCheck=any(grepl("Chinese",sessionInfo()))
    id = 0
    if (missing(power)){
        #missing power : choose power automatically
        power = 0
        test=data.frame(R2=0.5)
        while (any(test$R2<1)) {
            if (power==100) {
                if (ChiCheck) stop(tmcn::toUTF8("power\u503C\u5DF2\u8FBE\u5230\u6781\u9650100,\u8BF7\u624B\u52A8\u8BBE\u7F6Epower\u503C"))
                if (!ChiCheck) stop("power gets limit 100, R2 still lower1, please choose power by hand.")
            }
            power=power+1
            for (i in 1:length(prob_part)) {
                if (i==1){
                    nomo.reslut=data.frame()
                    test=data.frame()
                    real_fit_list=list()
                }
                ######get each 3 variables
                nomo.i=prob_part[i]
                var.i=names(nomo.i)
                #change name of nomo.i to a, to get list points
                names(nomo.i)="a"
                points.i=nomo.i$a$x
                names(points.i)=NULL
                value.i=nomo.i$a$x.real
                ######caculate
                formu=as.formula(paste0('value.i~',inner_Add_Symbol(paste0("I(points.i^",1:power,")"))))
                reg=lm(formu)
                fit.i=predict(reg)
                value.i=value.i
                diff=round(value.i-fit.i,6)
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
        id = id +1
        if (any(grepl("Chinese",sessionInfo()))){
            cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,
                                                                   tmcn::toUTF8(". power\u503C:"))),power,"\n")
        }else{
            cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,
                                                                   ". power chooses:")),power,"\n")
        }
    }else{
        #exist power
        if (power<1) stop("power must not be less 1")
        for (i in 1:length(prob_part)) {
            if (i==1){
                nomo.reslut=data.frame()
                test=data.frame()
                real_fit_list=list()
            }
            ######get each 3 variables
            nomo.i=prob_part[i]
            var.i=names(nomo.i)
            #change name of nomo.i to a, to get list points
            names(nomo.i)="a"
            points.i=nomo.i$a$x
            names(points.i)=NULL
            value.i=nomo.i$a$x.real
            ######caculate
            formu=as.formula(paste0('value.i~',inner_Add_Symbol(paste0("I(points.i^",1:power,")"))))
            reg=lm(formu)
            fit.i=predict(reg)
            value.i=value.i
            diff=round(value.i-fit.i,6)
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
    if (ChiCheck){
        #formula
        id = id + 1
        cat("\n")
        cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
                                                        tmcn::toUTF8("\u5F97\u5230\u7684\u516C\u5F0F")),"\n")
        print(nomo.reslut)
        Formula_probability<<-nomo.reslut
        #r2 rmse
        id = id +1
        cat("\n")
        cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
                                                        tmcn::toUTF8("R\u65B9\u548CRMSE")),"\n")
        print(test)
        #diff
        if (length(diff)>50){
            id = id +1
            cat("\n")
            cat(crayon::black$bgCyan("  "),
                crayon::red$bold(paste0(id,'.'),
                                 tmcn::toUTF8("\u753B\u56FE")),"\n")
            par(mfrow=c(length(real_fit_list),2))
            for (i in 1:length(real_fit_list)){
                predic.i=real_fit_list[i]
                #cat(crayon::black$bgWhite(names(predic.i)),"\n")
                #print(predic.i[[1]])
                length.check=length(predic.i[[1]]["nomogram",])
                cex.points=ifelse(length.check<100,1,
                                  ifelse(length.check<500,0.5,0.3))
                plot(x=colnames(predic.i[[1]]),
                     y=predic.i[[1]]["nomogram",],
                     main = paste0(names(predic.i),"\n",
                                   "probability of nomogram and fit"),
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
                     ylab="P.nomogram - P.fit",
                     pch=16,cex=cex.points)
            }
        }else{
            id = id +1
            cat("\n")
            cat(crayon::black$bgCyan("  "),
                crayon::red$bold(paste0(id,'.'),
                tmcn::toUTF8("\u6BD4\u8F83nomogram\u7684\u6982\u7387\u548C\u62DF\u5408\u6982\u7387")),"\n")
            for (i in 1:length(real_fit_list)){
                predic.i=real_fit_list[i]
                cat(crayon::black$bgWhite(names(predic.i)),"\n")
                print(predic.i[[1]])
            }
        }
    }else{
        #formula
        id = id + 1
        cat("\n")
        cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'. Formula')),"\n")
        print(nomo.reslut)
        Formula_probability<<-nomo.reslut
        #r2 rmse
        id = id +1
        cat("\n")
        cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'. R2 and RMSE')),"\n")
        print(test)
        #diff
        if (length(diff)>50){
            id = id +1
            cat("\n")
            cat(crayon::black$bgCyan("  "),
                crayon::red$bold(paste0(id,'. plot')),"\n")
            par(mfrow=c(length(real_fit_list),2))
            for (i in 1:length(real_fit_list)){
                predic.i=real_fit_list[i]
                #cat(crayon::black$bgWhite(names(predic.i)),"\n")
                #print(predic.i[[1]])
                length.check=length(predic.i[[1]]["nomogram",])
                cex.points=ifelse(length.check<100,1,
                                  ifelse(length.check<500,0.5,0.3))
                plot(x=colnames(predic.i[[1]]),
                     y=predic.i[[1]]["nomogram",],
                     main = paste0(names(predic.i),"\n",
                                   "probability of nomogram and fit"),
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
                     ylab="P.nomogram - P.fit",
                     pch=16,cex=cex.points)
            }
        }else{
            id = id +1
            cat("\n")
            cat(crayon::black$bgCyan("  "),
                crayon::red$bold(paste0(id,'. compare the probability of nomogram and fit')),"\n")
            for (i in 1:length(real_fit_list)){
                predic.i=real_fit_list[i]
                cat(crayon::black$bgWhite(names(predic.i)),"\n")
                print(predic.i[[1]])
            }
        }
    }
}

