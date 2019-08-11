#' @title extra and cat formula for points
#'
#' @description explore the points formula and get proper power.
#'
#'
#' @param nomogram nomogram after nomogram command from rms package
#' @param power power, if missing, power will be choose automatically
#'
#' @return list
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
#' nomoFormu_points_cat(nomogram = nomo)
#' nomoFormu_points_cat(nomogram = nomo,power = 1)
#' nomoFormu_points_cat(nomogram = nomo,power = 2)
#' nomoFormu_points_cat(nomogram = nomo,power = 3)
#' }
#'
nomoFormu_points_cat <- function(nomogram,power){
    sub.nom=sub("","",nomogram)
    sub.nom.real=sub.nom[!grepl("x.real",sub.nom)]
    sub.nom.real2=sub.nom.real[-length(sub.nom.real)]
    if (missing(power)){
        #missing power : choose power automatically
        power = 0
        test=data.frame(R2=0.5)
        while (any(test$R2<1)) {
            power=power+1
            for (i in 1:length(sub.nom.real2)) {
                if (i==1){
                    nomo.reslut=data.frame()
                    test=data.frame()
                    real_predict_list=list()
                }
                ######get each 3 variables
                nomo.i=nomogram[i]
                var.i=names(nomo.i)
                #change name of nomo.i to a, to get list points
                names(nomo.i)="a"
                points.i=nomo.i$a$points
                names(points.i)=NULL
                value.i=as.numeric(unlist(nomo.i$a[var.i %==% names(nomo.i$a)]))
                ######caculate
                formu=paste0('points.i~',inner_Add_Symbol(paste0("I(value.i^",1:power,")")))
                reg=lm(as.formula(formu))
                predict.i=predict(reg)
                diff=round(predict.i-points.i,5)
                real_predict=t(data.frame(nomogram=points.i,predict=predict.i,diff))
                colnames(real_predict)=value.i
                real_predict.i=list(real_predict)
                names(real_predict.i)=var.i
                real_predict_list=c(real_predict_list,real_predict.i)
                R2=suppressWarnings(summary(reg)$r.squared)
                RMSE=(mean(predict(reg)-points.i)^2)^(1/2)
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
                real_predict_list=list()
            }
            ######get each 3 variables
            nomo.i=nomogram[i]
            var.i=names(nomo.i)
            #change name of nomo.i to a, to get list points
            names(nomo.i)="a"
            points.i=nomo.i$a$points
            names(points.i)=NULL
            value.i=as.numeric(unlist(nomo.i$a[var.i %==% names(nomo.i$a)]))
            ######caculate
            formu=paste0('points.i~',inner_Add_Symbol(paste0("I(value.i^",1:power,")")))
            reg=lm(as.formula(formu))
            predict.i=predict(reg)
            diff=round(predict.i-points.i,5)
            real_predict=t(data.frame(nomogram=points.i,predict=predict.i,diff))
            colnames(real_predict)=value.i
            real_predict.i=list(real_predict)
            names(real_predict.i)=var.i
            real_predict_list=c(real_predict_list,real_predict.i)
            R2=suppressWarnings(summary(reg)$r.squared)
            RMSE=(mean(predict(reg)-points.i)^2)^(1/2)
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
      #formula
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(tmcn::toUTF8("\u5F97\u5230\u7684\u516C\u5F0F")),"\n")
      print(nomo.reslut)
      cat("\n")
      #test
      cat(crayon::black$bgCyan("  "),crayon::red$bold(tmcn::toUTF8("\u6BCF\u4E2A\u53D8\u91CF\u7684R\u65B9\u548CRMSE")),"\n")
      print(test)
      #diff
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(tmcn::toUTF8("\u6BD4\u8F83\u6BCF\u4E2A\u53D8\u91CF\u7684nomogram\u5F97\u5206\u548C\u9884\u6D4B\u5F97\u5206")),"\n")
      for (i in 1:length(real_predict_list)){
        predic.i=real_predict_list[i]
        cat(crayon::black$bgWhite(names(predic.i)),"\n")
        print(predic.i[[1]])
      }
  }else{
    cat("\n")
    cat(crayon::black$bgCyan("  "),crayon::red$bold("Formula"),"\n")
    print(nomo.reslut)
    cat("\n")
    cat(crayon::black$bgCyan("  "),crayon::red$bold("R2 and RMSE for each variable"),"\n")
    print(test)
    cat("\n")
    cat(crayon::black$bgCyan("  "),crayon::red$bold("difference between predict and nomogram points"),"\n")
    for (i in 1:length(real_predict_list)){
      predic.i=real_predict_list[i]
      cat(crayon::black$bgWhite(names(predic.i)),"\n")
      print(predic.i[[1]])
    }
}
}

