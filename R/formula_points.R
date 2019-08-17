#' @title explore formula of points and each variable
#'
#' @description explore the points formula and each variable to get best power.
#'
#' @param nomogram nomogram, after nomogram command from rms package
#' @param power if missing, power will be choose automatically
#' @param digits default is 6
#'
#' @return a global variable Formula_points, the formula of points and each variable
#' @export
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
#' formula_points(nomogram = nomo)
#' formula_points(nomogram = nomo,power = 1)
#' formula_points(nomogram = nomo,power = 2)
#' formula_points(nomogram = nomo,power = 3,digits=6)
#' }
#'
formula_points <- function(nomogram,power,digits=6){
    #total.points always appears in names of nomogram
    #total.points can only be changed in plot using points.label
    options(digits=digits)
    variable_part=nomogram[1:("total.points" %==% names(nomogram)-1)]
    id = 0
    ChiCheck=any(grepl("Chinese",sessionInfo()))
    cat("\n")
    if (ChiCheck) cat(crayon::red$bold(tmcn::toUTF8("\u62DF\u5408\u65B9\u7A0B: \u6839\u636E\u6BCF\u4E2A\u53D8\u91CF\u7684\u5F97\u5206\u8BA1\u7B97\u603B\u5F97\u5206")))
    if (!ChiCheck) cat(crayon::red$bold("Formula: caculate total points based on each variable"))
    cat("\n")
    if (missing(power)){
        #missing power : choose power automatically
        power = 0
        test=data.frame(R2=0.5)
        while (any(test$R2<1)) {
            power=power+1
            for (i in 1:length(variable_part)) {
                if (i==1){
                    nomo.reslut=data.frame()
                    test=data.frame()
                    real_fit_list=list()
                }
            ######get each 3 variables
                nomo.i=nomogram[i]
                var.i=names(nomo.i)
                #change name of nomo.i to a, to get list points
                names(nomo.i)="a"
                points.i=nomo.i$a$points
                names(points.i)=NULL
                value.i=as.numeric(unlist(nomo.i$a[1]))
                if (any(is.na(value.i))){
                    if (ChiCheck) stop(tmcn::toUTF8("\u8BF7\u786E\u4FDD\u53D8\u91CF"),
                                       var.i,
                                       tmcn::toUTF8("\u5728\u56DE\u5F52\u4E2D\u662F\u6570\u5B57"))
                    if (!ChiCheck) stop("please make sure that the variable",var.i," is a number in the regression")
                }
            ######caculate
                formu=paste0('points.i~',inner_Add_Symbol(paste0("I(value.i^",1:power,")")))
                reg=lm(as.formula(formu))
                fit.i=predict(reg)
                diff=round(points.i-fit.i,6)
                real_fit=t(data.frame(nomogram=points.i,fit=fit.i,diff))
                colnames(real_fit)=value.i
                real_fit.i=list(real_fit)
                names(real_fit.i)=var.i
                real_fit_list=c(real_fit_list,real_fit.i)
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
        id = id +1
        if (ChiCheck){
            cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,
                tmcn::toUTF8(". power\u503C:"))),power,"\n")
        }else{
            cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,
                ". power chooses:")),power,"\n")
        }
    }else{
        #exist power
        if (power<1) stop("power must not be less 1")
        for (i in 1:length(variable_part)) {
            if (i==1){
                nomo.reslut=data.frame()
                test=data.frame()
                real_fit_list=list()
            }
            ######get each 3 variables
            nomo.i=nomogram[i]
            var.i=names(nomo.i)
            #change name of nomo.i to a, to get list points
            names(nomo.i)="a"
            points.i=nomo.i$a$points
            names(points.i)=NULL
            value.i=as.numeric(unlist(nomo.i$a[1]))
            if (any(is.na(value.i))){
                if (ChiCheck) stop(tmcn::toUTF8("\u8BF7\u786E\u4FDD\u53D8\u91CF"),
                                   var.i,
                                   tmcn::toUTF8("\u5728\u56DE\u5F52\u4E2D\u662F\u6570\u5B57"))
                if (!ChiCheck) stop("please make sure that the variable",var.i," is a number in the regression")
            }
            ######caculate
            formu=paste0('points.i~',inner_Add_Symbol(paste0("I(value.i^",1:power,")")))
            reg=lm(as.formula(formu))
            fit.i=predict(reg)
            diff=round(points.i-fit.i,6)
            real_fit=t(data.frame(nomogram=points.i,fit=fit.i,diff))
            colnames(real_fit)=value.i
            real_fit.i=list(real_fit)
            names(real_fit.i)=var.i
            real_fit_list=c(real_fit_list,real_fit.i)
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
  if (ChiCheck){
      #formula
      id = id + 1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
              tmcn::toUTF8("\u5F97\u5230\u7684\u516C\u5F0F")),"\n")
      print(nomo.reslut)
      Formula_points<<-nomo.reslut
      #test
      id = id +1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          tmcn::toUTF8("\u6BCF\u4E2A\u53D8\u91CF\u7684R\u65B9\u548CRMSE")),"\n")
      print(test)
      #diff
      id = id +1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          tmcn::toUTF8("\u6BD4\u8F83\u6BCF\u4E2A\u53D8\u91CF\u7684nomogram\u5F97\u5206\u548C\u9884\u6D4B\u5F97\u5206")),"\n")
      for (i in 1:length(real_fit_list)){
        predic.i=real_fit_list[i]
        cat(crayon::black$bgWhite(names(predic.i)),"\n")
        print(predic.i[[1]])
      }
  }else{
      #formula
      id = id + 1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          "Formula"),"\n")
      print(nomo.reslut)
      Formula_points<<-nomo.reslut
      #test
      id = id +1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          "R2 and RMSE for Formula"),"\n")
      print(test)
      #diff
      id = id +1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          "difference between fit and nomogram points"),"\n")
      for (i in 1:length(real_fit_list)){
        predic.i=real_fit_list[i]
        cat(crayon::black$bgWhite(names(predic.i)),"\n")
        print(predic.i[[1]])
      }
}
}

