#' @title explore formula of total points and linear predictors
#' @description explore the total points formula to linear predictors and get best power.
#'
#' @param nomogram nomogram after nomogram command from rms package
#' @param power if missing, power will be choose automatically
#' @param digits default is 6
#'
#' @return a global variable Formula_lp, the formula of total points and linear predictors
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
#' formula_lp(nomogram = nomo)
#' formula_lp(nomogram = nomo,power = 1)
#' formula_lp(nomogram = nomo,power = 2)
#' formula_lp(nomogram = nomo,power = 3)
#' }
#'
formula_lp <- function(nomogram,power,digits=6){
    #total.points always appears in names of nomogram
    #total.points can only be changed in plot using points.label
    options(digits=digits)
    variable_part=nomogram["lp" == names(nomogram)]
    #check langue chinese
    ChiCheck=any(grepl("Chinese",sessionInfo()))
    id = 0
    if (missing(power)){
        #missing power : choose power automatically
        power = 0
        test=data.frame(R2=0.5)
        while (test$R2<1) {
            power=power+1
        ######get 2 variables
            #1
            points=as.numeric(unlist(variable_part$lp[1]))
            #2
            lp=as.numeric(unlist(variable_part$lp[2]))
        ######caculate
            formu=as.formula(paste0('points~',
                                    inner_Add_Symbol(paste0("I(lp^",1:power,")"))))
            #regressiong
            reg=lm(formu)
            #formula
            lm.result=data.frame(t(reg$coefficients))
            rownames(lm.result)="linear predictor"
            colnames(lm.result)=c("b0",paste0("x^",1:power))
            #real,fit,diff
            fit=predict(reg)
            diff=round(points-fit,6)
            real_fit=t(data.frame(nomogram=points,fit,diff))
            colnames(real_fit)=lp
            # test
            R2=suppressWarnings(summary(reg)$r.squared)
            RMSE=(mean(predict(reg)-points)^2)^(1/2)
            test=data.frame(R2,RMSE)
            if (power==100){
                if (ChiCheck) stop(tmcn::toUTF8("power\u503C\u8FBE\u5230\u6781\u503C100"))
                if (!ChiCheck) stop("power reaches the limit: 100")
            }
        }
        cat("\n")
        #output auto power
        if (ChiCheck){
            id = id +1
            cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,
                tmcn::toUTF8(". power\u503C:"))),power,"\n")
        }
        if (!ChiCheck){
            id = id +1
            cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,
                ". power chooses:")),power,"\n")
        }
    }else{
        #exist power
        if (power<1){
            if (ChiCheck) stop(tmcn::toUTF8("power\u503C\u4E0D\u80FD\u5C0F\u4E8E1"))
            if (!ChiCheck) stop("power must not be less 1")
        }
        ######get 2 variables
        #1
        points=as.numeric(unlist(variable_part$lp[1]))
        #2
        lp=as.numeric(unlist(variable_part$lp[2]))
        ######caculate
        formu=as.formula(paste0('points~',
                                inner_Add_Symbol(paste0("I(lp^",1:power,")"))))
        #regressiong
        reg=lm(formu)
        #formula
        lm.result=data.frame(t(reg$coefficients))
        rownames(lm.result)="linear predictor"
        colnames(lm.result)=c("b0",paste0("x^",1:power))
        #real,fit,diff
        fit=predict(reg)
        diff=round(points-fit,6)
        real_fit=t(data.frame(nomogram=points,fit,diff))
        colnames(real_fit)=lp
        # test
        R2=suppressWarnings(summary(reg)$r.squared)
        RMSE=(mean(predict(reg)-points)^2)^(1/2)
        test=data.frame(R2,RMSE)
    }
  rownames(test)="linear predictor"
  if (ChiCheck){
      #formula
      id = id + 1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
              tmcn::toUTF8("\u5F97\u5230\u7684\u516C\u5F0F")),"\n")
      print(lm.result)
      Formula_lp<<-lm.result
      #test
      id = id +1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          tmcn::toUTF8("\u7EBF\u6027\u9884\u6D4B\u503C\u7684R\u65B9\u548CRMSE")),"\n")
      print(test)
      #diff
      id = id +1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          tmcn::toUTF8("\u6BD4\u8F83\u7EBF\u6027\u9884\u6D4B\u503C\u7684nomogram\u5F97\u5206\u548C\u9884\u6D4B\u5F97\u5206")),"\n")
      print(real_fit)
      
  }else{
      #formula
      id = id + 1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          "Formula"),"\n")
      print(lm.result)
      Formula_lp<<-lm.result
      #test
      id = id +1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          "R2 and RMSE for linear predictor"),"\n")
      print(test)
      #diff
      id = id +1
      cat("\n")
      cat(crayon::black$bgCyan("  "),crayon::red$bold(paste0(id,'.'),
          "difference between fit and nomogram points"),"\n")
      print(real_fit)
}
}

