##
## Reproductive success - age at first birth
##
## Supplementary Table 8
##
source("init.r")

library(survival)
library(MuMIn)


####################################################################################
## Prepare data
##
mosuo.afb = as.data.frame( subset(mosuo.people, Age >= 15 & EthnicGroup=="Mosuo") )
mosuo.afb$AFB = ifelse(is.na(mosuo.afb$AFB), 0, mosuo.afb$AFB)

mosuo.afb$Cohort = ifelse(mosuo.afb$BirthYear < 1941, "Before_1941",
                          ifelse(mosuo.afb$BirthYear >= 1941 & mosuo.afb$BirthYear <= 1972, "1941_to_1972", "After_1972"))
mosuo.afb$Cohort = as.factor(mosuo.afb$Cohort)
mosuo.afb$Cohort = relevel(mosuo.afb$Cohort, "Before_1941")

# censor people who never gave birth
mosuo.afb$Status = ifelse(mosuo.afb$AFB > 0, 2, 1)  # status==2 for people who gave birth; ==1 for those who didn't
mosuo.afb$SurvObj = with(mosuo.afb, Surv(AFB, Status==2))

# descriptives about AFB
## mode function from: http://stackoverflow.com/a/8189441
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

library(plyr)
ddply(subset(mosuo.afb, AFB>0),
      .(zhubo1, Sex), plyr::summarise,
      min = min(AFB),
      max = max(AFB),
      mean = mean(AFB),
      median = median(AFB),
      mode = Mode(AFB)
      )


####################################################################################
## Cox regressions
##
# pick one of these two
mosuo.afb_f = na.omit(subset(mosuo.afb, Sex=="f", select=c(SurvObj, Cohort, NumSibs.SameSex, NumSibs.DiffSex, year.of.education, income01_12, livestockno_12, zhubo1)))
mosuo.afb_m = na.omit(subset(mosuo.afb, Sex=="m", select=c(SurvObj, Cohort, NumSibs.SameSex, NumSibs.DiffSex, year.of.education, income01_12, livestockno_12, zhubo1)))

# sample sizes
nrow(mosuo.afb_f)
nrow(mosuo.afb_m)

#' Fit Cox regressions to model females and males ages at first birth
#'
#' @param d Data: either `mosuo.afb_f` or `mosuo.afb_m`
#'
#' @return Named list containing model selection table and summary of average model
#'         Note that the parameter "No. same-sex sibs" should be changed to "No. sisters" for females and "No. brothers" for males
#'         (and similar for 'opposite-sex sibs')
#'
fit_cox = function(d){
  afb_models = list()
  # null
  afb_models[[1]] = coxph(SurvObj ~ 1, data=d)
  # control
  afb_models[[2]] = coxph(SurvObj ~ Cohort + NumSibs.SameSex + NumSibs.DiffSex + year.of.education + income01_12 + livestockno_12, data=d)
  # zhu
  afb_models[[3]] = coxph(SurvObj ~ Cohort + NumSibs.SameSex + NumSibs.DiffSex + year.of.education + income01_12 + livestockno_12 + zhubo1, data=d)
  
  afb_names = c("Null", "Control", "Control + Zhubo")
  
  # model selection/averaging
  best.models = model.sel(afb_models)
  avg.model = model.avg(best.models, subset = delta < 2, revised.var=T)
  
  # get param estimates and CIs
  avg.model.sum = summary(avg.model)$coefficients[2,] %>% 
    dplyr::as_data_frame() %>% 
    rownames_to_column(var="Parameter") %>% 
    bind_cols(confint(avg.model) %>% dplyr::as_data_frame())
  
  # prettify parameter names
  avg.model.sum$Parameter <- c("Cohort 1941 to 1972", "Cohort After 1972"
                             ,"No. same-sex sibs", "No. opposite-sex sibs"
                             ,"Education", "Tourist income (ref: no)", "No. livestock"
                             ,"Zhu? (ref: no)"
  )
  
  # calculate hazard ratios + CIs
  avg.model.sum$HR = exp(avg.model.sum$value)
  avg.model.sum$HR_l = exp(avg.model.sum$`2.5 %`)
  avg.model.sum$HR_u = exp(avg.model.sum$`97.5 %`)
  
  # make model selection table prettier
  best.models$Model = afb_names[ as.integer(rownames(best.models))]  # add model names in correct order
  best.models = best.models %>% 
    dplyr::select(Model, K=df, delta, weight, LL=logLik)
  
  return(list(
    best.models = best.models,
    avg.model.sum = avg.model.sum
  ))
}

# fit models
afb_f = fit_cox(mosuo.afb_f)
afb_m = fit_cox(mosuo.afb_m)

# save everything
write_csv(afb_f$best.models, file.path(results.dir, "AFB - confidence set - females.csv"))
write_csv(afb_m$best.models, file.path(results.dir, "AFB - confidence set - males.csv"))

write_csv(afb_f$avg.model.sum, file.path(results.dir, "AFB - param estimates - females.csv"))
write_csv(afb_m$avg.model.sum, file.path(results.dir, "AFB - param estimates - males.csv"))
