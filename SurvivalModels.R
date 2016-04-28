## Load packages 
library(data.table)
library(survival)
library(ggplot2)
library(ggfortify)
library(OIsurv)
library(fmsb)

dat_df <- read.csv(dat_dt,"ProcessedData.csv")

## Dependent variables

X <-dat_df[,c("Age","Ethnicity","Gender","GraduationPeriod","state","Same_city_members",
              "Same_degree_year_members", "Type_membership","Degree_level","Membership_desc"     
              )]

time <- as.numeric(dat_dt$dtm)
event <- dat_dt$cens

## Check the pair-wise correlation for multicollinearity
numvar <- dat_df[,c("time","Age","Same_city_members",
                    "Same_degree_year_members"      )]
## If VIF > 10, then multicollinear
VIF(lm(time ~ ., data=numvar))

summary(X)
summary(time)
summary(event)


########### Survival analysis - Non-parametric ##############

## I: Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)
sink("KM_table")
summary(kmsurvival)
sink()
ggsurv(kmsurvival)
kmsurvival_ll <- survfit(Surv(time,event) ~ 1,conf.type = "log-log")
ggsurv(kmsurvival_ll)
kmsurvival_pl <- survfit(Surv(time,event) ~ 1,conf.type = "plain")
ggsurv(kmsurvival_pl)

# Kaplan-Meier non-parametric analysis by group

kmsurvival_gp <- survfit(Surv(time, event) ~ GraduationPeriod,data=dat_df)
ggsurv(kmsurvival_gp) 

survdiff(Surv(time, event)~GraduationPeriod, data=dat_df,rho=0)

kmsurvival_Ethnicity <- survfit(Surv(time, event) ~ Ethnicity,data=dat_df)
ggsurv(kmsurvival_Ethnicity)

survdiff(Surv(time, event)~Ethnicity, data=dat_df,rho=0)

dat_df_et1 <- dat_df[dat_df$Ethnicity %in% c("Caucasian","Hispanic"), ]
dat_df_et2 <- dat_df[dat_df$Ethnicity %in% c("Caucasian","others"), ]
dat_df_et3 <- dat_df[dat_df$Ethnicity %in% c("others","Hispanic"), ]

survdiff(Surv(as.numeric(dat_df_et1$dtm), dat_df_et1$cens)~Ethnicity, data=dat_df_et1,rho=0)
survdiff(Surv(as.numeric(dat_df_et2$dtm), dat_df_et2$cens)~Ethnicity, data=dat_df_et2,rho=0)
survdiff(Surv(as.numeric(dat_df_et3$dtm), dat_df_et3$cens)~Ethnicity, data=dat_df_et3,rho=0)

kmsurvival_gender <- survfit(Surv(time, event) ~ Gender,data=dat_df)
ggsurv(kmsurvival_gender) 

survdiff(Surv(time, event)~Gender, data=dat_df,rho=0)

kmsurvival_state <- survfit(Surv(time, event) ~ state,data=dat_df)
ggsurv(kmsurvival_state) 

survdiff(Surv(time, event)~state, data=dat_df,rho=0)

dat_df_st1 <- dat_df[dat_df$state %in% c("Arizona","California"), ]
dat_df_st2 <- dat_df[dat_df$state %in% c("Arizona","others"), ]
dat_df_st3 <- dat_df[dat_df$state %in% c("others","California"), ]

survdiff(Surv(as.numeric(dat_df_st1$dtm), dat_df_st1$cens)~state, data=dat_df_st1,rho=0)
survdiff(Surv(as.numeric(dat_df_st2$dtm), dat_df_st2$cens)~state, data=dat_df_st2,rho=0)
survdiff(Surv(as.numeric(dat_df_st3$dtm), dat_df_st3$cens)~state, data=dat_df_st3,rho=0)

kmsurvival_deg <- survfit(Surv(time, event) ~ Degree_level,data=dat_df)
ggsurv(kmsurvival_deg) 

survdiff(Surv(time, event)~Degree_level, data=dat_df,rho=0)


# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~1), type="aalen")
summary(nasurvival)
#plot(nasurvival, xlab="Time", ylab="Survival Probability")
ggsurv(nasurvival)

# Cox proportional hazard model - coefficients and hazard rates

data_in <- data.table(time, event, X)

#coxph1 <- coxph(Surv(time,event) ~ .,data=data_in, method="breslow")

coxph1 <- coxph(Surv(time,event) ~   Age  +           
                  Ethnicity    +    Gender    +              
                  GraduationPeriod     +    state   + 
                  Same_city_members + Same_degree_year_members  +
                  Type_membership + Degree_level ,data=data_in, method="breslow")
summary(coxph1)
extractAIC(coxph1)

coxpht <- coxph(Surv(time,event) ~   log(time):Age  +           
                  log(time):Ethnicity    +    log(time):Gender    +              
                  log(time):GraduationPeriod     +    log(time):state   + 
                  log(time):Same_city_members + log(time):Same_degree_year_members  +
                  log(time):Type_membership + log(time):Degree_level ,data=data_in, method="breslow")
summary(coxpht)

cox.zph(coxph1)
## Test the PH assumption  
a <- cox.zph(coxph1)

plot(a[1])

plot(a[3])

plot(a[5])

plot(a[8])
plot(a[9])
plot(a[10])

data_in$Agesr <- data_in$Age^2  

lfit <- aareg(Surv(time,event) ~   Age   +          
                Ethnicity    +    Gender    +              
                GraduationPeriod     +    state   + 
                Same_city_members + Same_degree_year_members  +
                Type_membership  ,data=data_in,
              nmin=1)

summary(lfit)

plot(lfit[1])
plot(lfit[2])
plot(lfit[3])
plot(lfit[4])
plot(lfit[5])
plot(lfit[6])
plot(lfit[7])
plot(lfit[8])
plot(lfit[9])
plot(lfit[10])
plot(lfit[11])
plot(lfit[12])


# Exponential, Weibull, and log-logistic parametric model coefficients

exponential <- survreg(Surv(time,event) ~ Age  +           
                         Ethnicity    +    Gender    +              
                         GraduationPeriod     +    state   + 
                         Same_city_members + Same_degree_year_members  +
                         Type_membership + Degree_level,data=data_in, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(time,event) ~  Age  +           
                     Ethnicity    +    Gender    +              
                     GraduationPeriod     +    state   + 
                     Same_city_members + Same_degree_year_members  +
                     Type_membership + Degree_level,data=data_in, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(time,event) ~ Age  +           
                         Ethnicity    +    Gender    +              
                         GraduationPeriod     +    state   + 
                         Same_city_members + Same_degree_year_members  +
                         Type_membership + Degree_level  ,data=data_in, dist="loglogistic")
summary(loglogistic)

extractAIC(exponential)
extractAIC(weibull)
extractAIC(loglogistic)

loglogistic2 <- survreg(Surv(time,event) ~ Age  +  Agesr +        
                         Ethnicity    +    Gender    +              
                         GraduationPeriod     +    state   + 
                         Same_city_members + Same_degree_year_members  +
                         Type_membership + Degree_level  ,data=data_in, dist="loglogistic")

summary(loglogistic2)
extractAIC(loglogistic2)
## Function from http://www.r-statistics.com/2013/07/creating-good-looking-survival-curves-the-ggsurv-function/ 

ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'black', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = F, xlab = 'Time',
                   ylab = 'Survival probability', main = ''){
  
  library(ggplot2)
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  
  ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'black', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival probability', main = ''){
    
    dat <- data.frame(time = c(0, s$time),
                      surv = c(1, s$surv),
                      up = c(1, s$upper),
                      low = c(1, s$lower),
                      cens = c(0, s$n.censor))
    dat.cens <- subset(dat, cens != 0)
    
    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
    
    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(col = col, lty = lty.est)
    
    pl <- if(CI == T | CI == 'def') {
      pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
        geom_step(aes(y = low), color = col, lty = lty.ci)
    } else (pl)
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  
  ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                       cens.col = 'red', lty.est = 1, lty.ci = 2,
                       cens.shape = 3, back.white = F, xlab = 'Time',
                       ylab = 'Survival probability', main = '') {
    n <- s$strata
    
    groups <- factor(unlist(strsplit(names
                                     (s$strata), '='))[seq(2, 2*strata, by = 2)])
    gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
    
    for(i in 1:strata){
      gr.df[[i]] <- data.frame(
        time = c(0, s$time[ ind[[i]] ]),
        surv = c(1, s$surv[ ind[[i]] ]),
        up = c(1, s$upper[ ind[[i]] ]),
        low = c(1, s$lower[ ind[[i]] ]),
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], n[i] + 1))
    }
    
    dat <- do.call(rbind, gr.df)
    dat.cens <- subset(dat, cens != 0)
    
    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(aes(col = group, lty = group))
    
    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}
    
    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
          geom_step(aes(y = low, color = group), lty = lty.ci)
      } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
          geom_step(aes(y = low,lty = group), col = surv.col)}
    } else {pl}
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0){
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0){
      stop ('There are no censored observations')
    } else(pl)
    
    pl <- if(back.white == T) {pl + theme_bw()
    } else (pl)
    pl
  }
  pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                  cens.col, lty.est, lty.ci,
                                  cens.shape, back.white, xlab,
                                  ylab, main)
  } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                   cens.col, lty.est, lty.ci,
                   cens.shape, back.white, xlab,
                   ylab, main)}
  return(pl)
}




