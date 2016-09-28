#-------------------------------------------------------------------------------------------------------
#
#  Example R code to read in ultrasound data and perform some basic plotting
#
#--------------------------------------------------------------------------------------------------------



# Optionally, install and load the 'hbgd' package to access tools for calculating 
# Z-scores and for fitting growth trajectories

install.packages("hbgd", repos = c(
 CRAN    = "http://cran.rstudio.org",
 tessera = "http://packages.tessera.io")
)

library(hbgd)
library(Hmisc)
library(ggplot2)



#-----------------------------------------------------------------------
# Read and prep data
#-----------------------------------------------------------------------

# Read training data  (45% of data was reserved for testing)
dat0 <- read.csv("..//adam//training_ultrasound.csv")
contents(dat0)
head(dat0)


# Use Hadlock equation to estimate fetal weight from the 4 key ultrasound
# measurements
dat <- within(dat0 ,{
  LOG10.FWT.GM <- 1.3596 + 0.0064*HCIRCM + 0.0424*ABCIRCM + 0.174*FEMURCM + 0.00061*BPDCM*ABCIRCM - 0.00386*ABCIRCM*FEMURCM
  WTKG <- ifelse(AGEDAYS<1 ,(10^LOG10.FWT.GM)/1000 ,WTKG)  
})
head(subset(dat ,!is.na(WTKG) ,c(STUDYID,SUBJID,GAGEDAYS,AGEDAYS,WTKG)) ,n=50)

dat$Study <- with(dat ,paste('Study',STUDYID))





#-----------------------------------------------------------------------
# Spaghetti plots
#-----------------------------------------------------------------------

# Plot abdominal circumference vs gestational age (ultrasound only)
dati <- subset(dat ,!is.na(ABCIRCM))
gp <- ggplot(dati ,aes(x=GAGEDAYS ,y=ABCIRCM ,group=SUBJID ,color=SEX))
gp + geom_line() + facet_wrap(Study ~ SEX ,nrow=2) + 
    theme(legend.position="none")



# Plot head circumference vs birth age in days (ultrasound and post-natal)
dati <- subset(dat ,!is.na(HCIRCM))
gp <- ggplot(dati ,aes(x=AGEDAYS ,y=HCIRCM ,group=SUBJID ,color=SEX))
gp + geom_line() + facet_wrap(Study ~ SEX ,nrow=2) + 
  theme(legend.position="none")



# Plot weight vs birth age in days (estimated from ultrasound and measured post-natal)
dati <- subset(dat ,!is.na(WTKG))
gp <- ggplot(dati ,aes(x=AGEDAYS ,y=WTKG ,group=SUBJID ,color=SEX))
gp + geom_line() + facet_wrap(Study ~ SEX ,nrow=2) + 
  theme(legend.position="none")




#-----------------------------------------------------------------------
# Re-do the spaghetti plots as dot plots to see the visit schedule
#-----------------------------------------------------------------------

dati <- subset(dat ,!is.na(ABCIRCM))
gp <- ggplot(dati ,aes(x=GAGEDAYS ,y=ABCIRCM ,group=SUBJID ,color=SEX))
gp + geom_point() + facet_wrap(Study ~ SEX ,nrow=2) + 
  theme(legend.position="none")


dati <- subset(dat ,!is.na(HCIRCM))
gp <- ggplot(dati ,aes(x=AGEDAYS ,y=HCIRCM ,group=SUBJID ,color=SEX))
gp + geom_point() + facet_wrap(Study ~ SEX ,nrow=2) + 
  theme(legend.position="none")


dati <- subset(dat ,!is.na(WTKG))
gp <- ggplot(dati ,aes(x=AGEDAYS ,y=WTKG ,group=SUBJID ,color=SEX))
gp + geom_point() + facet_wrap(Study ~ SEX ,nrow=2) + 
  theme(legend.position="none")





#-----------------------------------------------------------------------
# Plot the difference between birth weight and estimated weight at 
# gest age 40 vs gest age at birth
#-----------------------------------------------------------------------

dati <- unique(subset(dat ,!is.na(BWT_40) ,c(STUDYID,SUBJID,SEX,GAGEBRTH,BIRTHWT,BWT_40)))
gp <- ggplot(dati ,aes(x=GAGEBRTH ,y=BWT_40-BIRTHWT/1000 ,color=SEX))
gp + geom_point() + geom_hline(yintercept=0) + geom_vline(xintercept=280)

