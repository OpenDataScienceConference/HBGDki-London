#-------------------------------------------------------------------------------------------------------
#
#  Example R code to read in brain myelination data and perform some basic plotting
#
#--------------------------------------------------------------------------------------------------------



library(Hmisc)
library(ggplot2)



#-----------------------------------------------------------------------
# Read and prep data
#-----------------------------------------------------------------------

dat0 <- read.csv("..//adam//ads.csv")
contents(dat0)
head(dat0)

dat <- dat0
dat$Cohort <- with(dat ,paste('Cohort',STRATUM))





#-----------------------------------------------------------------------
# Spaghetti plots
#-----------------------------------------------------------------------

# Plot height vs birth age 
dati <- subset(dat ,!is.na(HTCM))
gp <- ggplot(dati ,aes(x=AGEDAYS ,y=HTCM ,group=SUBJID ,color=SEX))
gp + geom_line() + facet_wrap(SEX ~ Cohort ,nrow=2) + 
    theme(legend.position="none")


# Plot head circumference  vs gestational age 
dati <- subset(dat ,!is.na(HCIRCM))
gp <- ggplot(dati ,aes(x=GAGEDAYS ,y=HCIRCM ,group=SUBJID ,color=SEX))
gp + geom_line() + facet_wrap(SEX ~ Cohort ,nrow=2) + 
  theme(legend.position="none")




#-----------------------------------------------------------------------
# Re-do the spaghetti plots as dot plots so that subjects with only 
# one visit are not lost
#-----------------------------------------------------------------------

# Plot height vs birth age 
dati <- subset(dat ,!is.na(HTCM))
gp <- ggplot(dati ,aes(x=AGEDAYS ,y=HTCM ,group=SUBJID ,color=SEX))
gp + geom_point() + facet_wrap(SEX ~ Cohort ,nrow=2) + 
  theme(legend.position="none")


# Plot head circumference  vs gestational age 
dati <- subset(dat ,!is.na(HCIRCM))
gp <- ggplot(dati ,aes(x=GAGEDAYS ,y=HCIRCM ,group=SUBJID ,color=SEX))
gp + geom_point() + facet_wrap(SEX ~ Cohort ,nrow=2) + 
  theme(legend.position="none")




