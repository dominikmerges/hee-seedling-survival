###############################################
##Mixed Model of seedling survival using MASS
###############################################

library(MASS)

survival = read.csv('seedling_survival.csv',header=TRUE)
survival$Species[78] = "B"
survival$Species[469] = "B"
survival$Species = factor(survival$Species)

#Stupid corrections
survival$Surv5[which(survival$Surv5=="?")] = 0
survival$Surv5[which(survival$Surv5=="")] = NA
survival$Surv5 = as.numeric(survival$Surv5)
survival$Surv5[which(survival$Surv5==3)] = 1
survival$Surv5[which(survival$Surv5==2)] = 0
for (i in 1:1841){
  for (j in 9:13){
    if(is.na(survival[i,j])){
      survival[i,j] = 0
    }
  }
}

attach(survival)


model = glmmPQL(Surv5 ~ trt + plno + Species + Age + Age*plno, random = ~1 | as.factor(Unit), family=binomial)

summary(model)