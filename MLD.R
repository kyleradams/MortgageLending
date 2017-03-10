# Kyler Adams

#Sample R Program for Estimating Binary Response Models

#Remove objects (data) from your workspace
rm(list=ls(all=TRUE))

#Set working directory by clicking on Session --> Set Working Directory --> To Source File Location

MLD <- read.csv("MLD Data File.csv", header=TRUE)  # import data

#Print variable names on the screen
colnames(MLD)

#Install Packages if necessary
#install.packages("aod")
library(aod)
library(ggplot2)
library(Rcpp)

#Generate Descriptive Statistics
summary(MLD) #take a careful look -- there are some problems

#problem 1: GDLIN has 2 entries of 666 in it. when it should be only 0 or 1
#problem 2: MARRIED has 3 missing values, entered as periods
#problem 3: MALE has 15 missing values, entered as periods
#problem 4: LOANPRC has some values over 1, which seem unintuitive. at least 30 values.
#also has some very low values such as .02. 2% loan on a house when you pay 98% down pmt seems kind of strange


#Impose appropriate sample selection criteria here
#criteria 1: only select obs rows with GDLIN = 0 or 1
#criteria 2: only select obs rows with MARRIED not equal to '.'
#criteria 3: only select obs rows with MALE not equal to '.'
#resulting subset has removed 20 observations
MLD1<- MLD[which((MLD$GDLIN == 0 | MLD$GDLIN ==1) & (MLD$MARRIED != '.') & (MLD$MALE != '.')), ]

#to deal with factor versus numeric issue
MLD1[,1] <- as.numeric(MLD1[,1])-2
MLD1[,6] <- as.numeric(MLD1[,6])-2
# MLD1$OBRAT <- MLD1$OBRAT/100
MLD1$LOANPRC <- MLD1$LOANPRC*100

#summary stats!
summary(MLD1)

#getting standard deviations over all columns
apply(MLD1, 2, sd)

#subsetting black and repeat
MLDBLACK <- MLD1[which(MLD1$BLACK == 1),]
summary(MLDBLACK)
apply(MLDBLACK, 2, sd)

#subsetting hispanic and repeat
MLDHISPAN <- MLD1[which(MLD1$HISPAN == 1),]
summary(MLDHISPAN)
apply(MLDHISPAN, 2, sd)

#subsetting white and repeat
MLDWHITE <- MLD1[which(MLD1$HISPAN == 0 & MLD1$BLACK == 0),]
summary(MLDWHITE)
apply(MLDHISPAN, 2, sd)

mldtest <- MLD1[which(MLD1$HISPAN == 1 & MLD1$BLACK == 0 & MLD1$GDLIN == 1 & MLD1$MARRIED == 1),]
summary(mldtest)
#Estimate Logit Model
LogitModel = glm(APPROVE ~., data = MLD1, 
                 family = "binomial")
summary(LogitModel)

# Male is not significant, re-estimate Logit Model
LogitModel = glm(APPROVE ~ MARRIED + GDLIN + OBRAT + BLACK + HISPAN + LOANPRC, data = MLD1, 
                 family = "binomial")
summary(LogitModel)

#Generate Odds Ratios
oddsRatios <- exp(coef(LogitModel))

#Estimate Probit Model
ProbitModel = glm(APPROVE ~ ., data = MLD1, 
                  family = "binomial" (link = "probit"))
summary(ProbitModel)

#Similarly, MALE is not significant, re-estimate Probit Model
ProbitModel = glm(APPROVE ~ MARRIED + GDLIN + OBRAT + BLACK + HISPAN + LOANPRC, data = MLD1, 
                  family = "binomial" (link = "probit"))
summary(ProbitModel)

#Define 6 prototypical loan applicants
prototype1 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 1, HISPAN = 0, MARRIED = 0, GDLIN = 1)
prototype2 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 0, HISPAN = 1, MARRIED = 0, GDLIN = 1)
prototype3 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 0, HISPAN = 0, MARRIED = 0, GDLIN = 1)
prototype4 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 1, HISPAN = 0, MARRIED = 0, GDLIN = 0)
prototype5 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 0, HISPAN = 1, MARRIED = 0, GDLIN = 0)
prototype6 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 0, HISPAN = 0, MARRIED = 0, GDLIN = 0)
prototype7 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 1, HISPAN = 0, MARRIED = 1, GDLIN = 1)
prototype8 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 0, HISPAN = 1, MARRIED = 1, GDLIN = 1)
prototype9 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 0, HISPAN = 0, MARRIED = 1, GDLIN = 1)
prototype10 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 1, HISPAN = 0, MARRIED = 1, GDLIN = 0)
prototype11 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 0, HISPAN = 1, MARRIED = 1, GDLIN = 0)
prototype12 <- data.frame(OBRAT=mean(MLD1$OBRAT), LOANPRC=mean(MLD1$LOANPRC), BLACK = 0, HISPAN = 0, MARRIED = 1, GDLIN = 0)


# Predict probabilities for prototypical individuals
# newdata - An optional data frame in which to look for variables with which to predict. If omitted, the fitted values are used.
# prototype1$predicted_logit <- predict (LogitModel, newdata = prototype1, type ="response")
# prototype2$predicted_logit <- predict (LogitModel, newdata = prototype2, type ="response")
# prototype3$predicted_logit <- predict (LogitModel, newdata = prototype3, type ="response")
# prototype4$predicted_logit <- predict (LogitModel, newdata = prototype4, type ="response")
# prototype5$predicted_logit <- predict (LogitModel, newdata = prototype5, type ="response")
# prototype6$predicted_logit <- predict (LogitModel, newdata = prototype6, type ="response")
# prototype7$predicted_logit <- predict (LogitModel, newdata = prototype7, type ="response")
# prototype8$predicted_logit <- predict (LogitModel, newdata = prototype8, type ="response")
# prototype9$predicted_logit <- predict (LogitModel, newdata = prototype9, type ="response")
# prototype10$predicted_logit <- predict (LogitModel, newdata = prototype10, type ="response")
# prototype11$predicted_logit <- predict (LogitModel, newdata = prototype11, type ="response")
# prototype12$predicted_logit <- predict (LogitModel, newdata = prototype12, type ="response")

prototype1$predicted_probit <- predict (ProbitModel, newdata = prototype1, type ="response")
prototype2$predicted_probit <- predict (ProbitModel, newdata = prototype2, type ="response")
prototype3$predicted_probit <- predict (ProbitModel, newdata = prototype3, type ="response")
prototype4$predicted_probit <- predict (ProbitModel, newdata = prototype4, type ="response")
prototype5$predicted_probit <- predict (ProbitModel, newdata = prototype5, type ="response")
prototype6$predicted_probit <- predict (ProbitModel, newdata = prototype6, type ="response")
prototype7$predicted_probit <- predict (ProbitModel, newdata = prototype7, type ="response")
prototype8$predicted_probit <- predict (ProbitModel, newdata = prototype8, type ="response")
prototype9$predicted_probit <- predict (ProbitModel, newdata = prototype9, type ="response")
prototype10$predicted_probit <- predict (ProbitModel, newdata = prototype10, type ="response")
prototype11$predicted_probit <- predict (ProbitModel, newdata = prototype11, type ="response")
prototype12$predicted_probit <- predict (ProbitModel, newdata = prototype12, type ="response")

# Results
prototype1
prototype2
prototype3
prototype4
prototype5
prototype6
prototype7
prototype8
prototype9
prototype10
prototype11
prototype12

# stargazer results
library(stargazer)
stargazer(LogitModel, type = "text")
stargazer(ProbitModel, type = "text")

summary(LogitModel)
summary(ProbitModel)

# Use this for logit/probit
stargazer(LogitModel, ProbitModel, type = "text", out = "models.html")
# Use this for Odds Ratio **** Make sure to take out SEs
stargazer(LogitModel, apply.coef = exp, type = "text", out = "or.html")

stargazer(prototype1, type = "text")




## chi2-test on Marriage and Male variableS
LogitModel1 = glm(APPROVE ~., data = MLD1, 
                 family = "binomial")
summary(LogitModel1)

LogitModel2 = glm(APPROVE ~ GDLIN + OBRAT + BLACK + HISPAN + LOANPRC, data = MLD1, 
                 family = "binomial")
summary(LogitModel2)

ProbitModel1 = glm(APPROVE ~ ., data = MLD1, 
                  family = "binomial" (link = "probit"))
summary(ProbitModel1)

ProbitModel2 = glm(APPROVE ~ GDLIN + OBRAT + BLACK + HISPAN + LOANPRC, data = MLD1, 
                  family = "binomial" (link = "probit"))
summary(ProbitModel2)

# Slightly high p-values but still under 5%
anova(LogitModel1, LogitModel2, test="Chisq")
anova(ProbitModel1, ProbitModel2, test="Chisq")

# Stargazer
stargazer(LogitModel, LogitModel2, ProbitModel, ProbitModel2, type = "text")
#=========#
#== END ==#
#=========#