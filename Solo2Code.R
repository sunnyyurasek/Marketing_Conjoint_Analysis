
setwd("C:/Users/syurasek/OneDrive - Constellation Brands/Documents/Northwestern/PREDICT 450/Solo2")
load("stc-cbc-respondents-v3(1).RData")
library(pROC)
library(knitr)
library(ggplot2)
require(dummies)
require(bayesm)

#simple effect coding
contr.sum(4)

###Data Load###
str(resp.data.v3)
head(resp.data.v3)
taskV3 <- read.csv("stc-dc-task-cbc -v3(1).csv", sep=",")
str(taskV3)
head(taskV3)

load("efCode.RData")
str(efcode.att.f)
str(efcode.attmat.f)

###Task Table Creation###
apply(resp.data.v3[4:39], 2, function(x){tabulate(na.omit(x))}) 
task.mat <- as.matrix(taskV3[, c("screen", "RAM", "processor", "price", "brand")])
dim(task.mat)
head(task.mat)
task.mat

###Effect Coding###
X.mat=efcode.attmat.f(task.mat)  
dim(X.mat)
head(X.mat)

#price variable creation
pricevec=taskV3$price-mean(taskV3$price)
head(pricevec)
str(pricevec)

#brand varaible creation
X.brands=X.mat[,9:11]
dim(X.brands)
str(X.brands)

#brand and price varaible creation
X.BrandByPrice = X.brands*pricevec
dim(X.BrandByPrice)
str(X.BrandByPrice)

#matrix combined for a new table
X.matrix=cbind(X.mat,X.BrandByPrice)
dim(X.matrix)
str(X.matrix)
head(X.matrix)
X2.matrix=X.matrix[,1:2]
dim(X2.matrix)
det(t(X.matrix) %*% X.matrix)

###Y Response Variable Table Creation###
ydata=resp.data.v3[,4:39]
names(ydata)
str(ydata)
head(ydata)
ydata=na.omit(ydata)
ydatadf <- ydata
head(ydatadf)
ydata=as.matrix(ydata)
dim(ydata)
zowner <- 1 * ( ! is.na(resp.data.v3$vList3) )
head(zowner)

###Data Preparation for rhierMnlDP###
lgtdata = NULL
for (i in 1:424) { lgtdata[[i]]=list( y=ydata[i,],X=X.matrix )}
length(lgtdata)
str(lgtdata)
lgtdata[[3]]

###Test Run for rhierMnlDP###
### Set parameters
resp.count <- 1:424
iterations <- 10000
post.burndown <- 501:2000
ndraws <- iterations/5
### Set parameters

lgtdata.model <- lgtdata[resp.count]
mcmctest <- list(R=iterations, keep=5) # specify every 5th sample is kept
Data1 <- list(p=3,lgtdata=lgtdata.model) # create the "Data" list rhierMnlDP() expects; p is choice set size
testrun1 <- rhierMnlDP(Data=Data1,Mcmc=mcmctest)
names(testrun1)
betadraw1 <- testrun1$betadraw  # array that has the draws (i.e. samples from marginal posterior distributions) for the regression coefficients
dim(betadraw1)

for (j in 1:6) {
  jpeg(paste0("Beta",j,".jpeg"), width = 900, height = 600, quality = 100)
  par(mfrow = c(3,2))
  for (i in 1:6) {
    plot(1:length(betadraw1[i,j,]),betadraw1[i,j,], xlab = "Index", ylab = "Beta Est.", main = paste("Beta",j,"Respondent",i)) # "Index" is the iteration number after "thinning."
    abline(v=5000, col = "red") ## vertical line
    abline(v=10000, col = "red") ## vertical line?jpb
  }
  dev.off()
}

plot(density(betadraw1[1,1,post.burndown],width=2)) # look at distribution
summary(betadraw1[1,1,post.burndown])
mn <- mean(betadraw1[1,1,post.burndown])
sd <- sd(betadraw1[1,1,post.burndown])
mn
sd
abline(v=0, col="red") ## vertical line
abline(v=mn, col="red") ## vertical line
prob <- pnorm(0,mean=mn, sd=sd, lower.tail = FALSE)
prob # compute the probability that person 1 beta 1 > 0

for (j in 1:6) {
  jpeg(paste0("BetaDistr",j,".jpeg"), width = 900, height = 400, quality = 100)
  par(mfrow = c(2,3))
  for (i in 1:6) {
    plot(density(betadraw1[i,j,post.burndown],width=2), xlim = c(-4,4), main = paste("Beta",j,"Respondent",i)) # look at distribution
    mn <- mean(betadraw1[i,j,post.burndown])
    abline(v=mn, col="red") ## vertical line
  }
  dev.off()
}

# overall means of the coefficients across respondents
betameansoverall <- apply(betadraw1[,,post.burndown],c(2),mean) 
betameansoverall
plot(betameansoverall, main="Mean Betas for Model 1 over all respondents")
abline(h=0, lty=2)

# Summary table of the overall means of the coefficient
df_meanBetasall <- data.frame(betameansoverall)
colnames(df_meanBetasall) <- c("Beta Mean All")

df_meanBetasall <- round(df_meanBetasall, 3)
df_meanBetasall$Attribute <- c("7 inch Screen", "10 inch Screen",
                            "16Gb RAM", "32Gb RAM",
                            "2GHz Processor", "2.5Ghz Processor",
                            "$299", "$399",
                            "Somesong Brand", "Pear Brand", "Gaggle Brand",
                            "Somesong Brand by Price", "Pear Brand by Price", "Gaggle Brand by Price")
df_meanBetasall <- df_meanBetasall[, c("Attribute",
                                 "Beta mean")]
kable(df_meanBetasall)

#sample 3rd individual respondent coefficents
round(apply(betadraw1[3, , post.burndown], 1, 
            quantile,probs=c(0.05,0.50,0.95)),3) 

# matrix of coefficient means by respondent
betameansindividual <- apply(betadraw1[,,post.burndown],c(1,2),mean) 
betameansindividual
plot(betameansindividual, main="Mean Betas for Model 1 by individual respondents")
abline(h=0, lty=2)

betameansoverall <- apply(betadraw1[,,post.burndown],c(2),mean) 
betameansoverall
plot(betameansoverall, main="Mean Betas for Model 1 over all respondents")
abline(h=0, lty=2)

# Summary table of the individual means of the coefficient
df_meanBetasindividual <- data.frame(betameansindividual)
colnames(df_meanBetasindividual) <- c("Beta Mean Individual")

df_meanBetasindividual <- round(df_meanBetasall, 3)
df_meanBetasindividual$Attribute <- c("7 inch Screen", "10 inch Screen",
                               "16Gb RAM", "32Gb RAM",
                               "2GHz Processor", "2.5Ghz Processor",
                               "$299", "$399",
                               "Somesong Brand", "Pear Brand", "Gaggle Brand",
                               "Somesong Brand by Price", "Pear Brand by Price", "Gaggle Brand by Price")
df_meanBetasindividual <- df_meanBetasindividual[, c("Attribute",
                                       "Beta mean")]
kable(df_meanBetasindividual)

# Calculate measures of fit
xbeta <- X.matrix%*%t(betameansindividual) 
xbeta2 <- matrix(xbeta,ncol=3,byrow=TRUE)
expxbeta2 <- exp(xbeta2)

# get predicted choice probabilities
rsumvec <- rowSums(expxbeta2)# Divide each row by its sum to get predicted choice probabilities
pchoicemat <- expxbeta2/rsumvec # predicted choice probabilities
head(pchoicemat)
dim(pchoicemat)
custchoice <- max.col(pchoicemat) # prediction of customer choice
head(custchoice)
str(custchoice)
rm(xbeta, xbeta2, expxbeta2, rsumvec)

round(apply(df_pchoicemat, 2, quantile, 
            probs=c(0.10,0.25,0.5,0.75,0.90)), 4)
rm(df_pchoicemat)

### Model Fitness Assessment
ydatavec <- as.vector(t(ydata))
str(ydatavec)
table(custchoice,ydatavec[1:15264]) # provides confusion matrix
roctest <- roc(ydatavec[1:15264], custchoice, plot=TRUE) # ROC curve
auc(roctest) # Area Under the Curve
logliketest <- testrun1$loglike # -2log(likelihood) test applies to nested models only
str(logliketest)
mean(logliketest)
par(mfrow = c(1,1))
hist(logliketest, main = "Model 1 - Log Likelihood Histogram-AUC 0.8534")


# predict the choices for the 36 choice sets
m <- matrix(custchoice, nrow =36, byrow=F)
m2 <- t(m)
predicted1 <- apply(m2, 2, function(x){tabulate(na.omit(x))}) 
write.csv(predicted1, "predicted1.csv")
hist(predicted1)

percent <- apply(betadraw1[,,post.burndown],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
write.csv(percent,"table1.csv")
percent

plot(density(betadraw1[1,1,post.burndown]-betadraw1[1,2,post.burndown],width=2))

# Compute chains of mean betas
# Returns 14xn matrix, of coefficient means for each draw (based on R / keep draws)
betaMeanChains <- apply(betadraw1, c(2:3), mean) 
print(round(betaMeanChains, 3))

# Plot comparison of chains of mean betas between two attributes for each draw
plot(betaMeanChains[, c(1:2)], main="")
abline(h=0, lty=2)


# Estimate the distribution of the differences between the 1'st respondents 7th and 8th coefficients over the last 200 draws
summary(betadraw1[1, 7,(ndraws-200):ndraws]-betadraw1[1, 8, (ndraws-200):ndraws])
plot(density(betadraw1[1, 7,(ndraws-200):ndraws]-betadraw1[1, 8, (ndraws-200):ndraws], width=2.5))
abline(v=mean(betadraw1[1, 7,(ndraws-200):ndraws]-betadraw1[1, 8, (ndraws-200):ndraws]), lty=2)

# Generate plot of beta means over draws to validate iterations validity
library(reshape)
betaMeanChains.t <- t(betaMeanChains)
dimnames(betaMeanChains.t) <- list(NULL, paste("mbeta", c(1:14), sep=""))

meltedbetas <- melt(betaMeanChains.t, varnames=c("draw", "beta"), value.name="mean.beta")

ggplot(meltedbetas, aes(draw, value)) + 
  geom_line(aes(group=beta, colour=beta)) +
  labs(x='Draw', y='Beta value') +
  scale_x_continuous(breaks = round(seq(0, 3000, by=500), 1)) +
  theme(legend.position='none') +
  ggtitle("Model 1 betadraws by Number Draws ")

# Generate plot of log likelihood measures over draws
loglike1 <- data.frame(draw=1:length(testrun1$loglike), 
                          loglike=testrun1$loglike)

ggplot(loglike1) + 
  geom_line(aes(y=loglike, x=draw)) +
  labs(x='Draw', y='Model 1 Log Likelihood')+
  ggtitle("Model 1 Log Likelihood by Number Draws ")

# Find number of draws which fall outside of percentile range
getp.f <- function(x, y=0){
  pfcn <- ecdf(x)
  return(pfcn(y))
}

for (i in 12:14){
  betamat <- betadraw1[,i,(ndraws-200):ndraws]
  zp <- apply(betamat,1,getp.f)
  
  betaDiffZero <- rep(0,nrow(betamat))
  betaDiffZero[zp <= 0.05 | zp >= 0.95] = 1
  respDiffBetas <- betamat[betaDiffZero == 1,]
  
  obsDiff <- as.numeric(dim(respDiffBetas))[1] 
  obsTot <- as.numeric(dim(respDiffBetas))[2]
  print(paste(obsDiff, obsTot, obsDiff/obsTot))
}

rm(betamat, zp, betaDiffZero, respDiffBetas, obsDiff, obsTot)

### compute the empirical probability based on sample values
p1b1 <- betadraw1[1,1,post.burndown]
p1b1df <- as.data.frame(p1b1)
str(p1b1df)
tab <- table(sign(p1b1df$p1b1))
str(tab)
tabdf <- as.data.frame(tab)
tabdf$Pct <- tabdf$Freq/sum(tabdf$Freq)
tabdf$Pct

table(testrun1$Istardraw) # 3 is the mode


#################################################
#Model 2. Create a model with the ownership#
#################################################

zownertest <- matrix(scale(zowner[resp.count],scale=FALSE),ncol=1)
Data2 <- list(p=3, lgtdata = lgtdata.model, Z = zownertest)
testrun2 <- rhierMnlDP(Data = Data2, Mcmc = mcmctest)
names(testrun2)
dim(testrun2$Deltadraw)
apply(testrun2$Deltadraw[post.burndown,],2,mean) 
deltadrawdistr <- apply(testrun2$Deltadraw[post.burndown,],2,quantile,probs=c(0.95,0.90,0.75,0.5 ,0.25,0.10,0.05))
deltadrawdistr
write.csv(deltadrawdistr, "deltadrawdistr.csv")
betadraw2 <- testrun2$betadraw
dim(betadraw2)

betameansoverall2 <- apply(betadraw2[,,post.burndown],c(2),mean) # overall means of the coefficients across respondents
betameansoverall2
plot(betameansoverall2, main="Mean Betas for Model 2 by all respondents")
abline(h=0, lty=2)

betameansindividual2 <- apply(betadraw2[,,post.burndown],c(1,2),mean) # matrix of coefficient means by respondent
betameansindividual2
summary((betadraw2[1,1,post.burndown]-betadraw2[1,2,post.burndown])) # calculate a distribution for the difference between respondent 1's 1st and 2nd coefficients (these are for the screen attribute)
plot(betameansindividual2, main="Mean Betas for Model 2 by individual respondents")
abline(h=0, lty=2)

percent <- apply(betadraw2[,,post.burndown],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
write.csv(percent,"table2.csv")
percent

# Summary table of the overall means of the coefficient
df_meanBetasall2 <- data.frame(betameansoverall2)
colnames(df_meanBetasall2) <- c("Beta Mean All")

df_meanBetasall2 <- round(df_meanBetasall2, 3)
df_meanBetasall2$Attribute <- c("7 inch Screen", "10 inch Screen",
                               "16Gb RAM", "32Gb RAM",
                               "2GHz Processor", "2.5Ghz Processor",
                               "$299", "$399",
                               "Somesong Brand", "Pear Brand", "Gaggle Brand",
                               "Somesong Brand by Price", "Pear Brand by Price", "Gaggle Brand by Price")
df_meanBetasall2 <- df_meanBetasall2[, c("Attribute",
                                       "Beta mean")]
kable(df_meanBetasall2)

#################################################################################################
###Predicted choice probabilities for Model 2  - Covariate (Individual Beta Means) #
#################################################################################################

dim(betameansindividual2) # matrix with subjects in the rows and means in the columns
dim(t(betameansindividual2))
dim(X.matrix)
xbeta <- X.matrix%*%t(betameansindividual2)
dim(xbeta)
xbeta2 <- matrix(xbeta,ncol=3,byrow=TRUE) # reading values column by column and putting them into rows of new matrix with 3 columns
dim(xbeta2) 
expxbeta2 <- exp(xbeta2) # exponentiate
dim(expxbeta2)

### get predicted choice probabilities
rsumvec <- rowSums(expxbeta2)
pchoicemat <- expxbeta2/rsumvec # predicted choice probabilities
head(pchoicemat)
dim(pchoicemat)
custchoice <- max.col(pchoicemat) # prediction of customer choice
head(custchoice)
str(custchoice)

### assess model fit
ydatavec <- as.vector(t(ydata))
str(ydatavec)
table(custchoice,ydatavec[1:15264]) # provides confusion matrix
roctest <- roc(ydatavec[1:15264], custchoice, plot=TRUE) # ROC curve
auc(roctest) # Area Under the Curve
logliketest <- testrun2$loglike # -2log(likelihood) test applies to nested models only
str(logliketest)
mean(logliketest)
par(mfrow = c(1,1))
hist(logliketest, main = "Model 2 - Log Likelihood Histogram AUC 0.8533")

# predict the choices for the 36 choice sets
m <- matrix(custchoice, nrow =36, byrow=F)
m2 <- t(m)
predicted2 <- apply(m2, 2, function(x){tabulate(na.omit(x))}) 
write.csv(predicted2, "predicted2.csv") 

#############################################################################################
###Predicted choice probabilities for Model 2 - Covariate (Overall Beta Means) #
#############################################################################################

betavec=matrix(betameansoverall2,ncol=1,byrow=TRUE)
xbeta=X.matrix%*%(betavec)
dim(xbeta)
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)
expxbeta2=exp(xbeta2)
rsumvec=rowSums(expxbeta2)
pchoicemat=expxbeta2/rsumvec
pchoicemat
### Predicted frequencies based on overall model
pchoicematoverall2 <- round(pchoicemat*424,digits=0)
pchoicematoverall2
write.csv(pchoicematoverall2, "predictoverall1.csv")

custchoice <- max.col(pchoicematoverall2) # prediction of customer choice
head(custchoice)
str(custchoice)

### assess model fit
ydatavec <- as.vector(t(ydata))
str(ydatavec)
table(c(rep(custchoice,424)),ydatavec[1:15264]) # provides confusion matrix
roctest <- roc(ydatavec[1:15264], c(rep(custchoice,424)), plot=TRUE) # ROC curve
auc(roctest) # Area Under the Curve


###########################################################
##Model 3 - evaluation of extra scenarios ######
###################################################################
ex_scen <- read.csv("extra-scenarios(1).csv")
Xextra.matrix <- as.matrix(ex_scen[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9",
                                      "V10","V11","V12","V13","V14")])

### predict extra scenarios using the overall model  ###################
betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)
xextrabeta=Xextra.matrix%*%(betavec)
dim(xextrabeta)
xbetaextra2=matrix(xextrabeta,ncol=3,byrow=TRUE)
dim(xbetaextra2)

expxbetaextra2=exp(xbetaextra2)
rsumvec=rowSums(expxbetaextra2)
pchoicemat=expxbetaextra2/rsumvec
pchoicemat

### predict extra scenarios based on individual models #############################

xextrabetaind=Xextra.matrix%*%(t(betameansindividual))
xbetaextra2ind=matrix(xextrabetaind,ncol=3,byrow=TRUE)

#xextrabetaind=Xextra.matrix%*%(t(betameansindividual))
#dim(xextrabetaind)
#xbetaextra2ind=rbind(matrix(xextrabetaind[1:3,],ncol=3,byrow=TRUE),
#                     matrix(xextrabetaind[4:6,],ncol=3,byrow=TRUE))
dim(xbetaextra2ind)


expxbetaextra2ind=exp(xbetaextra2ind)
rsumvecind=rowSums(expxbetaextra2ind)
pchoicematind=expxbetaextra2ind/rsumvecind
dim(pchoicematind)
head(pchoicematind)


custchoiceind <- max.col(pchoicematind)
head(custchoiceind)
str(custchoiceind)
extra1 <- custchoiceind[1:424]
extra2 <- custchoiceind[425:848]
table(extra1)
table(extra2)


################################################################################################
#accuracy based on confusion matrix for each of the 424 respondents using individual models
############ for all the 36 choice sets - actual response vs predicte response ###############
###############################################################################################
resp_accuracy <- NULL
for (i in 1:424) {
  start <- i*36-35
  end <- i*36
  d <- table(factor(custchoicedf[start:end],levels = 1:3),
             factor(ydatavec[start:end], levels = 1:3))
  resp_accuracy[i] <- sum(diag(d))/sum(d)
} 
plot(resp_accuracy, main = "Model Accuracy by Respondent")
respdf <- data.frame(resp_accuracy)
head(respdf)
str(respdf)
head(ydatadf)
rn <- rownames(ydatadf)
rndf <- as.data.frame(rn)
resp_all <- cbind(rndf,respdf)
head(resp_all)

str(resp_all)
hist(resp_all$resp_accuracy)
outlier <- subset(resp_all, resp_accuracy < 0.6)
outlier[order(outlier$resp_accuracy),]
#############################################################


#############################################################
#########################################################



