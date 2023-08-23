
#############################################################################################################################
#############################################################################################################################
#####                                                                                                                   #####
#####  Appendix S5 to Jucker et al. (2016). Allometric equations for integrating remote sensing imagery                 #####
#####  into forest monitoring programs. Global Change Biology.                                                          #####                                        
#####                                                                                                                   #####
#####  The R code below replicates the data binning appraoch descibed in Jucker et al. (2016). Please cite the          #####
#####  original paper if using this code in a pubblication. The code uses data from the Global Allometric Database      ##### 
#####  which is archived on figshare and avaialbe for download here: https://dx.doi.org/10.6084/m9.figshare.3413539.v1  #####   
#####                                                                                                                   #####
#############################################################################################################################
#############################################################################################################################

# Load data and libraries -------------------------------------------------

#### Libraries
library(devtools)
library(plyr)
library(scales)

#### Data
Data<-read.csv("GlobalAllometricDatabase.csv",header=T)                                 # GlobalAllometricDatabase

#### Functions
source_gist("https://gist.github.com/mrdwab/6424112")                                   # function to perform stratified random sampling (developed by Ananda Mahto; available through GitHub: https://github.com/mrdwab)
RMSE<-function(Obs,Pred){sqrt(mean((Obs-Pred)^2))}                                      # function to calculate Root Mean Square Error (RMSE) of model predictions
bias<-function(Obs,Pred){mean((Pred-Obs)/Obs)*100}                                      # function to calculate average systematic bias (%) of model predictions
relative_err<-function(Obs,Pred){((Pred-Obs)/Obs)*100}                                  # function to calculate the relative error (%) of each model prediction 
CV<-function(Obs,Pred){sqrt(sum((Pred-Obs)^2)/(length(Obs)))/mean(Obs)*100}             # function to calculate the tree-level coefficient of variation of the model
sigma_v<-function(Obs,Pred){sqrt(sum((log(Obs)-log(Pred))^2)/(length(Obs)-2))}          # function to calculate the residual standard deviation 
Prediction_interval<-function(t_crit,sigma_v,n,x_new,x_mean,x_sd){                      # function to calculate prediction intervals
  t_crit * sigma_v * sqrt(1 + 1/n + (((x_new-x_mean)^2)/((n-1)*x_sd^2)))
}

# Data binning appraoch and comparison with model fit to raw data ------------------------------------

#### Define the compound variable height x crown diameter and assign trees to log-scale diameter classes
Data$H_CD<-Data$H*Data$CD
Data$D_class<- cut(log(Data$D), breaks=hist(log(Data$D),
                    breaks=seq(min(log(1)),max(log(Data$D)),len=50),plot=F)$breaks)

#### Data binning procedure 

## Create empty dataframes to store model fit statistics for each randomization step
reps<-100
Raw_data_model<-data.frame(matrix(nrow=reps, ncol=6)) 
colnames(Raw_data_model)[1:6]<-c("alpha","beta","sigma","RMSE","bias","CV") 
Binned_data_model<-data.frame(matrix(nrow=reps, ncol=7))  
colnames(Binned_data_model)[1:7]<-c("alpha","beta","sigma","sigma_v","RMSE","bias","CV") 

## Create empty plot to visualize model errors for each randomization step
plot(1,1,log="x", pch=16, col="white",bty="l",xlim=c(1,200),ylim=c(-100,100),
     xlab="Observed diameter (cm)",ylab="Error (%)",las=1,cex.axis=0.8,cex.lab=0.9)
grid(equilogs=FALSE,lty=2,col="grey80",lwd=0.5)
abline(h=0,col="grey20",lty=2)
legend("bottomleft",lwd=2,col=c("forestgreen","grey20"),
       c("Model fit to raw data","Model fit to binned data"),bty="n",cex=0.8)

for (i in 1:reps){
  
  #### Set up validation and model datasets
  Validation_data<-stratified(Data,c("Biome","D_class"),size=0.10)                       # set aside 10% of the data for validation (based on a size-stratified random samplign appraoch)
  Model_trees<-data.frame(Tree_id=setdiff(Data$Tree_id,Validation_data$Tree_id))
  Data_model<-merge(Data,Model_trees)                                                    # data for model fitting (90% of trees)
  
  #### Data binning: calculate mean values per size class for each allometric component
  Data_bin <- ddply(Data_model, .(D_class),summarise,
                    N_trees =length(D),
                    D  = mean(D),
                    H  = mean(H),
                    CD = mean(CD),
                    H_CD= mean(H_CD))
  Data_bin<-Data_bin[Data_bin$N_trees>2,]
  
  #### Fit models and generate predictions
  
  ## Model 1: model fit to raw data
  M1<-lm(log(D)~log(H_CD),data=Data_model)
  Validation_data$D_pred_M1<-exp(predict(M1,Validation_data))*exp(summary(M1)$sigma^2/2)
  Validation_data$Error_M1<-relative_err(Validation_data$D,Validation_data$D_pred_M1)
  lines(smooth.spline(Validation_data$Error_M1 ~ Validation_data$D,nknots=5),col=alpha("forestgreen",0.25),lwd=0.5)
  
  ## Model 2: model fit to binned data
  M2<-lm(log(D)~log(H_CD),data=Data_bin)
  Validation_data$D_pred_M2<-exp(predict(M2,Validation_data))*exp(summary(M2)$sigma^2/2)
  Validation_data$Error_M2<-relative_err(Validation_data$D,Validation_data$D_pred_M2)
  lines(smooth.spline(Validation_data$Error_M2 ~ Validation_data$D,nknots=5),col=alpha("grey20",0.25),lwd=0.5)
  
  #### Compute and store model error statistics
  
  ## Model fit to raw data
  Raw_data_model[i,"alpha"]<-coef(M1)[1]
  Raw_data_model[i,"beta"]<-coef(M1)[2]
  Raw_data_model[i,"sigma"]<-summary(M1)$sigma
  Raw_data_model[i,"RMSE"]<-RMSE(Validation_data$D,Validation_data$D_pred_M1)
  Raw_data_model[i,"bias"]<-bias(Validation_data$D,Validation_data$D_pred_M1)
  Raw_data_model[i,"CV"]<-CV(Validation_data$D,Validation_data$D_pred_M1)
  
  ## Model fit to binned data
  Binned_data_model[i,"alpha"]<-coef(M2)[1]
  Binned_data_model[i,"beta"]<-coef(M2)[2]
  Binned_data_model[i,"sigma"]<-summary(M2)$sigma
  Binned_data_model[i,"sigma_v"]<-sigma_v(Validation_data$D,Validation_data$D_pred_M2)
  Binned_data_model[i,"RMSE"]<-RMSE(Validation_data$D,Validation_data$D_pred_M2)
  Binned_data_model[i,"bias"]<-bias(Validation_data$D,Validation_data$D_pred_M2)
  Binned_data_model[i,"CV"]<-CV(Validation_data$D,Validation_data$D_pred_M2)

}

# Estimating uncertainty for models fit to binned data -------------

t_crit<-1.96                                                                            # critical value of the Student's t distribution 
n<-length(Validation_data$D)                                                            # number of observations
sigma_v_mean<-mean(Binned_data_model$sigma_v)                                           # mean sigma_v value based on 100 randomization steps
H_CD_new<-seq(min(log(Validation_data$H_CD)),max(log(Validation_data$H_CD)),len=100)    # new data for which to make predictions
H_CD_mean<-mean(log(Validation_data$H_CD))                                              # mean value of the explanatory variable 
H_CD_sd<-sd(log(Validation_data$H_CD))                                                  # standard deviation of the explanatory variable 

plot(D~H_CD,Validation_data,col=alpha("grey80",1),pch=16,log="xy",cex=0.2,xlim=c(0.5,5000),ylim=c(1,300),bty="l",
     ylab="Diameter (cm)",xlab="Height (m) × Crown diameter (m)",xaxt="n",cex.lab=1.1,cex.axis=0.9)
axis(1,at=c(0.5,5,50,500,5000),c("0.5","5","50","500","5000"),cex.lab=1.1,cex.axis=0.9)

new.dat<-data.frame(H_CD=exp(H_CD_new))
new.dat$D_pred<-exp(predict(M1,new.dat))*exp(summary(M2)$sigma^2/2)
points(D_pred~H_CD,new.dat[new.dat$D_pred>1,],type="l",col="darkorange",lwd=3)

new.dat$PI_hi<-exp(log(new.dat$D_pred)+Prediction_interval(t_crit,sigma_v_mean,n,H_CD_new,H_CD_mean,H_CD_sd))
new.dat$PI_lo<-exp(log(new.dat$D_pred)-Prediction_interval(t_crit,sigma_v_mean,n,H_CD_new,H_CD_mean,H_CD_sd))
points(PI_hi~H_CD,new.dat[new.dat$PI_hi>1,],type="l",col="#4682B4",lty=2,lwd=2)
points(PI_lo~H_CD,new.dat[new.dat$PI_lo>1,],type="l",col="#4682B4",lty=2,lwd=2)
