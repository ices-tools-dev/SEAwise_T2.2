#SEAwise project (seawiseproject.org) - Task 2.2
# This script developed the work presented in Deliverable 2.2 
# Bitetto, Isabella; Spedicato, Maria-Teresa; Lembo, Guiseppe; Plataniotis, Angelos; Halkos, Georgios; Koundouri, Phoebe; et al. (2024). SEAwise first synthetic summary report on social and economic aspects of fishing for online tool. Technical University of Denmark. Online resource. 
# https://doi.org/10.11583/DTU.25634508.v1


# SCRIPT to estimate coefficients of the variable costs functions (equations are in the Deliverable 2.2) on the data of Adriatic and western Ionian Sea (Italy)

rm(list=ls())
wd<-"C:\\Users\\Bitetto Isabella\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020-SEAwise\\_____________WP2\\TASK 2.2\\reviewers"
setwd(wd)

# The fuel costs by metier were estimated using the methodology described in Bitetto et al. 2022 https://doi.org/10.1371/journal.pone.0264334

data= read.table("data_fitting.csv",sep=";", header=TRUE)
n_iterations=30


#Merg_temp=readxl::read_excel("Dataset_fin.xlsx",sheet="Foglio1")

# Energy costs
dir.create("FuelCosts")


FS=unique(as.character(data$Fleet_segment))

res=data.frame(coeff=matrix(ncol=1,nrow=length(FS)))
res$FS=""
res$MODEL=1
res$points=0
res$RMSE=NA



data$EnCosMod1<-0


FC= data[data$Variable=="variable.cost[fuel.cost]",]
FD=data[data$Variable=="DAYS.annual",]

Merg_temp=merge(FC,FD, by=c("Year","Fleet_segment","Species","EnCosMod1"))
colnames(Merg_temp)[c(6,10)]=c("fuel_costs","FD")


Merg_temp1=Merg_temp[1,]


for (i in 1:length(FS)){
  print(FS[i])
  
 # Model 1 
  Merg_temp2=Merg_temp[Merg_temp$Fleet_segment==FS[i],] 
  

  n=round(nrow(Merg_temp2)*5/6,0)
  
  flee=data.frame(coeff=rep(NA,n_iterations),points=0, FS=FS[i], RMSE=0, iteration=0)
  
  for (k in 1: n_iterations){
   
  hind_indices=sample(1:nrow(Merg_temp2),n)
  
  fore= which(!seq(1:nrow(Merg_temp2)) %in% hind_indices)
  
  flee$coeff[k]=coefficients(lm(fuel_costs~FD+0,data=Merg_temp2[hind_indices,]))
  flee$points[k]=nrow(Merg_temp2)
  
  Merg_temp2$EnCosMod1=flee$coeff[k]*Merg_temp2$FD
  flee$FS[k]=FS[i]
  
  
  flee$RMSE[k]=sqrt(sum((as.numeric(Merg_temp2[fore,]$fuel_costs)-Merg_temp2[fore,]$EnCosMod1)^2,na.rm=TRUE)/(length(Merg_temp2[fore,]$fuel_costs)+1)) 
  flee$iteration[k]=k
 
   }
  
  res$FS[i]=FS[i]
  res$MODEL[i]=1
  res$points[i]=n
  res$RMSE[i]=mean(flee$RMSE)
  res$coeff[i]=mean(flee$coeff)
  
 Merg_temp2$EnCosMod1=res$coeff[i]*Merg_temp2$FD  
 
     Merg_temp1=rbind(Merg_temp1,Merg_temp2)
   
  
  
}


Merg_temp2=Merg_temp1[-1,]




head(res)

write.table(res,"FuelCosts/EnCost_model1.csv",sep=";",row.names=F)



# Plots

Merg_temp1=Merg_temp2

for (r in 1:length(FS)){
  jpeg(paste("FuelCosts/graph",r,".jpg",sep=""),units="cm",width = 35,height=20,res=400)
  Merg_tempp1=Merg_temp1[Merg_temp1$Fleet_segment==FS[r],]
  plot(Merg_tempp1$Year,as.numeric(Merg_tempp1$fuel_costs), ylab="Fuel costs (euro)",xlab="Year",main=FS[r],ylim=c(0,1.5*max(as.numeric(Merg_tempp1$fuel_costs),na.rm=TRUE)),pch=19,cex=1.5,col="dark grey",cex.main=1.5,cex.axis=1.5,cex.lab=1.3)  
  lines(Merg_tempp1$Year,as.numeric(Merg_tempp1$EnCosMod1),col="green",lwd=3)
  #lines(Merg_tempp1$year,as.numeric(Merg_tempp1$EnCosMod2),col="blue",lwd=3)
  legend("topleft",c("Observed","Model1"),col=c("dark grey","blue"),pch=c(19,NA),lty=c(NA,"solid"),lwd=c(NA,4))
 dev.off() 
  }



