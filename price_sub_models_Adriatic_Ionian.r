rm(list=ls())
wd<-"C:\\Users\\Bitetto Isabella\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\H2020-SEAwise\\_____________WP2\\TASK 2.2\\reviewers"
setwd(wd)

data= read.table("data_fitting.csv",sep=";", header=TRUE)
n_iterations=30
library(dplyr)

#land_data=readxl::read_excel("Landing.xlsx",sheet="Foglio1")
#colnames(land_data)


# Selection of species and area
species<-c("HKE1718")


dir.create("FishPrice")
dir.create(paste("FishPrice/",species,sep=""))

data_temp= data[data$Species %in% species,]
data_temp=data_temp[data_temp$Value!=0,]


Rev= data_temp[data_temp$Value >0 & data_temp$Variable == "revenues.landing",]
colnames(Rev)[5]="Revenues"
Land= data_temp[data_temp$Value >0 & data_temp$Variable == "landing.weight",]
colnames(Land)[5]="Landing"

library(dplyr)
land_data2=left_join(Rev[,c(1:3,5)],Land[,c(1:3,5)])

#land_data2=merge(Rev[,c(1:Landland_data2=merge(Rev[,c(1:3,5)],Land[,c(1:3,5)],by=c("Year","Fleet_segment","Species"))

#colnames(land_data2)[c(4,5)]=c("Revenues","Landing")


#land_data2=land_data[land_data$Revenues!=0,]
land_data2$price<-0
land_data2$price<-as.numeric(land_data2$Revenues)/as.numeric(land_data2$Landing)


# Model 1
# 

strata=unique(land_data2$Fleet_segment)
i=1

epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=1
#epsilon$fore_points=0

for (i in c(1:length(strata))){
  
epsilon$FS[i]=as.character(strata[i])
land_data2_temp=land_data2[land_data2$Fleet_segment==strata[i],]
land_data2_temp$ratio=0
land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[-nrow(land_data2_temp)]
land_data2_temp$ratio2=999
land_data2_temp$ratio2[-1]=(land_data2_temp$Land[-1]-land_data2_temp$Land[-nrow(land_data2_temp)])/land_data2_temp$Land[-1]

land_data2_temp$price_mod1=0

flee=data.frame(coeff=rep(NA,n_iterations),points=0, FS=strata[i], RMSE=0, iteration=0)

n=round(nrow(land_data2_temp)*4/6,0)

for (k in 1: n_iterations){
  # hindcasting only on 4/6 of the data

hind_indices=sample(2:nrow(land_data2_temp),n)

fore= which(!seq(1:nrow(land_data2_temp)) %in% hind_indices)

#epsilon[i,]$fore_points=paste(fore,collapse=",")

if(nrow(land_data2_temp)>5){
epsilon[i,]$epsilon=round(coefficients(lm((ratio-1)~ratio2+0,data=land_data2_temp[hind_indices,])),5)
}

land_data2_temp$price_mod1[-1]= land_data2_temp$price[-nrow(land_data2_temp)]*(1+epsilon[i,]$epsilon*land_data2_temp$ratio2[-1])


flee$coeff[k]= epsilon[i,]$epsilon
flee$RMSE[k] = sqrt(sum((as.numeric(land_data2_temp[fore,]$price)-land_data2_temp[fore,]$price_mod1)^2,na.rm=TRUE)/(length(land_data2_temp[fore,]$price)+1))
flee$FS=strata[i]
flee$iteration= k

}
epsilon$epsilon[i]=mean(flee$coeff)

epsilon$RMSE[i]=mean(flee$RMSE)


}
write.table(epsilon,paste("FishPrice/",species,"/Price_model1.csv",sep=""),sep=";",row.names=F)

# Model 2 (model 3 in Deliverable 2.2)

i=1

epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=2
#epsilon$fore_points=0

for (i in c(1:length(strata))){
  epsilon$FS[i]=strata[i]
  land_data2_temp=land_data2[land_data2$Fleet_segment==strata[i],]
  land_data2_temp$ratio=0
  land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[-nrow(land_data2_temp)]
  land_data2_temp$ratio2=999
  land_data2_temp$ratio2[-1]=(land_data2_temp$Landing[-1]/land_data2_temp$Landing[-nrow(land_data2_temp)])
  
  land_data2_temp$price_mod2=0
  # hindcasting only on 2/3 of the data
  n=round(nrow(land_data2_temp)*4/6,0)
  
  flee=data.frame(coeff=rep(NA,n_iterations),points=0, FS=strata[i], RMSE=0, iteration=0)
  
  n=round(nrow(land_data2_temp)*4/6,0)
  
  for (k in 1: n_iterations){
    
  hind_indices=sample(2:nrow(land_data2_temp),n)
  fore= which(!seq(1:nrow(land_data2_temp)) %in% hind_indices)
  
  #epsilon[i,]$fore_points=paste(fore,collapse=",")
  
  if (class(try(coefficients(nls((ratio)~(ratio2)^a,start=list(a=1),data=land_data2_temp[hind_indices,])),silent=TRUE))!="try-error") {
  epsilon[i,]$epsilon=round(coefficients(nls((ratio)~(ratio2)^a,start=list(a=1),data=land_data2_temp[hind_indices,]),silent=TRUE),3)
  } else {
    epsilon[i,]$epsilon=NA  
  }
  
  land_data2_temp$price_mod2[-1]= land_data2_temp$price[-nrow(land_data2_temp)]*(land_data2_temp$Landing[-1]/land_data2_temp$Landing[-nrow(land_data2_temp)])^epsilon[i,]$epsilon
  
  
  flee$coeff[k]= epsilon[i,]$epsilon
  flee$RMSE[k] = sqrt(sum((as.numeric(land_data2_temp[fore,]$price)-land_data2_temp[fore,]$price_mod2)^2,na.rm=TRUE)/(length(land_data2_temp[fore,]$price)+1))
  flee$FS=strata[i]
  flee$iteration= k
  
  }
  epsilon$epsilon[i]=mean(flee$coeff)
  epsilon$RMSE[i]=mean(flee$RMSE)
  
}

write.table(epsilon,paste("FishPrice/",species,"/Price_model2.csv",sep=""),sep=";",row.names=F)

# Model 3 (model 4 in Deliverable 2.2)


epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=3
#epsilon$fore_points<-0

for (i in c(1:length(strata))){
  epsilon$FS[i]=strata[i]
  land_data2_temp=land_data2[land_data2$Fleet_segment==strata[i],]
  land_data2_temp$ratio=0
  land_data2_temp$ratio=land_data2_temp$price/land_data2_temp$price[nrow(land_data2_temp)]
  
  land_data2_temp$price_mod3=0
  
  #land_data2_temp$ratio2=999
  #land_data2_temp$ratio2[-1]=(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])
  
  # hindcasting only on 2/3 of the data
  n=round(nrow(land_data2_temp)*4/6,0)
  flee=data.frame(coeff=rep(NA,n_iterations),points=0, FS=strata[i], RMSE=0, iteration=0)
  
  n=round(nrow(land_data2_temp)*4/6,0)
  
  for (k in 1: n_iterations){
    
  hind_indices=sample(2:nrow(land_data2_temp),n)
  fore= which(!seq(1:nrow(land_data2_temp)) %in% hind_indices)
  
  #epsilon[i,]$fore_points=paste(fore,collapse=",")
  
  if (class(try(coefficients(nls(ratio~exp(a*Landing),start=list(a=-0.00005),data=land_data2_temp[hind_indices,])),silent=TRUE))!="try-error") {
    epsilon[i,]$epsilon=coefficients(nls(ratio~exp(a*Landing),start=list(a=-0.00005),data=land_data2_temp[hind_indices,]))
  } else {
    epsilon[i,]$epsilon=NA  
  }

  land_data2_temp$price_mod3= land_data2_temp$price[nrow(land_data2_temp)]*exp(epsilon[i,]$epsilon*land_data2_temp$Landing)
  
  
  flee$coeff[k]= epsilon[i,]$epsilon
  flee$RMSE[k] = sqrt(sum((as.numeric(land_data2_temp[fore,]$price)-land_data2_temp[fore,]$price_mod3)^2,na.rm=TRUE)/(length(land_data2_temp[fore,]$price)+1))
  
  
  flee$FS=strata[i]
  flee$iteration= k
  
  }
  epsilon$epsilon[i]=mean(flee$coeff)
  epsilon$RMSE[i]=mean(flee$RMSE)
  
  
  }

write.table(epsilon,paste("FishPrice/",species,"/Price_model3.csv",sep=""),sep=";",row.names=F)

# Model 4 (model 5 in Deliverable 2.2)



epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=4
#epsilon$fore_points=0
 
for (i in c(1:length(strata))){
  epsilon$FS[i]=strata[i]
  land_data2_temp=land_data2[land_data2$Fleet_segment==strata[i],]
 
  #land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[nrow(land_data2_temp)]
  #land_data2_temp$ratio2=999
  #land_data2_temp$ratio2[-1]=(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])
  
  # hindcasting only on 2/3 of the data
  land_data2_temp$price_mod4=0
  
  #land_data2_temp$ratio2=999
  #land_data2_temp$ratio2[-1]=(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])
  
  # hindcasting only on 4/6 of the data
  
   flee=data.frame(coeff=rep(NA,n_iterations),points=0, FS=strata[i], RMSE=0, iteration=0)
  
  n=round(nrow(land_data2_temp)*4/6,0)
  
  for (k in 1: n_iterations){
    hind_indices=sample(1:nrow(land_data2_temp),n)
  fore= which(!seq(1:nrow(land_data2_temp)) %in% hind_indices)
  
  #epsilon[i,]$fore_points=paste(fore,collapse=",")
  
 
    epsilon[i,]$epsilon=mean(land_data2_temp$price[hind_indices])  
    
    land_data2_temp$price_mod4= epsilon[i,]$epsilon
    #land_data2_temp$price_mod4[fore]= mean(land_data2_temp$price[fore])  
    
        flee$coeff[k]= epsilon[i,]$epsilon
    flee$RMSE[k] = sqrt(sum((as.numeric(land_data2_temp[fore,]$price)-land_data2_temp[fore,]$price_mod4)^2,na.rm=TRUE)/(length(land_data2_temp[fore,]$price)+1))
    flee$FS=strata[i]
    flee$iteration= k
    
  }
  epsilon$epsilon[i]=mean(flee$coeff)
  epsilon$RMSE[i]=mean(flee$RMSE)
  
}

write.table(epsilon,paste("FishPrice/",species,"/Price_model4.csv",sep=""),sep=";",row.names=F)

# Model 5 (model 6 of deliverable 2.2, not by season)

epsilon=data.frame(epsilon=matrix(ncol=1,nrow=length(strata)))
epsilon$FS=""
epsilon$SPECIES=species
epsilon$MODEL=5
#epsilon$fore_points=0

for (i in c(1:length(strata))){
  
  epsilon$FS[i]=strata[i]
  land_data2_temp=land_data2[land_data2$Fleet_segment==strata[i],]
  land_data2_temp$ratio=0
  #land_data2_temp$ratio[-1]=land_data2_temp$price[-1]/land_data2_temp$price[1]
  land_data2_temp$ratio2=999
  land_data2_temp$ratio2=land_data2_temp$price[1]*(land_data2_temp$Landing[1]/land_data2_temp$Landing)
  land_data2_temp$price_mod5=0
  
  # hindcasting only on 4/6 of the data
  n=round(nrow(land_data2_temp)*4/6,0)
  flee=data.frame(coeff=rep(NA,n_iterations),points=0, FS=strata[i], RMSE=0, iteration=0)
  
    for (k in 1: n_iterations){
    
  hind_indices=sample(1:nrow(land_data2_temp),n)
  fore= which(!seq(1:nrow(land_data2_temp)) %in% hind_indices)
  
  #epsilon[i,]$fore_points=paste(fore,collapse=",")
  
  
  if (class(try(coefficients(nls((price)~(ratio2)^a,start=list(a=0.5),data=land_data2_temp[hind_indices,])),silent=TRUE))!="try-error") {
    epsilon[i,]$epsilon=round(coefficients(nls((price)~(ratio2)^a,start=list(a=0.5),data=land_data2_temp[hind_indices,])),3)
  } else {
    epsilon[i,]$epsilon=NA  
  }


  land_data2_temp$price_mod5= land_data2_temp$price[1]*(land_data2_temp$Landing[1]/land_data2_temp$Landing)^epsilon[i,]$epsilon
  
  
  flee$coeff[k]= epsilon[i,]$epsilon
  flee$RMSE[k] = sqrt(sum((as.numeric(land_data2_temp[fore,]$price)-land_data2_temp[fore,]$price_mod5)^2,na.rm=TRUE)/(length(land_data2_temp[fore,]$price)+1))
  flee$FS=strata[i]
  flee$iteration= k
  
}

epsilon$epsilon[i]=mean(flee$coeff)
epsilon$RMSE[i]=mean(flee$RMSE)

}

write.table(epsilon,paste("FishPrice/",species,"/Price_model5.csv",sep=""),sep=";",row.names=F)

# Binding the model results

results=list.files(path=paste("FishPrice/",species,sep=""),pattern="(model)",full.names = TRUE)

data_=read.table(results[1],sep=";",header=T) 

for (r in 2:length(results)){
 data2=read.table(results[r],sep=";",header=T) 
 data_=rbind(data_,data2) 
}

write.table(epsilon,paste("FishPrice/",species,"/coefficients.csv",sep=""),sep=";",row.names=F)


land_data2_tempp=land_data2[1,]
land_data2_tempp$price_Mod1=0
land_data2_tempp$price_Mod2=0
land_data2_tempp$price_Mod3=0
land_data2_tempp$price_Mod4=0
land_data2_tempp$price_Mod5=0
#land_data2_tempp$price_Mod6=0
r=1
data_$RMSE=0
data_$points=0


for (r in 1:length(strata)){
  
  # Observed
  land_data2_temp=land_data2[land_data2$Fleet_segment==strata[r],]  
  if (nrow(land_data2_temp)>2){
  
  jpeg(paste("FishPrice/",paste(species,sep=""),"/graph",r,".jpg",sep=""),units="cm",width = 20,height=15,res=400)
  
  data_[data_$FS==strata[r],]$points = nrow(land_data2_temp)
 
#plot(land_data2_temp$Year,land_data2_temp$price, ylab="Price/kg (euro)",xlab="Year",main=paste(species, strata[r]),ylim=c(0,2*max(land_data2_temp$price,na.rm=TRUE)),pch=19,cex=1.8,col="black",cex.main=2,cex.axis=1.2,cex.lab=1.8)  
  par(mar = c(5, 6, 4, 2))  # bottom, left, top, right
  
  # Fai il plot senza ylab
  plot(land_data2_temp$Year, land_data2_temp$price,
       ylab = "",  # lo mettiamo dopo
       xlab = "Year",
       main = paste(species, strata[r]),
       ylim = c(0, 2 * max(land_data2_temp$price, na.rm = TRUE)),
       pch = 19,
       cex = 1.8,
       col = "black",
       cex.main = 2,
       cex.axis = 1.2,
       cex.lab = 1.8)
  
  # Aggiungi etichetta y spostata più lontano dal bordo
  mtext("Price/kg (euro)", side = 2, line = 4.5, cex = 1.8)

# Model 1
  
data_temp=data_[data_$MODEL==1 & data_$FS==strata[r],] 
land_data2_temp$price_Mod1=0
land_data2_temp$price_Mod1[-1]=land_data2_temp$price[-nrow(land_data2_temp)]*(1+data_temp$epsilon*(land_data2_temp$Landing[-1]-land_data2_temp$Landing[-nrow(land_data2_temp)])/land_data2_temp$Landing[-1])

land_data2_temp$price_Mod1[1]=NA
lines(land_data2_temp$Year,land_data2_temp$price_Mod1,col="green",lwd=4)  

# RMSE to be estimated on the forecast points
# fore=as.numeric(str_split(data_[data_$MODEL==1 & data_$FS==strata[r],]$fore_points,",")[[1]])
# data_[data_$MODEL==1 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[fore]-land_data2_temp$price_Mod1[fore])^2,na.rm=TRUE)/(length(land_data2_temp$price[fore])+1)) 


# Model 2


data_temp=data_[data_$MODEL==2 & data_$FS==strata[r],] 
land_data2_temp$price_Mod2=0
land_data2_temp$price_Mod2[-1]=land_data2_temp$price[-nrow(land_data2_temp)]*(land_data2_temp$Landing[-1]/land_data2_temp$Landing[-nrow(land_data2_temp)])^data_temp$epsilon
land_data2_temp$price_Mod2[1]=NA
lines(land_data2_temp$Year,land_data2_temp$price_Mod2,col="blue",lwd=4)  

# RMSE to be estimated on the forecast points
# fore=as.numeric(str_split(data_[data_$MODEL==1 & data_$FS==strata[r],]$fore_points,",")[[1]])
# data_[data_$MODEL==3 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[fore]-land_data2_temp$price_Mod3[fore])^2,na.rm=TRUE)/(length(land_data2_temp$price[fore])+1)) 


# Model 3
data_temp=data_[data_$MODEL==3 & data_$FS==strata[r],] 
land_data2_temp$price_Mod3=0
land_data2_temp$price_Mod3=land_data2_temp$price[nrow(land_data2_temp)]*exp(land_data2_temp$Landing*data_temp$epsilon)
#land_data2_temp$price_Mod3=NA
lines(land_data2_temp$Year,land_data2_temp$price_Mod3,col="orange",lwd=4)  

# RMSE to be estimated on the forecast points
# fore=as.numeric(str_split(data_[data_$MODEL==1 & data_$FS==strata[r],]$fore_points,",")[[1]])
# data_[data_$MODEL==4 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[fore]-land_data2_temp$price_Mod4[fore])^2,na.rm=TRUE)/(length(land_data2_temp$price[fore])+1)) 



# Model 4

data_temp=data_[data_$MODEL==4 & data_$FS==strata[r],] 
land_data2_temp$price_Mod4=data_temp$epsilon
#land_data2_temp$price_Mod3[-1]=land_data2_temp$price[-nrow(land_data2_temp)]*(land_data2_temp$Ton[-1]/land_data2_temp$Ton[-nrow(land_data2_temp)])^data_temp$epsilon
#land_data2_temp$price_Mod3[1]=NA
lines(land_data2_temp$Year,land_data2_temp$price_Mod4,col="black",lwd=3,lty="dotted")  

# RMSE to be estimated on the forecast points
# fore=as.numeric(str_split(data_[data_$MODEL==1 & data_$FS==strata[r],]$fore_points,",")[[1]])
# data_[data_$MODEL==5 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[fore]-land_data2_temp$price_Mod5[fore])^2,na.rm=TRUE)/(length(land_data2_temp$price[fore])+1)) 



# Model 5

data_temp=data_[data_$MODEL==5 & data_$FS==strata[r],] 
land_data2_temp$price_Mod5=0
land_data2_temp$price_Mod5=land_data2_temp$price[1]*(land_data2_temp$Landing[1]/land_data2_temp$Landing)^data_temp$epsilon
#land_data2_temp$price_Mod5[1]=NA
lines(land_data2_temp$Year,land_data2_temp$price_Mod5,col="turquoise",lwd=4)  

# RMSE to be estimated on the forecast points
# fore=as.numeric(str_split(data_[data_$MODEL==1 & data_$FS==strata[r],]$fore_points,",")[[1]])
# data_[data_$MODEL==6 & data_$FS==strata[r],]$RMSE=sqrt(sum((land_data2_temp$price[fore]-land_data2_temp$price_Mod6[fore])^2,na.rm=TRUE)/(length(land_data2_temp$price[fore])+1)) 

land_data2_tempp=rbind(land_data2_tempp,land_data2_temp)

legend("topleft",c("Observed","Model1","Model2","Model3","Model4", "Model5"),col=c("black","green","blue","orange","black","turquoise"),pch=c(19,NA,NA,NA,NA,NA),lty=c(NA,"solid","solid","solid","dotted","solid"),lwd=c(NA,4,4,4,3,4))


dev.off()

}
}

data_=data_[data_$RMSE!=0,]
write.table(land_data2_tempp[-1,],paste("FishPrice/",species,"/Price_fittings.csv",sep=""),sep=";",row.names=F)
write.table(data_,paste("FishPrice/",species, "/Fit_RMSE.csv",sep=""),sep=";",row.names=F)


