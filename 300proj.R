# STAT 300 Project 1 
library(seewave)
library(abind)
library(eegkit)
library(ggplot2)
library(gplots)
#setwd("~/Documents/R/Drivingdata")
#Motion_subjects<-readRDS('Motion_subjects.RDS')
#channels<-colnames(Motion_subjects$s02_m1$EEGpre) # getting the names of the channels from the data
#subjs<-c('s02','s06','s12','s13','s23')
#length(Motion_subjects) # number of experiments 
sr=128 #sampling rate of the EEG
t=2 # physical time 2 seconds
obv=sr*t # total number of observations sampling rate x physical time

#ntrials_motion<-c()
#for (i in 1:length(Motion_subjects)) {
#  ntrials_motion<-append(ntrials_motion,length(Motion_subjects[[i]][[4]]))}
#plot(ntrials_motion,lwd=4,type='h',ylab='number of trials',
#     main=paste0('Histogram of # tirals \n Total #trials =',
#                 sum(ntrials_motion)))

#focusing on the EEGpre data for the motion subjects 
#motion<-  abind(Motion_subjects$s02_m1$EEGpre,Motion_subjects$s02_m2$EEGpre,
#                 Motion_subjects$s06_m1$EEGpre,Motion_subjects$s12_m1$EEGpre,
#                 Motion_subjects$s12_m2$EEGpre,Motion_subjects$s13_m1$EEGpre,
#                 Motion_subjects$s13_m2$EEGpre,Motion_subjects$s23_m1$EEGpre,along = 1)
dim(motion) #number of trials, number of channels, number of observations

# a set of useful functions
{
  freq<-seq(0,sr/2,length=(obv/2))
  #periodograms
  ffts<-function(ts){(1/sqrt(obv))*fft(ts)}
  periods<-function(fft){return((((abs(fft))^2))[1:(sr*t/2)])}

  #spectral decomposition
  extract_rhythms <- function(x,fs=sr,rhythms = c("delta","theta","alpha","beta","gamma"),or=4){
    y <- vector("list",length(rhythms))
    for(i in 1:length(rhythms)){
      
      if(rhythms[i] == "delta"){
        y[[i]] <- bwfilter(x, f = fs, from = 0.5, to = 4, n = or)
      } else if(rhythms[i] == "theta"){
        y[[i]] <- bwfilter(x, f = fs, from = 4, to = 8, n = or)
      } else if(rhythms[i] == "alpha"){
        y[[i]] <- bwfilter(x, f = fs, from = 8, to = 12, n = or)
      } else if(rhythms[i] == "beta"){
        y[[i]] <- bwfilter(x, f = fs, from = 12, to = 30, n = or)
      } else if(rhythms[i] == "gamma"){
        y[[i]] <- bwfilter(x, f = fs, from = 30, to = 50, n = or)
      }
    }
    return(y)
  }
  
  #my coherence function
  mycoh<-function(ts1,ts2,lag){
    corvec<-vector()
    len<-length(ts1)
    #zero lag
    corvec<-append(corvec,abs(cor(ts1,ts2))^2)
    #negative lag
    for (h in 1:lag) {
      corvec<-append(corvec,abs(cor(ts1[1:(len-h)],ts2[(1+h):len]))^2)
    }
    #positive lag
    for (h in (1):(lag)) {
      corvec<-append(corvec,abs(cor(ts1[(1+h):(len)],ts2[(1):(len-h)]))^2)
    }
    max(corvec)
  }
  
}

#Let's see how the periodogram looks like before doing spectral decomposition
trial=1
channel=1
plot(freq,periods(ffts(motion[trial,channel,])),type = 'l',xlab = 'Frequency',ylab = 'Magnitude',
     main = paste('Periodogram of channel',channels[channel],'and trial:',trial))

#now spectral decomposition
spectral_motion<-array(dim = c(5,dim(motion)))
#running this might take a minute or two :) 
for (t in 1:dim(motion)[1]) {
    for (c in 1:dim(motion)[2]) {
      temp<-extract_rhythms(motion[t,c,],f=128,or=4)
      for (b in 1:5) {
        spectral_motion[b,t,c,]<-temp[[b]]

        spectral_motion[b,t,c,]<-(spectral_motion[b,t,c,]-mean(spectral_motion[b,t,c,]))/
          sd(spectral_motion[b,t,c,])
    }
  }
}

coherence_motion<-array(dim = c(dim(spectral_motion)[1:3],30))
#l=19
l=3 #Minimal lag for now to minimize computation time
#running this might take two minutes or five :))
for (b in 1:5) {
  for (i in 1:30) {
    for(j in i:30){
     for (t in 1:dim(spectral_motion)[2]) {
        coherence_motion[b,t,i,j]<-mycoh(spectral_motion[b,t,i,],spectral_motion[b,t,j,],lag = l)
      }
    }
  }
}
#coherence is symmetrical
for (b in 1:5) {
    for (i in 1:30) {
      for (j in 1:(i-1)) {
        for (t in 1:dim(spectral_motion)[2]) {
          coherence_motion[b,t,i, j] <-  coherence_motion[b,t,j, i] # Mirror the value from the upper triangle
      }
    }
  }
}


dim(coherence_motion)
mean_coherence<-array(dim = c(5,30,30))

for (b in 1:5) {
  for (c in 1:30) {
    for (c2 in 1:30) {
      mean_coherence[b,c,c2]<-as.numeric(mean(coherence_motion[b,,c,c2]))
    }  
  }
}

dim(mean_coherence)

heatmap(mean_coherence[1,,],symm = TRUE)

coh_colormap<-array(dim = dim(mean_coherence))
for (b in 1:5) {
  for (c1 in 1:30){
    for (c2 in 1:30) {
      if (c1==c2){
        coh_colormap[b,c1,c2]='green'
      }else if((mean_coherence[b,c1,c2])>0.75){
        coh_colormap[b,c1,c2]='red'
      }else if((mean_coherence[b,c1,c2])>0.5){
        coh_colormap[b,c1,c2]='pink'
      }else if(mean_coherence[b,c1,c2]>0.25){
        coh_colormap[b,c1,c2]='mistyrose'
      }else{
        coh_colormap[b,c1,c2]='white'
      }
    }
  }
}
'mistyrose'

library(plotly)
band=1
ref_channel=2
g<-eegcap2d(c("FP1" , "FP2" , "F7"  , "F3"  , "FZ"  , "F4"  , "F8"  , "FT7" , "FC3" , "FCZ",  "FC4" ,
           "FT8" , "T7"   ,"C3"  , "CZ"  , "C4","T8",   "TP7",  "CP3",  "CPZ",  "CP4",  "TP8"  ,
           "P7" , "P3"  , "PZ"  , "P4"  , "P8", "O1"  , "OZ"  , "O2"  ),
         col.point=coh_colormap[band,ref_channel,]
         )

ggplotly(g)

legend()


Colors=c("white","red")
Colors=colorRampPalette(Colors)(6)

heatmap.2(mean_coherence[1,,],dendrogram = "none",Rowv=NA,Colv=NA,margins=c(5,5),
          main="Average Coherence in motion subjects",scale="none",ylab="Channles",xlab="Channels",
          key=TRUE,trace="none",keysize=1.5,col = Colors,
          tracecol = "black",
          lmat = rbind(c(4,3,3,3),c(4,1,1,1),c(2,1,1,1),c(0,1,1,1)),lwid = c(1,1,1,2),lhei = c(0.5,1.5,1,2))

