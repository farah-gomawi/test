#for github

mean_coherence<-readRDS('mean_coherence.RDS')
channels<-readRDS('channels.RDS')
subjs<-('subjs.RDS')
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

  electrodes <- data.frame(
      name = c("FP1", "FP2",
               "F7", "F3", "FZ", "F4", "F8", 
               "FT7", "FC3", "FCZ", "FC4", "FT8", 
               "T3", "C3", "CZ", "C4", "T4", 
               "TP7", "CP3", "CPZ", "CP4", "TP8", 
               "T5", "P3", "PZ", "P4", "T6", 
               "O1", "OZ", "O2"),
      x = c(-3, 3,
            -6.2, -3, 0, 3, 6.2,
            -7, -3, 0, 3, 7,
            -8, -3, 0, 3, 8,
            -8, -3, 0, 3, 8,
            -7, -3, 0, 3, 7,  
            -3.5, 0, 3.5),
      y = c(9.5, 9.5,
            6.5, 6.5, 6.5, 6.5, 6.5,
            3.5, 3, 3.5, 3, 3.5,
            0, 0, 0, 0, 0,
            -3.5, -3, -3.5, -3, -3.5,
            -6, -6.5, -6.5, -6.5,-6,
            -9.5, -10.5, -9.5) # Occipitals
    )
    
