#for github

mean_coherence<-readRDS('mean_coherence.RDS')

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