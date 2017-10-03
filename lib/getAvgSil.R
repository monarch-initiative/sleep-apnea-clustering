getAvgSil<-function(clustering, gower_mat){
  sil_val<-silhouette(clustering, gower_mat)
  sum.sil<-summary(sil_val)
  avgsil<-sum.sil$avg.width
  
  return(avgsil)
}