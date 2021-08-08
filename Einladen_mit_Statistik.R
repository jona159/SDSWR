einladenundanzeigen<-function(){
  T1<-read.csv2(file.choose(), na="Na", head=TRUE, sep=";", stringsAsFactors = FALSE)
  pdf("Statistical_Analysis.pdf")
  plot_histograms(T1)
  # svDialogs required
  dlg_message("A PDF with the analysis results can be found in your current workind directory")
  for(i in 1:ncol(T1)){
    new_df<-matrix(ncol=4, nrow=ncol(T1),byrow=TRUE)
    new_df<-as.data.frame(new_df)
    colnames(new_df)<-c("Mean","Median","Variance","Standard_Deviation")
    rownames(new_df)<-colnames(T1)
    View(new_df)
    if(is.numeric(T1[,i])){       
      new_df[i, 1] = mean(T1[, i])
      new_df[i, 2] = median(T1[, i])
      new_df[i, 3] = var(T1[, i])
      new_df[i, 4] = sd(T1[, i])
      View(new_df)
    plot.new()
    # library gridExtra required
    grid.table(new_df)
      
  }}
  
  dev.off()
  
}

plot_histograms<- function(x) for(i in 1:ncol(x))
{
  if(is.numeric(x[,i])){
  hist(x[,i],main=paste("Histogram of",colnames(x)[i]))
  }}
