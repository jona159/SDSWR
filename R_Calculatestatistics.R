einladenundanzeigen<-function(){
  T1<-read.csv2(file.choose(), na="Na", head=TRUE, sep=";", stringsAsFactors = FALSE)
  num_data = data.frame(data.matrix(T1))
  numeric_columns <- sapply(num_data,function(x){mean(as.numeric(is.na(x)))<0.5})
  final_data <- data.frame(num_data[,numeric_columns], T1[,!numeric_columns])
  View(final_data)
  View(T1)
  pdf("Testpdf.pdf")
  plot_histograms(T1)
  for(i in 1:ncol(final_data)){
    table <- matrix(ncol=4,byrow=TRUE)
    table<-as.data.frame(table)
    colnames(table)<-c("Mean","Median","Variance","Standard_Deviation")
    new_df<-matrix(ncol=4, nrow=ncol(final_data),byrow=TRUE)
    new_df<-as.data.frame(new_df)
    colnames(new_df)<-c("Mean","Median","Variance","Standard_Deviation")
    new_df[i,1]=mean(final_data[,i])
    new_df[i,2]=median(final_data[,i])
    new_df[i,3]=var(final_data[,i])
    new_df[i,4]=sd(final_data[,i])
    View(new_df)
    table<- rbind(table,new_df)
    #table<-table[-1,]
    #View(table)
    #rownames(table)<-colnames(final_data)
    #plot(table,main="Statistics")
    #dev.off()
  }
  table<-table[-1,]
  View(table)
  rownames(table)<-colnames(final_data)
  plot(table,main="Statistics")
  dev.off()
}

plot_histograms<- function(x) for(i in 1:ncol(x))
{
  if(is.numeric(x[,i])){
  hist(x[,i],main=paste("Histogram of",colnames(x)[i]))
  }}

'''
calculate_statistics<- function(x) for(i in 1:ncol(x))
{
  table <- matrix(ncol=4,byrow=TRUE)
  table<-as.table(table)
  colnames(table)<-c("Mean","Median","Variance","Standard_Deviation")
  
  if(is.numeric(x[,i])){
     x<- x %>% select(where(~!all(is.na(.))))
     new_df<-matrix(nrow=1,ncol=4)
     new_df[1,1]=mean(x[,1])
     new_df[1,2]=median(x[,1])
     new_df[1,3]=var(x[,1])
     new_df[1,4]=sd(x[,1])
  table<- rbind(table,new_df)
  table<-table[complete.cases(table), ]
     
  }
  #return(table)
  plot(table,main="Statistics")
  } 
'''


