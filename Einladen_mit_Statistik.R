einladenundanzeigen<-function(){
  language <- dlgInput("If your CSV is english type: ENG, if it is german type: DE", Sys.info()["language"])$res
  if(language=="DE"){
    T1<-read.csv2(file.choose(), na="Na", head=TRUE, sep=";", stringsAsFactors = FALSE)
  }
  else if(language=="ENG"){
    T1<-read.csv2(file.choose(), na="Na", head=TRUE, sep=",", stringsAsFactors = FALSE)
  }
  else {
    dlg_message("Your CSV file must be in english or german")
  }
  pdf("Statistical_Analysis.pdf")
  plot_histograms(T1)
  plot_boxplots(T1)
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

plot_boxplots<- function(x) for(i in 1:ncol(x))
{
  if(is.numeric(x[,i])){
    boxplot(x[,i],main=paste("Boxplot of",colnames(x)[i]))
  }}

correlation_analysis<-function(x) {
  T1<-read.csv2(file.choose(), na="Na", head=TRUE, sep=",", stringsAsFactors = FALSE)
  for(i in 1:ncol(T1))
{
  T2<-T1[i]
  T2<-T2[-1,]
  T2<-as.numeric(T2)
  if(all(!is.na(T2))){
     assign(paste0("var_",i),T2)
     p_value <- shapiro.test(T2[3:5000])$p.value
     print(p_value)
     if(p_value<0.05){
       print("Normally distributed")
       df<-matrix(ncol=ncol(T1), nrow=nrow(T1))
       values<-get(paste0("var_",i))
       print(values)
       #for(j in 1:nrow(T1))
       #{
        #df[j,]<-values[j,]
       #}
       #print(df)
       #values<-c()
       #values[i]<-get(paste0("var_",i))
       #print((values[i]))
       #variable_list<-c()
       #variable_list<-append(variable_list, values)
       #View((variable_list))
       #cor(variable_list[1], variable_list[2])
     }
  
  }}}
