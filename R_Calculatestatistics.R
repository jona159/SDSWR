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


# libraries: kutils, psych, svDialogs, gridExtra
einladenundanzeigen<-function(){
  #language <- dlgInput("If your CSV is english type: ENG, if it is german type: DE", Sys.info()["language"])$res
  seperation_list<-list("COMMA", "SEMICOLON", "EMPTY SPACE")
  response<-dlg_list(choices = seperation_list, multiple= FALSE, title="Seperation in your file")$res
  if(response=="COMMA"){
    T1<-read.csv2(file.choose(), na="Na", head=TRUE, sep=",", stringsAsFactors = FALSE)
  }
  else if(response=="SEMICOLON"){
    T1<-read.csv2(file.choose(), na="Na", head=TRUE, sep=";", stringsAsFactors = FALSE)
  }
  else if(response=="EMPTY SPACE"){
    T1<-read.csv2(file.choose(), na="Na", head=TRUE, stringsAsFactors = FALSE)
  }
  else {
    dlg_message("Your CSV file must be in english or german")
  }
  pdf("Statistical_Analysis.pdf")
  plot_histograms(T1)
  plot_boxplots(T1)
  dlg_message("A PDF named Statistical Analysis with the analysis results can be found in your current workind directory")
  for(i in 1:ncol(T1)){
    new_df<-matrix(ncol=4, nrow=ncol(T1),byrow=TRUE)
    new_df<-as.data.frame(new_df)
    colnames(new_df)<-c("Mean","Median","Variance","Standard_Deviation")
    rownames(new_df)<-colnames(T1)
    if(is.numeric(T1[,i])){       
      new_df[i, 1] = mean(T1[, i])
      new_df[i, 2] = median(T1[, i])
      new_df[i, 3] = var(T1[, i])
      new_df[i, 4] = sd(T1[, i])
      new_df<-new_df[complete.cases(new_df), ]
      if(length(new_df[,1])>2 && length(new_df[,1])<4999){
        p_value <- shapiro.test(new_df[,1])$p.value
        if(p_value>0.05){
          t_test <- t.test(new_df[,1])
          View(t_test)
          if(t_test$p.value>0.05){
            print("T Test resulted in p-Value above 0.05")
            #dlg_message("T Test resulted in p-Value above 0.05")
          }
          else{
            print("T Test resulted in p-Value lower than 0.05")
            #dlg_message("T Test resulted in p-Value lower than 0.05")
          }
        }
      }
      View(new_df)
    plot.new()
    grid.table(new_df)
    x<-nrow(T1)
    x<-x-1
    df<-matrix(ncol=ncol(T1), nrow=nrow(T1))
    df<-df[-1,]
    for(i in 1:ncol(T1))
    {
      T2<-T1[i]
      T2<-T2[-1,]
      T2<-as.numeric(T2)
      if(all(!is.na(T2))){
        for(j in 1:x){ 
          df[j,i]<-T2[j]
        }
        
        p_value <- shapiro.test(T2[3:5000])$p.value
        print(p_value)
        
      }
    }
    colnames(df)<-colnames(T1)
    colnames(df) <- shorten(colnames(df), k=20, unique=FALSE)
    df<-df[, colSums(is.na(df)) != nrow(df)]
    View(df)
    '''
    print(length(df[,1]))
    if(length(df[,1])>2 && length(df[,1])<4999){
      p_value <- shapiro.test(df[,1])$p.value
      print(paste0("P value is", p_value))
      if(p_value>0.05){
        t_test <- t.test(new_df[,1])
        if(t_test$p.value>0.05){
          print("T Test resulted in p-Value above 0.05")
          #dlg_message("T Test resulted in p-Value above 0.05")
        }
        else{
          print("T Test resulted in p-Value lower than 0.05")
          #dlg_message("T Test resulted in p-Value lower than 0.05")
        }
      }}
    
    '''
    correlation <- cor(df)
    corrplot(correlation, method="circle", title = "Correlation Matrix", sig.level = 0.01, insig = "blank")
    
  }}
  
  dev.off()
  
}
'''
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
'''
correlation_analysis<-function(x) {
  T1<-read.csv2(file.choose(), na="Na", head=TRUE, sep=",", stringsAsFactors = FALSE)
  x<-nrow(T1)
  x<-x-1
  print(x)
  df<-matrix(ncol=ncol(T1), nrow=nrow(T1))
  df<-df[-1,]
  for(i in 1:ncol(T1))
   {
    T2<-T1[i]
    T2<-T2[-1,]
    T2<-as.numeric(T2)
    if(all(!is.na(T2))){
      for(j in 1:x){ 
      df[j,i]<-T2[j]
      }
      
      p_value <- shapiro.test(T2[3:5000])$p.value
      print(p_value)
      
     }
  }
  df<-df[, colSums(is.na(df)) != nrow(df)]
  View(df)
  correlation <- cor(df)
  corrplot(correlation, method="circle", title = "Correlation Matrix", sig.level = 0.01, insig = "blank")
  }
  
#libraries: sf, raster, rnaturalearth, rnaturalearthdata, ggplot2
 csv_to_shp <-function(x){ 
   dlgMessage("Your CSV must contain clumns named latitude and longitude with valid entries and the head row cannot be empty")
   seperation_list<-list("COMMA", "SEMICOLON", "EMPTY SPACE")
   response<-dlg_list(choices = seperation_list, multiple= FALSE, title="Seperation in your file")$res
   if(response=="COMMA"){
     spatial_csv<-read.csv2(file.choose(), na="Na", head=TRUE, sep=",", stringsAsFactors = FALSE)
   }
   else if(response=="SEMICOLON"){
     spatial_csv<-read.csv2(file.choose(), na="Na", head=TRUE, sep=";", stringsAsFactors = FALSE)
   }
   else if(response=="EMPTY SPACE"){
     spatial_csv<-read.csv2(file.choose(), na="Na", head=TRUE, stringsAsFactors = FALSE)
   }
   View(spatial_csv)
   print(head(spatial_csv$Latitude))
   latitude <- spatial_csv$latitude
   print(latitude)
   longitude <- spatial_csv$longitude
   print(longitude)
   coordinate_RS <- dlg_input(message="Type which CRS your data have", Sys.info()["coordinate_RS"])$res
   #coordinate_RS <- spatial_csv$CRS[1]
   #print(crs)
   converted <-st_as_sf(spatial_csv, coords = c("latitude", "longitude"), crs = coordinate_RS)
   class(converted)
   st_crs(converted)
   head(st_crs(converted))
   head(st_coordinates(converted))
   View(converted)
   world <- ne_countries(scale = "medium", returnclass = "sf")
   print(ggplot(data = world) +
           geom_sf() +
           #coord_sf(default_crs = NULL, lims_method = "geometry_bbox") + 
           coord_sf(crs= coordinate_RS, lims_method = "geometry_bbox") +  
           geom_point(data = converted$geometry, col="yellow", size=3, aes(x= longitude, y=latitude)) +
           ggtitle("World map with your locations" ))
       
       
    current= getwd()
    st_write(converted, current, layer="coordinates", driver="ESRI Shapefile")
    dlgMessage("You can now inspect this shapefile in a GIS of your choice, the shapefile is in your current working directory")
       
}   
  
  

'''
plot_pie<- function(x) for(i in 1:ncol(x))
{
  if(is.numeric(x[,i])){
    pie(x[,i],main=paste("Pie diagram of",colnames(x)[i]))
  }}
'''


