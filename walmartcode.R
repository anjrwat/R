
#loading walmart data & counting no. of rows
n_rows = nrow(walmart_data)
head(walmart_data,5)
rows_per_store = table(walmart_data$Store)

#converting rows into a data frame
rows_per_store= as.data.frame(rows_per_store)
rows_per_store[order(rows_per_store$Freq),]
rows_per_store[order(-rows_per_store$Freq),"Var1"]

#summing up total sales
sym_by_store = aggregate(
  x=walmart_data$Weekly_Sales,
  by = list(walmart_data$Store),
  FUN = sum
)

#modifyig colnames
colnames(sym_by_store) = c('storeno.','total_sales')
colnames(rows_per_store)= c('variables','frequency')

#plotting total sales per store
barplot(sym_by_store[order(-sym_by_store$total_sales),"total_sales"],main='sales by store',col='green')

#mean,Standard deviation and standardisation
mean_walmart = sapply(X= walmart_features,FUN=mean,na.rm = TRUE)
walmart_features['standardised_cpi']=((walmart_features$CPI-mean(walmart_features$CPI,na.rm=TRUE))/sd(walmart_features$CPI,na.rm=TRUE))
 
store_1 = walmart_data[walmart_data$Store==1,]

 
store_1_toTAL= aggregate(
  x= store_1$Weekly_Sales,
  by=list(store_1$Date),
  FUN=sum
)

#ggplot and other plots
plot(
  x=store_1_toTAL$Group.1,
  y=store_1_toTAL$x,
 xlab='date',
 ylab='sales'
 )
lines(x=store_1_toTAL$Group.1,
      y=store_1_toTAL$x)

store_20 = walmart_data[walmart_data$Store==20,]
store_20_toTAL= aggregate(
  x= store_20$Weekly_Sales,
  by=list(store_20$Date),
  FUN=sum
)

library(ggplot2)
ggplot(
  data=store_20_toTAL,
  aes(x=as.Date(Group.1),y=x,group=1)
  )+geom_line()+geom_point(col='green')+ylab('sales')

as.Date(store_20_toTAL$Group.1)

top_5 = walmart_data[walmart_data$Store==2,]
sales_by_department = aggregate(
  x=top_5$Weekly_Sales,
  by=list(top_5$Dept),
  FUN=sum
)

sales_by_department[order(sales_by_department$x),]
sales_by_department[order(-sales_by_department$x),"Group.1"]

top_5_dept = sales_by_department[order(-sales_by_department$x),"Group.1"][1:5]
top5deptsales = store_20[store_20$Dept %in% top_5_dept,] 

ggplot(
 data = top5deptsales,
 aes(x=as.Date(Date),y=Weekly_Sales,group=Dept,color=as.factor(Dept))
  )+geom_line()




