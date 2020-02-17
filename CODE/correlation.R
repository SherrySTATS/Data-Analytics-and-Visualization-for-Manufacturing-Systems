load("/Users/xinranshi/Dropbox/lecture/CSE6242-Project/BOSCH/BOSCH/loaddata_work.RData")
num_name = names(dt_numer)
library(stringr)
features_num = 0
for (i in 0:51)
{
  temp = paste("_S", i,"_", sep = "")
  features_num[i+1] = sum(str_count(num_name, temp))
}
#features_num
date_name = names(dt_date)
date_num = 0
for (i in 0:51)
{
  temp = paste("_S", i,"_", sep = "")
  date_num[i+1] = sum(str_count(date_name, temp))
}
#date_num
cat_name = names(dt_categ)
cat_num = 0
for (i in 0:51)
{
  temp = paste("_S", i,"_", sep = "")
  cat_num[i+1] = sum(str_count(cat_name, temp))
}

#split data by station
dt_num_list = list()
id = dt_numer[,1]
resp = dt_numer[,970]
tempr = 0
templ = 2
for (i in 0:51){
  temp = 0
  tempr = templ+features_num[i+1]-1
  temp = dt_numer[,templ:tempr]
  temp = as.data.frame(temp)
  temp = cbind(id, temp,resp)
  temp = na.omit(temp)
  assign(paste("S", i, sep = ""), temp) 
  dt_num_list[[i+1]] = temp
  templ = tempr+1
  templ
  }

id = dt_numer[,1]
dt_date_list = list()
#resp = dt_numer[,970]
tempr = 0
templ = 2
for (i in 0:51){
  temp = 0
  tempr = templ+date_num[i+1]-1
  temp = dt_date[,templ:tempr]
  temp = cbind(id, temp)
  temp = na.omit(temp)
  temp = temp[,1:2]
  temp = as.data.frame(temp)
  assign(paste("Date_S", i, sep = ""), temp)
  dt_date_list[[i+1]] = temp
  templ = tempr+1
  templ
} #the date of a part pass a station is same

for (i in 1:52){
if (features_num[i]==0)
{
  dt_date_list[[i]]=0
  dt_num_list[[i]]=0
}
}


dt_date_num = list()
for (i in 1:52){
temp = 0
temp = cbind.data.frame(dt_date_list[[i]],dt_num_list[[i]])
#temp = temp[, unique(colnames(temp))]
temp = temp[, -3]
names(temp)[2] <- "timestep"
#temp = temp(order(timestep),)
dt_date_num[[i]] = temp
}

library(reshape2)
library(ggplot2)
library(RColorBrewer)
for (i in 1:52){
  if (ncol(dt_date_num[[i]])>4){
corr =cor(dt_date_num[[i]][,4:ncol(dt_date_num[[i]])-1])
corr = melt(corr)
temp_plot = ggplot(data = corr, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
  }
}

num_fail = 0
total = 0
fail_pct = 0
for (i in 1:52)
{
  num_fail[i]=sum(dt_date_num[[i]]$resp)
  total[i]=dim(dt_date_num[[i]])[1]
  fail_pct[i]=num_fail[i]/total[i]
}

plot(dt_date_num[[33]]$timestep,dt_date_num[[33]]$temp.1)
test = dt_date_num[[33]]
test1 = subset(test, test$resp==0)
hist(test$temp.1)
test2 = subset(test, test$resp==1)
qcc(test1$temp.1,type = "xbar.one")
test[order(test$timestep),]
test3 = subset(test, test$timestep<500)
plot(test3$timestep,test3$temp.1)
qcc(test3$temp.1,type = "xbar.one")
remove = subset(test3, test3$temp.1>-0.1253973)
qcc(remove$temp.1,type = "xbar.one",newdata = test$temp.1)
