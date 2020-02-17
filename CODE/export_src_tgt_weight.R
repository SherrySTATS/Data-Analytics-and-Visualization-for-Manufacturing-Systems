library(data.table)
library(dplyr)


#imports for plotting
require(GGally)
library(network)
library(sna)
library(ggplot2)
require(igraph)
require(intergraph)
library(stringr)

##########################################DONOT RUN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#topFeaturesDate <- c("L3_S33_D3856","L3_S34_D3875","L3_S30_D3496","L3_S29_D3316","L3_S37_D3942",
                    # "L3_S29_D3474","L0_S0_D1" ,    "L3_S30_D3506","L3_S30_D3501","L3_S30_D3726",
                    #  "L3_S30_D3566","L0_S3_D70" ,   "L3_S35_D3895","L0_S8_D145"  , "L2_S26_D3037",
                    #  "L3_S35_D3886","L3_S35_D3900","L0_S13_D355" , "L3_S36_D3919","L0_S1_D26"   ,
                    #  "L3_S36_D3928","L2_S26_D3081","L1_S24_D1511","L0_S12_D331" , "L1_S24_D1809",
                    #  "L0_S2_D34","L1_S24_D1536","L0_S14_D360" , "L1_S24_D1558","L0_S23_D617" ,
                    #  "L0_S6_D120","L1_S24_D1566","L1_S24_D1576","L0_S20_D462" , "L0_S4_D106"  ,
                    #  "L0_S1_D30","L0_S19_D454",  "L0_S7_D137","L0_S9_D152" ,  "L1_S24_D1826",
                    #  "L0_S16_D423","L0_S17_D432" , "L1_S24_D1568","L0_S5_D115" ,  "L1_S24_D1674",
                    #  "L1_S24_D1570","L0_S18_D437"  ,"L0_S10_D216","L0_S9_D157"  , "L1_S24_D1522",
                    #  "L0_S4_D111","L2_S27_D3130","L0_S11_D280" , "L0_S22_D543" , "L0_S9_D162"  ,
                    #  "L0_S10_D221","L0_S15_D395",  "L0_S9_D167"  , "L1_S24_D1765","L3_S31_D3836",
                    #  "L0_S22_D548","L1_S24_D1583","L0_S21_D469",  "L1_S24_D1770","L0_S10_D231" ,
                    #  "L0_S22_D553","L0_S21_D474" , "L1_S25_D1854","L3_S32_D3852",
                    #  "L3_S39_D3966","L0_S21_D484" , "L1_S25_D1867","L3_S44_D4101","L3_S40_D3981",
                    #  "L3_S38_D3953","L0_S22_D558" , "L1_S25_D2780","L3_S43_D4062","L3_S49_D4208",
                    #  "L2_S28_D3223","L1_S24_D1116","L1_S25_D2788","L3_S45_D4125","L3_S49_D4218",
                    #  "L3_S50_D4242","L3_S40_D3985","L1_S25_D2471","L1_S25_D2058","L1_S25_D1891",
                    #  "L1_S24_D677","L1_S24_D1135","L3_S43_D4082","L1_S25_D2792","L1_S25_D1883",
                    #  "L3_S41_D3997","L1_S25_D2098","L1_S25_D1887","L1_S25_D2754","L1_S25_D2138",
                    #  "L3_S51_D4255","L1_S24_D1155","L1_S25_D1980","L1_S25_D2790","L3_S47_D4140",
                    #  "L1_S25_D3011","L1_S25_D2240","L1_S25_D2230","L1_S25_D2798","L1_S25_D1902",
                    #  "L1_S24_D702","L1_S24_D697","L3_S48_D4194","L1_S25_D2996","L1_S24_D1413",
                    #  "L1_S24_D818","L1_S24_D1168","L1_S24_D1163","L1_S24_D1171","L1_S24_D804", 
                    #  "L1_S24_D909","L1_S24_D801","L1_S25_D2801","L1_S24_D1018","L1_S25_D2206","L1_S25_D2505")

#one expansion option would be to select a time frame to visualized
#dtNum <- fread("/Users/xinranshi/Dropbox/lecture/CSE6242-Project/BOSCH/Data/train_numeric.csv", select = c("Id", "Response"))
#dtDate <- fread("/Users/xinranshi/Dropbox/lecture/CSE6242-Project/BOSCH/Data/train_date.csv",select = c("Id",topFeaturesDate))
#save(dtDate, file = "/Users/xinranshi/Dropbox/lecture/CSE6242-Project/BOSCH/BOSCH/data/dtDate.RData")
#save(dtNum, file  = "/Users/xinranshi/Dropbox/lecture/CSE6242-Project/BOSCH/BOSCH/data/dtNum.RData")


################################################################################################
load(".../Rdata/dtNum.Rdata")
load(".../Rdata/dtDate.RData")
#for each job identify which stations are passed through and for those store the minimum time
for (station in paste0("S",0:51))
{
  cols = min(which((grepl(station,colnames(dtDate)))))
  if(!cols==Inf){
    dtDate[,paste0(station) := dtDate[,cols,with = FALSE]]
  }
}

#lable the station path for each part and prepare for logistic regression
dtPath = dtDate[,127:176]
dtPath[is.na(dtPath)] <- 0
dtPath[dtPath > 0] <- 1
#dtPath = as.factor(dtPath)
dtlog = cbind.data.frame(dtPath, dtNum$Response)
names(dtlog)[51] <- 'Response'

#calculate the historical fail rate for each station
fail_rate_col1 = list()
fail_rate_col2 = list()
for (i in 1:50){
  mytemp = as.matrix(dtlog)
  temp = mytemp[which(mytemp[,i]==1),]
  total = dim(temp)[1]
  fail_rate_col1[i] = names(dtlog)[i] 
  fail_rate_col2[i] = sum(temp[,51])/total
}
fail_rate = cbind.data.frame(fail_rate_col1, fail_rate_col2)
names(fail_rate) <- c("Station","Failure Rate")
#write.csv(fail_rate, "/Users/xinranshi/Dropbox/lecture/CSE6242-Project/BOSCH/BOSCH/station failure rate.csv")

dtlogID = cbind.data.frame(dtNum$Id,dtlog)
names(dtlogID)[1] <- 'ID'
row.names(dtlogID) <-as.character(dtlogID$ID)
dtlogID$ID <- NULL
UniqID =as.numeric(row.names(dtlogID)) 
#View(dtlogID[1:5,])
dtUniqPath = unique(dtlogID) #unique path and results
save(dtUniqPath, file = "dtUniqPath.RData")
#sort data by time
dtSort = cbind(dtDate$ID,dtDate[,127:176])
time = rowSums(dtDate[,127:176], na.rm = T)/rowSums(!is.na(dtDate[,127:176]))
dtSort$time = time
dtSorted = dtSort[order(time),] 
names(dtSorted)[1] <-"ID"
#omit the parts no time info
timeRange = range(time,na.rm = T)
dtSortedFilterNAtime = dtSorted[complete.cases(dtSorted$time), ]
timeOmit = as.data.frame(dtSortedFilterNAtime$time)
names(timeOmit) <- "time"
#frequence in time range
timeOmitFreq = as.character(round(timeOmit$time,2))
require(plyr)
freq = count(timeOmitFreq)
timeOmitFreq = cbind.data.frame(unique(round(timeOmit$time,2)),freq)
timeOmitFreq$x  = NULL
names(timeOmitFreq)[1] = "time"
#analysis time data
#ggplot(timeOmitFreq, aes(x=time, y=freq)) + geom_line()
#ggplot(timeOmitFreq[1:43000,], aes(x=time, y=freq)) + geom_line()
###from the plot, we can see two cycles
TimeSplit = timeOmitFreq$time[43000]
#Slice the data into 8 sets
breakPoint = c(1,147895,295791,443686,591582,739477,887372,1035267,1183165)
dtbreaked = list()
for (i in 1:8){
  a = breakPoint[i]
  b = breakPoint[i+1]
  dtbreaked[[i]] = dtSortedFilterNAtime[a:b,1:51] #split the data to 8 groups 
  #assign(paste("dtSortedFilterNAtime_", i, sep = ""), temp)
  }
#limit data to only when passed through station X
  dtDate = dtbreaked[[i]]
  dtStations = dtDate[,!grepl("L",colnames(dtDate)),with=F]

#melt data to go from wide to long format
#join with numeric to have Response
#remove NA entries - these are plentiful as after melting each station-job combination has its own row

melt(dtStations,id.vars=c("Id")) %>%
  left_join(dtNum, by = "Id")  %>%
  filter(!is.na(value)) -> dtStationsMFiltered

dtStationsMFiltered %>%
  group_by(variable) %>%
  summarize(stationCount=n()) -> stationCount


stationCount = rbind(stationCount,data.frame(variable=c("result-0","result-1"),
                                             stationCount=c(table(dtNum$Response)[1],
                                                            table(dtNum$Response)[2])))

colnames(stationCount) = c("Node","stationCount")


#sort entries by ascending time
dtStationsMFiltered %>%
  arrange(value) -> dtStationsMFiltered



dtStationsMFiltered %>%
  group_by(Id) %>%
  mutate(nextStation = lead(variable)) -> edgelistsComplete

rm(dtStations)
rm(dtStationsMFiltered)
rm(dtNum)
invisible(gc())

edgelistsComplete %>%
  group_by(Id) %>%
  filter(!(variable %in% nextStation)) -> startingPoints


startingPoints %>%
  mutate(nextStation = variable) -> startingPoints

startingPoints$variable = as.character(startingPoints$variable)
startingPoints$nextStation = as.character(startingPoints$nextStation)

startingPoints$variable = "start"

edgelistsComplete %>%
  group_by(Id) %>%
  filter(!(nextStation %in% variable)) -> endPoints

endPoints$variable = as.character(endPoints$variable)

endPoints$nextStation = paste0("result-",endPoints$Response)

edgelistsComplete$variable = as.character(edgelistsComplete$variable)
edgelistsComplete$nextStation = as.character(edgelistsComplete$nextStation)

edgelistsComplete %>% filter(nextStation!="NA") -> edgelistsComplete

dataPlot = rbind(startingPoints,edgelistsComplete,endPoints)
dataPlot$variable = as.factor(dataPlot$variable)
dataPlot$nextStation = as.factor(dataPlot$nextStation)

rm(endPoints)
rm(startingPoints)
invisible(gc())

dataPlot$edge = paste0(dataPlot$variable,"%",dataPlot$nextStation)

dataPlot %>%
  group_by(edge) %>%
  summarize(weight = n()) -> plotDataSummarized


plotDataSummarized %>%
  mutate(Source = str_split_fixed(edge,"%",2)[,1],
         Target = str_split_fixed(edge,"%",2)[,2]) %>%
  select(-edge) -> plotDataSummarized


plotDataSummarized %>%
  select(Source,Target,weight) -> plotDataSummarized


#create a station-line mapping lookup
LineStations = NULL
for (station in unique(plotDataSummarized$Source)){
  if(station!="start")
  {
    x=paste0("_",station,"_")
    y=head(colnames(dtDate)[which(grepl(x,colnames(dtDate)))],1)
    y=strsplit(y,"_")[[1]][1]
    LineStations = rbind(LineStations,data.frame(Node=station,Line=y))
  }
}
LineStations = rbind(LineStations,data.frame(Node=c("result-1","result-0","start"),Line=c("Outcome","Outcome","START")))

#plotting format
#options(repr.plot.width=15, repr.plot.height=5)


#create network from edgelist
plotDataSummarized$weight = plotDataSummarized$weight / 100000
save(plotDataSummarized, file=("src_tgt_weight.csv")

