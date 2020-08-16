
### This process starts from effective date 2019-02-28 and will end in 2020-09-30. ###
EOMList<-seq(as.Date('2019-03-01'),length=20,by='1 month') -1
str <-sort(c(0:9,0:9))
CatDtUse <-data.frame(EOMList,str)
indexUse<- CatDtUse[CatDtUse$EOMList==publishDate,]$str

################### read the tabs from excel ##########################
setwd(file_path) 
In<-data.frame(read.xlsx(excelfile,sheetName='In')) %>% filter(Country==CountryCode) %>% select(Schedule,CategoryId,SubcategoryId,MakeId,Country,Level2)
InR<-data.frame(read.xlsx(excelfile,sheetName='InR'))  %>% filter(Country==CountryCode) %>% select(Schedule,CategoryId,SubcategoryId,MakeId,Country,Level2)
combIn<-rbind(In,InR)

Out<-data.frame(read.xlsx(excelfile,sheetName='Out')) %>% filter(Country==CountryCode)
OutR<-data.frame(read.xlsx(excelfile,sheetName='OutR')) %>% filter(Country==CountryCode) 
### Application tab
comb_Out<-rbind(Out %>% select(ClassificationId, Schedule,Plot,Level2),OutR %>% select(ClassificationId, Schedule, Plot,Level2))


### load the Sched file
Sched<-data.frame(read.xlsx(excelfile,sheetName='Sched',startRow=6)) %>% filter(Country==CountryCode) %>% select(Schedule,RetailNewYrMin, RetailNewYrMax, AuctionNewYrMin, AuctionNewYrMax) 
SchedR<-data.frame(read.xlsx(excelfile,sheetName='SchedR',startRow=6)) %>% filter(Country==CountryCode) %>% select(Schedule,RetailNewYrMin, RetailNewYrMax, AuctionNewYrMin, AuctionNewYrMax,BorrowSchedule,BorrowType) 
SchedFullList<-rbind(Sched,SchedR %>% select(-BorrowSchedule,-BorrowType))

SchedFullList <- SchedFullList%>% select(Schedule)


################################ Clean the table and data ########################################## 
## split the input file into three levels
Catlevel<-combIn %>% filter(Level2 =='Category') %>% select(Schedule,CategoryId,Country)
Subcatlevel<-combIn %>% filter(Level2 == "SubcatGroup") %>% select(Schedule,CategoryId,SubcategoryId,Country)
Makelevel<-combIn %>% filter(Level2 =='Make') %>% select(Schedule,CategoryId,SubcategoryId,MakeId,Country)

## join the above three levels sub table each level schedules
cat_mapsched <-merge(schedule.cat,Catlevel,by='CategoryId') %>% select(CategoryId,M1AppraisalBookPublishDate,ModelYear,fmv,flv,Schedule,Country)
subcat_mapsched <-merge(schedule.subcat,Subcatlevel,by=c('CategoryId','SubcategoryId'))%>% select(CategoryId,SubcategoryId,M1AppraisalBookPublishDate,ModelYear,fmv,flv,Schedule,Country)
make_mapsched <-merge(schedule.make,Makelevel,by=c('CategoryId','SubcategoryId','MakeId'))%>% select(CategoryId,SubcategoryId,MakeId,M1AppraisalBookPublishDate,ModelYear,fmv,flv,Schedule,Country)

## join the in progress schedules to above three levels sub table each level schedules
cat_mapto_inprog <-merge(inprog.cat,Catlevel,by='CategoryId') %>% select(CategoryId,ModelYear,fmv,flv,Schedule,Country)
subcat_mapto_inprog <-merge(inprog.subcat,Subcatlevel,by=c('CategoryId','SubcategoryId'))%>% select(CategoryId,SubcategoryId,ModelYear,fmv,flv,Schedule,Country)
make_mapto_inprog <-merge(inprog.make,Makelevel,by=c('CategoryId','SubcategoryId','MakeId'))%>% select(CategoryId,SubcategoryId,MakeId,ModelYear,fmv,flv,Schedule,Country)


select.var<-c('CompId',	'CategoryId',	'CategoryName',	'SubcategoryId',	'SubcategoryName',	'MakeId',	'MakeName',	'ModelId',	'ModelName',	
              'ModelYear',	'SaleDate',	'EffectiveDate',	'SalePrice',	'M1Value',	'SaleType',	'M1AppraisalBookPublishDate',	'SaleAB',	
              'SPvalue',	'CurrentABCost',	'Age',	'Schedule','fmv','flv') 
## validation on mapping
if(
  dim(cat_mapsched)[1] == dim(Catlevel)[1]*12*12 &
  dim(subcat_mapsched)[1] == dim(Subcatlevel)[1]*12*12 &
  dim(make_mapsched)[1] == dim(Makelevel)[1]*12*12){
  print("Mapping works good")
} else{print("Check needs on mapping")}

## join the data and combine the three levels into one giant table

CatData <- merge(wholesales,cat_mapsched, by=c("CategoryId","Country","M1AppraisalBookPublishDate","ModelYear")) %>% 
  mutate(str = str_sub(CompId,-1)) %>%
  filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
  select(all_of(select.var)) 

SubcatData <- merge(wholesales,subcat_mapsched, by=c('CategoryId',"SubcategoryId","Country","M1AppraisalBookPublishDate","ModelYear")) %>% 
  mutate(str = str_sub(CompId,-1)) %>%
  filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
  select(all_of(select.var)) 

MakeData<-merge(wholesales,make_mapsched, by=c('CategoryId',"SubcategoryId","MakeId","Country","M1AppraisalBookPublishDate","ModelYear")) %>% 
  select(all_of(select.var)) 



## assign flv, fmv and schedule name to dealer data
Datainput.ws<-rbind(CatData,SubcatData,MakeData) %>%
  mutate(CompId = factor(CompId)) %>%
  mutate(factor.ind = (SaleAB-flv)/(fmv-flv))%>%
  group_by(Schedule) %>%
  filter(SPvalue <= mean(SPvalue) + stdInd*sd(SPvalue) & SPvalue>= mean(SPvalue) - stdInd*sd(SPvalue)) 

## calc new year factor
ws.newy<-Datainput.ws %>% 
  filter(Age<=ny.max) %>%
  group_by(Schedule) %>%
  summarise(age = mean(Age),
            n=n(),
            factor = mean(factor.ind))

ws.newy.full<-merge(SchedFullList,ws.newy,all.x=T,by='Schedule')

nyinherit<-inherit.fact(ws.newy.full) %>% select(-CS_ClassId,-C_ClassId) %>%
  rename(factor.ny=factor,
         n.ny=n)


## calc old year factor
ws.oldy<-Datainput.ws %>% 
  filter(Age>=oy.min & Age<=oy.max) %>%
  group_by(Schedule) %>%
  summarise(age = mean(Age),
            n=n(),
            factor = mean(factor.ind))
ws.oldy.full<-merge(SchedFullList,ws.oldy,all.x=T,by='Schedule')

oyinherit<-inherit.fact(ws.oldy.full) %>% select(-CS_ClassId,-C_ClassId)%>%
  rename(factor.oy=factor,
         n.oy=n)



## join the old and new
new.old.join<-merge(nyinherit,oyinherit,by='ClassificationId')

## global values
globalvalue<-data.frame(ClassificationId = GlobalClassId,
                        factor.ny = globvalue(ws.newy.full),
                        n.ny = 100,
                        factor.oy = globvalue(ws.oldy.full),
                        n.oy=100)

## cap the factor
cap.factor<-rbind(globalvalue,new.old.join) %>%
  mutate(ny.cap = pmax(pmin(factor.ny,maxf),minf),
         oy.cap = pmax(pmin(factor.oy,maxf),minf))

## import table
importtable<-cap.factor %>% mutate(MarketCode = 'USNA') %>%
  rename(WholesalesFactorNY = ny.cap, WholesalesFactorOY = oy.cap) %>%
  select(MarketCode,ClassificationId,WholesalesFactorNY,WholesalesFactorOY) 
  
dim(importtable)[1] == dim(comb_Out)[1]

plotscheds<-merge(comb_Out,cap.factor,by='ClassificationId',all.x=T) %>%
  filter(Plot == 'Y') %>%
  mutate(plotflag.ny = ifelse(n.ny<=thresold_ws | is.na(n.ny), 'inherit','self'),
         plotflag.oy = ifelse(n.oy<=thresold_ws | is.na(n.oy), 'inherit','self')) %>%
  select(ClassificationId,Schedule,ny.cap,oy.cap,plotflag.ny,plotflag.oy)

#########################################################################################################################
############################################### Plots #####################################################
#########################################################################################################################
lm.schedule <- rbind(cat_mapto_inprog %>% select(Schedule,ModelYear, fmv,flv) %>% distinct(),
                     subcat_mapto_inprog %>% select(Schedule,ModelYear, fmv,flv)%>% distinct(),
                     make_mapto_inprog %>% select(Schedule,ModelYear, fmv,flv)%>% distinct()) %>%
  filter(ModelYear>=ext_botYr & ModelYear<=topyear)

cols.num<-c('ny.cap','oy.cap')
plotscheds[cols.num] <- sapply(plotscheds[cols.num],as.numeric)
## interpolate the whole sales factor by year
wsfact.byyear<-merge(lm.schedule,plotscheds,by='Schedule') %>%
  mutate(YearAge = topyear - ModelYear) %>%
  arrange(Schedule,desc(ModelYear)) %>%
  mutate(factor.year = ifelse(ModelYear<botyear, oy.cap, ny.cap + YearAge *((oy.cap-ny.cap)/9)))


dealers.sched.self = wsfact.byyear %>%
  mutate(olv = fmv *factor.year + flv *(1-factor.year)) %>%
  mutate(Age= year(priorMonth)-ModelYear + (month(priorMonth)-6)/12.00 )

SchedFullList_plot<-plotscheds %>% filter(!str_detect(Schedule,'SubcatGrp')) %>% select(Schedule)
 
###################################### Plots ####################################
N_plot=dim(SchedFullList_plot)[1]
for (j in 1:N_plot){


  ###### Dots
  dataplot.inuse <- Datainput.ws %>% filter(as.character(Schedule) == as.character(SchedFullList_plot$Schedule[j])) %>%
    filter(Age<=ny.max | (Age >=oy.min & Age<=oy.max))
  dataplot.other <- Datainput.ws %>% filter(as.character(Schedule) == as.character(SchedFullList_plot$Schedule[j])) %>%
    filter(Age>ny.max & Age<oy.min)
  
  ###### Lines
  scheduleplot<-subset(dealers.sched.self,dealers.sched.self$Schedule==as.character(SchedFullList_plot[j,1]))
  
  ws.ny.point<-scheduleplot %>% filter(ModelYear == topyear)
  ws.oy.point<-scheduleplot %>% filter(ModelYear == botyear)
  
  xaxis = c(-.5,12)
  yaxis = c(0,2)
  
  yaxis_name='SP /AB Cost'
  ##Auction Plots
  # lines
  draw_Sched_Auc<-xyplot(flv ~ Age, scheduleplot,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
                         ,type=c('p','l','g'),col='dodgerblue3',lwd=3,cex=1.5, lty=1,pch=20
                         ,ylab = yaxis_name, main=list(label=paste(SchedFullList_plot[j,1],' - ' ,publishDate),font=2,cex=2)
                         ,panel = function(x,y,...){
                           panel.abline(v = 0:14, h = seq(0,2,.5), col="lightgrey")
                           panel.xyplot(x, y, ...)
                         })
  draw_Sched_Ret<-xyplot(fmv ~ Age, scheduleplot,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
                         ,type=c('p','l','g'),col='firebrick2',lwd=3,cex=1.5,lty=1,pch=20,
                         ylab = yaxis_name,main=list(label=paste(SchedFullList_plot[j,1],' - ' ,publishDate),font=2,cex=2))
  
  draw_Sched_dealer<-xyplot(olv ~ Age, scheduleplot,ylim=yaxis,xlim=xaxis,scales = list(y=list(tick.number=10),x=list(tick.number=16))
                         ,type=c('p','l','g'),col='darkgoldenrod2',lwd=3,cex=1.5,lty=1,pch=20,
                         ylab = yaxis_name,main=list(label=paste(SchedFullList_plot[j,1],' - ' ,publishDate),font=2,cex=2))
   #dots
  
  draw_dealer_inuse<-xyplot(SaleAB ~ Age, dataplot.inuse,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="darkgoldenrod3")
  draw_dealer_other<-xyplot(SaleAB ~ Age, dataplot.other,pch=20,cex=1.5,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="darkgoldenrod1")
  
  if (ws.ny.point$plotflag.ny=='self'){
    draw_nypoint<-xyplot(olv ~ Age, ws.ny.point,pch=15,cex=3,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="darkgoldenrod3")
  } else{
    draw_nypoint<-xyplot(olv ~ Age, ws.ny.point,pch=0,cex=3,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="darkgoldenrod3")
  }
  
  if (ws.oy.point$plotflag.oy=='self'){
    draw_oypoint<-xyplot(olv ~ Age, ws.oy.point,pch=15,cex=3,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="darkgoldenrod3")
  } else{
    draw_oypoint<-xyplot(olv ~ Age, ws.oy.point,pch=0,cex=3,ylim=yaxis,xlim=xaxis,ylab = yaxis_name,col="darkgoldenrod3")
  }
  
  ### Draw retail and auction in one chart

  draw<- draw_Sched_Auc + as.layer(draw_Sched_Ret)  +as.layer(draw_dealer_other) +as.layer(draw_dealer_inuse)+ as.layer(draw_Sched_dealer) + as.layer(draw_nypoint) + as.layer(draw_oypoint) 

  #### save
  mypath<-file.path(file_path,plotFolder,paste(SchedFullList_plot[j,1] ,".png"))
  png(file=mypath,width=1600,height=1200)
  print(draw)
  dev.off()
  
  
}



