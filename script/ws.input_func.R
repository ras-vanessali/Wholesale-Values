
## set the index of standard deviation side of mean 
stdInd =2
## year age in use
ny.max = 2
oy.min = 7 
oy.max = 11

## threshold of number of sales 
thresold_ws = 8

## cap the min and max factors
minf = .1
maxf = .95

topyear = 2020 ## top model year
botyear = topyear-9 ## bottom model year
ext_botYr = topyear-11 ## bottom model year of putting in regression

## Global category list
GlobalList<-c(313,	6,	2509,	15,	29,	315,	360,	451,	316,	362)
GlobalClassId=1

split.joinlevel<-function(input,dataload){
  
  select.var<-c('CompId',	'CategoryId',	'CategoryName',	'SubcategoryId',	'SubcategoryName',	'MakeId',	'MakeName',	'ModelId',	'ModelName',	
                'ModelYear',	'SaleDate',	'EffectiveDate',	'SalePrice',	'M1Value',	'SaleType',	'M1AppraisalBookPublishDate',	'SaleAB',	
                'SPvalue',	'CurrentABCost',	'Age',	'Schedule','fmv','flv') 
  
  ## split the input file into three levels
  Catlevel<-input %>% filter(Level2 =='Category') %>% select(Schedule,CategoryId,Country)
  Subcatlevel<-input %>% filter(Level2 == "SubcatGroup") %>% select(Schedule,CategoryId,SubcategoryId,Country)
  Makelevel<-input %>% filter(Level2 =='Make') %>% select(Schedule,CategoryId,SubcategoryId,MakeId,Country)
  
  ## join the above three levels sub table each level schedules
  cat_mapsched <-merge(schedule.cat,Catlevel,by='CategoryId') %>% select(CategoryId,M1AppraisalBookPublishDate,ModelYear,fmv,flv,Schedule,Country)
  subcat_mapsched <-merge(schedule.subcat,Subcatlevel,by=c('CategoryId','SubcategoryId'))%>% select(CategoryId,SubcategoryId,M1AppraisalBookPublishDate,ModelYear,fmv,flv,Schedule,Country)
  make_mapsched <-merge(schedule.make,Makelevel,by=c('CategoryId','SubcategoryId','MakeId'))%>% select(CategoryId,SubcategoryId,MakeId,M1AppraisalBookPublishDate,ModelYear,fmv,flv,Schedule,Country)
  
  ## validation on mapping
  if(
    dim(cat_mapsched)[1] == dim(Catlevel)[1]*12*12 &
    dim(subcat_mapsched)[1] == dim(Subcatlevel)[1]*12*12 &
    dim(make_mapsched)[1] == dim(Makelevel)[1]*12*12){
    print("Mapping works good")
  } else{print("Check needs on mapping")}
  
  ## join the data and combine the three levels into one giant table
  
  CatData <- merge(dataload,cat_mapsched, by=c("CategoryId","Country","M1AppraisalBookPublishDate","ModelYear")) %>% 
    mutate(str = str_sub(CompId,-1)) %>%
    filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
    select(all_of(select.var)) 
  
  SubcatData <- merge(dataload,subcat_mapsched, by=c('CategoryId',"SubcategoryId","Country","M1AppraisalBookPublishDate","ModelYear")) %>% 
    mutate(str = str_sub(CompId,-1)) %>%
    filter(MakeId !=31 | (MakeId ==31 & str<=indexUse)) %>% 
    select(all_of(select.var)) 
  
  MakeData<-merge(dataload,make_mapsched, by=c('CategoryId',"SubcategoryId","MakeId","Country","M1AppraisalBookPublishDate","ModelYear")) %>% 
    select(all_of(select.var)) 
  
  
  return(rbind(CatData,SubcatData,MakeData))}



inherit.fact<-function(df){
  
  col<-c('ClassificationId','factor','CS_ClassId', 'C_ClassId','n')
  
  glob <- merge(merge(df,comb_Out,by='Schedule'), AllClass,by='ClassificationId') %>%
    filter(CategoryId %in% GlobalList & Level2 =='Category' & !is.na(factor)) %>%
    summarise(mean(factor))
  
  
  start.df<-merge(merge(df,comb_Out,by='Schedule'), AllClass,by='ClassificationId') %>% select(col)
  
  non_inherit.df <- start.df %>% filter(!(is.na(factor) | n<=thresold_ws)) 
  
  # M level inherit CS level
  m.ih.cs <-merge(start.df %>% filter((is.na(factor) | n<=thresold_ws) & !is.na(CS_ClassId)) %>% select(ClassificationId,CS_ClassId,C_ClassId,n)
                  ,non_inherit.df %>% select(c('ClassificationId','factor')),by.x='CS_ClassId',by.y='ClassificationId') %>% select(col)
  
  # CSlevel inherit C level 
  cs.ih.c<- merge(anti_join(start.df,rbind(non_inherit.df,m.ih.cs),by='ClassificationId') %>% select(-factor)
                  ,non_inherit.df %>% select(c('ClassificationId','factor')),by.x='C_ClassId',by.y='ClassificationId') %>% select(col)
  
  # remaining inherit global
  ih.glob<- anti_join(start.df,rbind(non_inherit.df,m.ih.cs,cs.ih.c),by='ClassificationId') %>%
    mutate(factor = as.factor(glob))
  
  return(rbind(non_inherit.df,m.ih.cs,cs.ih.c,ih.glob))}

globvalue<-function(df){
  glob <- merge(merge(df,comb_Out,by='Schedule'), AllClass,by='ClassificationId') %>%
    filter(CategoryId %in% GlobalList & Level2 =='Category' & !is.na(factor)) %>%
    summarise(mean(factor))
  return(as.numeric(glob))
}
