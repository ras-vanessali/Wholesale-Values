wholesaledata<-"SET NOCOUNT ON                    

			      Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
			      Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)

            Declare @topyear INT = 2020
            Declare @compingyr INT = @topyear+1
					
					  Declare @ext_botYr INT = @compingyr-12
			  
                    SELECT 
					 CustomerAssetId AS CompId
                    ,[EquipmentTypeId]
                    ,[CategoryId]
                    ,[CategoryName]
                    ,[SubcategoryId]
                    ,[SubcategoryName]
                    ,[MakeId]
                    ,[MakeName]
                    ,[ModelId]
                    ,[ModelName]
                    ,[ModelYear]
                    ,SaleDate
                    ,EOMONTH(SaleDate) as EffectiveDate
                    ,[SalePriceSF] as [SalePrice]
                    ,SaleType
                    ,MilesHours
                    ,MilesHoursCode
                    ,[M1AppraisalBookPublishDate]
                    
                    ,(M1PrecedingFmv+M1PrecedingFlv)/2  AS M1Value
                    ,CurrentABCostUSNA as CurrentABCost
                    ,[SalePriceSF]/CurrentABCostUSNA AS [SaleAB]
                    ,SalePriceSF/((M1PrecedingFmv+M1PrecedingFlv)/2) AS SPvalue
                    
                    ,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,4))  as Age
                   ,'USA' as Country
                

                    FROM [ras_sas].[BI].[Comparables]
                    
                    WHERE SaleType ='dealer' AND IsUsedForComparablesUSNA='Y'
                    AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
                    AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed
				          	AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')  
				          	AND NOT (modelid = 40413)
                    AND SaleDate >@dateStart AND saledate<=@dateEnd 
                    --and categoryid in (2606,2612,30,2515,2616)
                    AND ModelYear <= @compingyr 
                    and ModelYear>= @ext_botYr
                    AND [SalePriceSF]>100
                    AND CurrentABCostUSNA is not NULL 
                    AND M1PrecedingFmv IS NOT NULL
                    AND M1PrecedingFlv IS NOT NULL
                    AND M1PrecedingABCostUSNA is not NULL
                    AND Option15 is NULL"

sched.cat<-"Declare @topyear INT = 2020
Declare @effectdate DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
Declare @lastpublish DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)
Declare @ext_botYr INT = @topyear-11
  SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[AppraisalBookPublishDate] as M1AppraisalBookPublishDate
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as fmv
      ,[FlvSchedulePercentage]/100 as flv
      
  FROM [ras_sas].[BI].[AppraisalBookClassificationValuesUSNA]
  where SubcategoryId is null AND [CategoryId] is not null
  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
  AND [AppraisalBookPublishDate]>=@effectdate and AppraisalBookPublishDate<=@lastpublish
  AND ModelYear>=@ext_botYr AND ModelYear<=@topyear"

sched.subcat<-"Declare @topyear INT = 2020
Declare @effectdate DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
Declare @lastpublish DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)
Declare @ext_botYr INT = @topyear-11
  SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[AppraisalBookPublishDate] as M1AppraisalBookPublishDate
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as fmv
      ,[FlvSchedulePercentage]/100 as flv
      
  FROM [ras_sas].[BI].[AppraisalBookClassificationValuesUSNA]
  where MakeId is null AND SubcategoryId is not null
  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
  AND SubcategoryName NOT LIKE 'DO NOT USE%'
  AND [AppraisalBookPublishDate]>=@effectdate and AppraisalBookPublishDate<=@lastpublish
  AND ModelYear>=@ext_botYr AND ModelYear<=@topyear"

sched.make<-" Declare @topyear INT = 2020
Declare @effectdate DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
Declare @lastpublish DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)
Declare @ext_botYr INT = @topyear-11	
SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[AppraisalBookPublishDate] as M1AppraisalBookPublishDate
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as fmv
      ,[FlvSchedulePercentage]/100 as flv
      
  FROM [ras_sas].[BI].[AppraisalBookClassificationValuesUSNA]
  where ModelId is null AND MakeId is not null
  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
  AND [AppraisalBookPublishDate]>=@effectdate and AppraisalBookPublishDate<=@lastpublish
  AND ModelYear>=@ext_botYr AND ModelYear<=@topyear"

cat.inprog<-"Declare @topyear INT = 2020
Declare @ext_botYr INT = @topyear-11

  SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as fmv
      ,[FlvSchedulePercentage]/100 as flv
      
  FROM [ras_sas].[BI].[ClassificationValuesInProgressUSNA]
  where SubcategoryId is null AND [CategoryId] is not null
  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
  AND ModelYear>=@ext_botYr AND ModelYear<=@topyear"


subcat.inprog<-"Declare @topyear INT = 2020
Declare @ext_botYr INT = @topyear-11
  SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as fmv
      ,[FlvSchedulePercentage]/100 as flv
      
  FROM [ras_sas].[BI].[ClassificationValuesInProgressUSNA]
  where MakeId is null AND SubcategoryId is not null
  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
  AND SubcategoryName NOT LIKE 'DO NOT USE%'
  AND ModelYear>=@ext_botYr AND ModelYear<=@topyear"


make.inprog<-"
Declare @topyear INT = 2020
Declare @ext_botYr INT = @topyear-11	
SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[ModelYear]
      ,[FmvSchedulePercentage]/100 as fmv
      ,[FlvSchedulePercentage]/100 as flv
      
  FROM [ras_sas].[BI].[ClassificationValuesInProgressUSNA]
  where ModelId is null AND MakeId is not null
  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
  AND ModelYear>=@ext_botYr AND ModelYear<=@topyear"



### import a table with all classifications
AllClass.sql<-"
SET NOCOUNT ON
Drop TABLE IF EXISTS #CSlevel
  SELECT [ClassificationId]
      ,[CategoryId]
      ,[SubcategoryId]
  INTO #CSlevel
  FROM [ras_sas].[BI].[Classifications]
  where MakeId IS NULL AND SubcategoryId IS NOT NULL
  
Drop TABLE IF EXISTS #Clevel
SELECT [ClassificationId]
      ,[CategoryId]
  INTO #Clevel
  FROM [ras_sas].[BI].[Classifications]
  where SubcategoryId IS NULL AND CategoryId IS NOT NULL


SELECT BIC.[ClassificationId]
      ,BIC.[CategoryId]
      ,BIC.[CategoryName]
      ,BIC.[SubcategoryId]
      ,BIC.[SubcategoryName]
      ,BIC.[MakeId]
      ,BIC.[MakeName]
	  ,CSL.[ClassificationId] CS_ClassId
	  ,CL.[ClassificationId] C_ClassId
  FROM (select * from [ras_sas].[BI].[Classifications] where ModelId IS NULL And NOT(Categoryid IN (220,1948,21) OR CategoryName LIKE 'DO NOT USE%')) BIC
  LEFT JOIN #CSlevel  CSL
  on BIC.CategoryId = CSL.CategoryId AND BIC.SubcategoryId = CSL.SubcategoryId
  LEFT JOIN #Clevel CL
  on BIC.CategoryId = CL.CategoryId
  order by BIC.[CategoryName]
      ,BIC.[SubcategoryName]
      ,BIC.[MakeName]"



 



