USE SUPERMARKET;

--------------------------------------------------------------------------
--PART: DATA MASSAGING
--1) Changing Datatypes, Dropping unnecessary columns;
--[dbo].[transaction_data]
--------------------------------------------------------------------------

EXEC sp_rename '[dbo].[transaction_data].[household_key]', 'H_KEY';

ALTER TABLE [dbo].[transaction_data] ALTER COLUMN H_KEY INT;
ALTER TABLE [dbo].[transaction_data] ALTER COLUMN [BASKET_ID] BIGINT;
ALTER TABLE [dbo].[transaction_data] ALTER COLUMN [DAY] INT;
ALTER TABLE [dbo].[transaction_data] ALTER COLUMN [PRODUCT_ID] INT;
ALTER TABLE [dbo].[transaction_data] ALTER COLUMN [QUANTITY] INT;
ALTER TABLE [dbo].[transaction_data] ALTER COLUMN [STORE_ID] INT;
ALTER TABLE [dbo].[transaction_data] ALTER COLUMN [WEEK_NO] INT;
ALTER TABLE [dbo].[transaction_data] DROP COLUMN [COUPON_DISC]; --Dropping columns which wont be used
ALTER TABLE [dbo].[transaction_data] DROP COLUMN [COUPON_MATCH_DISC]; --Dropping columns which wont be used

--------------------------------------------------------------------------
--2) Removing garbage values
--------------------------------------------------------------------------

UPDATE [dbo].[transaction_data] 
SET [TRANS_TIME] = CONCAT(LEFT([TRANS_TIME],2),':',RIGHT([TRANS_TIME],2),':00') --Modifying Time column to be more readble
WHERE [BASKET_ID] IS NOT NULL;

DELETE FROM [dbo].[transaction_data] 
WHERE LEN(SALES_VALUE) = 12

DELETE FROM [dbo].[transaction_data] 
WHERE LEN([RETAIL_DISC]) = 12

ALTER TABLE [dbo].[transaction_data] ALTER COLUMN SALES_VALUE NUMERIC(12,2);
ALTER TABLE [dbo].[transaction_data] ALTER COLUMN [RETAIL_DISC] NUMERIC(12,2);

--[dbo].[product]
ALTER TABLE [dbo].[product] ALTER COLUMN [PRODUCT_ID] INT;

--[dbo].[hh_demographic]
EXEC SP_RENAME '[dbo].[hh_demographic].household_key','H_KEY';
ALTER TABLE [dbo].[hh_demographic] ALTER COLUMN [H_KEY] INT;

------------------------------------------------------------------------------------------------------
--Creating a base dataset to work on: 
--1) Clubbing all three datasets
--2) Introducing 3 new columns calculating yearly sales and visits and also sales per visits value 
--DROP VIEW SUPERMARKET_FILTER_VIEW;
------------------------------------------------------------------------------------------------------

CREATE VIEW SUPERMARKET_FILTER_VIEW AS

SELECT 
T.H_KEY
,T.[BASKET_ID]
,T.[DAY]
,T.[PRODUCT_ID]
,T.[STORE_ID]

,D.[AGE_DESC]
,D.[MARITAL_STATUS_CODE]
,D.[INCOME_DESC]
,D.[HOMEOWNER_DESC]
,D.[HH_COMP_DESC]
,D.[HOUSEHOLD_SIZE_DESC]
,D.[KID_CATEGORY_DESC]

,P.[DEPARTMENT]
,P.[BRAND]
,P.[MANUFACTURER]

,T.[QUANTITY]
,T.[SALES_VALUE] AS TRANS_VALUE

,SUM(T.[SALES_VALUE]) OVER(PARTITION BY T.H_KEY) AS FAMILY_TOT_SALES
,DENSE_RANK() OVER(PARTITION BY T.H_KEY ORDER BY T.BASKET_ID) + DENSE_RANK() OVER(PARTITION BY T.H_KEY ORDER BY T.BASKET_ID DESC) - 1  AS FAMILY_TOT_VISITS
,CAST(SUM(T.[SALES_VALUE]) OVER(PARTITION BY T.H_KEY)/(DENSE_RANK() OVER(PARTITION BY T.H_KEY ORDER BY T.BASKET_ID) + DENSE_RANK() OVER(PARTITION BY T.H_KEY ORDER BY T.BASKET_ID DESC) - 1) AS DECIMAL(10,4)) AS  FAMILY_VALUE

FROM [dbo].[transaction_data] T 
INNER JOIN [dbo].[hh_demographic] D
ON T.H_KEY = D.H_KEY
INNER JOIN  [dbo].[product] P
ON P.PRODUCT_ID = T.PRODUCT_ID
WHERE T.DAY<366 ---1 year of data
--AND T.H_KEY = 1
;

-----------------------------------------------------------------------------------------------------
-- Code to retrive the sales and visits details at the household level and for using the clustering
-- filtering results for the products which were bought by more than 25 families in given 1 year
-----------------------------------------------------------------------------------------------------

SELECT 
[H_KEY]
,[AGE_DESC]
,[MARITAL_STATUS_CODE]
,[INCOME_DESC]
,[HOMEOWNER_DESC]
,[HH_COMP_DESC]
,[HOUSEHOLD_SIZE_DESC]
,[KID_CATEGORY_DESC]
,[FAMILY_TOT_SALES]
,[FAMILY_TOT_VISITS]
,[FAMILY_VALUE]
FROM [SUPERMARKET].[dbo].[SUPERMARKET_FILTER_VIEW]
WHERE [PRODUCT_ID] in (
						SELECT [PRODUCT_ID]
						FROM [SUPERMARKET].[dbo].[SUPERMARKET_FILTER_VIEW]
						group by [PRODUCT_ID]
						HAVING COUNT(DISTINCT [H_KEY]) >=25
						)
GROUP BY   
[H_KEY]
,[AGE_DESC]
,[MARITAL_STATUS_CODE]
,[INCOME_DESC]
,[HOMEOWNER_DESC]
,[HH_COMP_DESC]
,[HOUSEHOLD_SIZE_DESC]
,[KID_CATEGORY_DESC]
,[FAMILY_TOT_SALES]
,[FAMILY_TOT_VISITS]
,[FAMILY_VALUE]