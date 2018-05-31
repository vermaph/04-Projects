-----------------------------------
--Author: Piyush Verma
--Code: Customer Health Dashboard
--Date Modified: 04/14/2018
--Server: SCOTT\SQLEXPRESS
--Database: SUPERMARKET
-----------------------------------


------------------------------------
--Tab: Executive Summary
------------------------------------

---Sales: Total Sales, Total Units sold
--CREATE TABLE EXEC_SUMM_SALES ( 
--WEEK CHAR(10),
--SALES DECIMAL(34,2),
--UNITS INT
--);
TRUNCATE TABLE EXEC_SUMM_SALES; 
INSERT INTO EXEC_SUMM_SALES
SELECT 
A.WK AS WEEK,
SUM(A.SALES) AS SALES,
SUM(A.UNITS) AS UNITS
FROM
(
SELECT
CASE 
WHEN SV.DAY BETWEEN 12-7+1 AND 12 THEN 'THIS_WEEK' 
WHEN SV.DAY BETWEEN 12-2*7+1 AND 12-7 THEN 'LAST_WEEK'
ELSE 'NULL'
END AS WK,
SV.TRANS_VALUE AS SALES,
SV.QUANTITY AS UNITS
FROM [dbo].[SUPERMARKET_FILTER_VIEW] SV
WHERE SV.DAY BETWEEN 12-2*7+1 AND 12
)A
GROUP BY WK 


---Members: Total Member Visits, Total Unique Member Visits
DROP TABLE EXEC_SUMM_MEMBER;
CREATE TABLE EXEC_SUMM_MEMBER ( 
WEEK CHAR(10),
TOTAL_SHOPPERS INT,
TOTAL_VISITS INT,
);



TRUNCATE TABLE EXEC_SUMM_MEMBER --CLEARS OLD RECORDS
INSERT INTO EXEC_SUMM_MEMBER--UPDATES THE FRESH RECORDS
SELECT
WEEK,
COUNT(DISTINCT H_KEY) AS TOTAL_SHOPPERS,
COUNT(DISTINCT BASKET_ID) AS TOTAL_VISITS
FROM
(
SELECT
CASE 
WHEN SV.DAY BETWEEN 12-7+1 AND 12 THEN 'THIS_WEEK' 
WHEN SV.DAY BETWEEN 12-2*7+1 AND 12-7 THEN 'LAST_WEEK'
ELSE 'NULL'
END AS WEEK,
H_KEY,
BASKET_ID
FROM [dbo].[SUPERMARKET_FILTER_VIEW] SV
WHERE SV.DAY BETWEEN 12-2*7+1 AND 12
)A
GROUP BY WEEK



------------------------------------
--Tab: Sales Detail (Customer)
------------------------------------


------------------------------------
--Tab: Sales Detail (Product)
------------------------------------



------------------------------------
--Tab: Charts
------------------------------------