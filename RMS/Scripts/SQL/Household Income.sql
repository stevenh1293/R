SELECT  CONCAT(ROUND(COUNT(Income) / SUM(COUNT(Income)) over() *100),'%') AS Percent,
	CASE WHEN Income <= 50000 AND Income <> 0 THEN 'Under 50k'
    WHEN Income > 50000 AND Income <= 100000 THEN '50k to 100k'
    WHEN Income > 100000 THEN 'Over 100k'
    ELSE 'No Answer' END AS Income_Range
FROM Data
GROUP BY Income_Range;
