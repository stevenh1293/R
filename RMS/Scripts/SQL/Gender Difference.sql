SELECT Gender, CONCAT(ROUND(COUNT(Gender) / SUM(COUNT(Gender)) over() *100),'%') AS Percent
FROM Data
GROUP BY Gender;

