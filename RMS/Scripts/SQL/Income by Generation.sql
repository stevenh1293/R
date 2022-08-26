SELECT Generation, CONCAT(ROUND(COUNT(Generation) / SUM(COUNT(Generation)) over() *100),'%') AS Percent
FROM Data
GROUP BY Generation;