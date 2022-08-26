SELECT g.Area_Type, CONCAT(ROUND(COUNT(g.Area_Type) / SUM(COUNT(g.Area_Type)) over() *100),'%') AS Percent
FROM data AS d
LEFT JOIN geographic_area as g
	ON d.Geo_id = g.id
GROUP BY g.Area_Type;