SELECT
    CASE EXTRACT(DOW FROM date_of_departure)
        WHEN 0 THEN 'Воскресенье'
        WHEN 1 THEN 'Понедельник'
        WHEN 2 THEN 'Вторник'
        WHEN 3 THEN 'Среда'
        WHEN 4 THEN 'Четверг'
        WHEN 5 THEN 'Пятница'
        WHEN 6 THEN 'Суббота'
    END AS weekday,
    ROUND(AVG(delay_arr)::numeric / 60.0, 2) AS mean_delay_arr_min
FROM train_observations
WHERE date_of_departure IS NOT NULL
  AND delay_arr IS NOT NULL
GROUP BY EXTRACT(DOW FROM date_of_departure)
ORDER BY EXTRACT(DOW FROM date_of_departure);