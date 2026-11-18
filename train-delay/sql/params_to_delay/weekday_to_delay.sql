SELECT
    EXTRACT(ISODOW FROM date_of_departure)::integer AS weekday_num,
    delay_arr
FROM train_observations
WHERE date_of_departure IS NOT NULL
  AND delay_arr IS NOT NULL;