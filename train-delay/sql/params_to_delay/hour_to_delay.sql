SELECT
    EXTRACT(HOUR FROM planned_departure_time)::integer AS hour_dep,
    delay_arr
FROM train_observations
WHERE planned_departure_time IS NOT NULL
  AND delay_arr IS NOT NULL;