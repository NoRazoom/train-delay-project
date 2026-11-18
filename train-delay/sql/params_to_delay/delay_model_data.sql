SELECT
    EXTRACT(HOUR FROM planned_departure_time)::integer AS hour_dep,
    EXTRACT(ISODOW FROM date_of_departure)::integer AS weekday_num,
    railway_operator AS operator_name,
    arr_line AS line_no_arr,
    relation_direction,
    delay_arr,
    date_of_departure
FROM train_observations
WHERE planned_departure_time IS NOT NULL
  AND date_of_departure IS NOT NULL
  AND railway_operator IS NOT NULL
  AND TRIM(railway_operator) <> ''
  AND arr_line IS NOT NULL
  AND TRIM(arr_line) <> ''
  AND relation_direction IS NOT NULL
  AND delay_arr IS NOT NULL;