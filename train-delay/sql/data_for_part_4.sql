SELECT
    date_of_departure,
    EXTRACT(HOUR FROM planned_departure_time)::integer AS hour_dep,
    EXTRACT(ISODOW FROM date_of_departure)::integer AS weekday_num,
    railway_operator AS operator_name,
    arr_line AS line_no_arr,
    relation_direction,
    delay_arr
FROM train_observations
WHERE delay_arr IS NOT NULL;