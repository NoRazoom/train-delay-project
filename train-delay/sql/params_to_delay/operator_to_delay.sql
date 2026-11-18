SELECT
    railway_operator AS operator_name,
    delay_arr
FROM train_observations
WHERE railway_operator IS NOT NULL
  AND TRIM(railway_operator) <> ''
  AND delay_arr IS NOT NULL;