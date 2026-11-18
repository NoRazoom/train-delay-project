SELECT
    arr_line AS line_no_arr,
    delay_arr
FROM train_observations
WHERE arr_line IS NOT NULL
  AND TRIM(arr_line) <> ''
  AND delay_arr IS NOT NULL;