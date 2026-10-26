SELECT delay_arr / 60.0 AS delay_arr_min, delay_dep / 60.0 AS delay_dep_min
FROM train_observations
WHERE delay_arr IS NOT NULL AND delay_dep IS NOT NULL