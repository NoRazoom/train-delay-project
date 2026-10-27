SELECT
    delay_arr / 60.0 AS delay_arr_min,
    delay_dep / 60.0 AS delay_dep_min
FROM train_observations
WHERE delay_arr IS NOT NULL
    AND delay_dep IS NOT NULL
    WHERE delay_arr BETWEEN -204.5 AND 335.5
  AND delay_dep BETWEEN -185   AND 327;