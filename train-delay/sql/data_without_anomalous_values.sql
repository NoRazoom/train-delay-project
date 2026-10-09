SELECT *
FROM train_observations
WHERE delay_arr BETWEEN -3600 AND 3600
  AND delay_dep BETWEEN -3600 AND 3600;
