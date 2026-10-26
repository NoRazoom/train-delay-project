SELECT *
FROM train_observations
WHERE delay_arr BETWEEN -7200 AND 7200
  AND delay_dep BETWEEN -7200 AND 7200;
