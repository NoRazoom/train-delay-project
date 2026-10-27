SELECT *
FROM train_observations
WHERE delay_arr BETWEEN -204.5 AND 335.5
  AND delay_dep BETWEEN -185   AND 327;
