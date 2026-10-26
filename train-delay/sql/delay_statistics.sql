SELECT
  COUNT(*) AS observations,
  COUNT(delay_arr) AS arrivals_with_delay,
  COUNT(*) FILTER (WHERE delay_arr <= 60 AND delay_arr >= -60) AS arrivals_with_delay_in_range_60,
  ROUND(AVG(delay_arr)::numeric, 2) AS mean_delay_arr_sec,
  PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY delay_arr) AS median_delay_arr_sec,
  MIN(delay_arr) AS min_delay_arr_sec,
  MAX(delay_arr) AS max_delay_arr_sec
FROM train_observations;
