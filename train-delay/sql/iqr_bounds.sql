WITH stats AS (
    SELECT
        PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY delay_arr) AS q1_arr,
        PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY delay_arr) AS q3_arr,
        PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY delay_dep) AS q1_dep,
        PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY delay_dep) AS q3_dep
    FROM train_observations
    WHERE delay_arr IS NOT NULL
      AND delay_dep IS NOT NULL
)
SELECT
    q1_arr,
    q3_arr,
    q3_arr - q1_arr                                   AS iqr_arr,
    q1_arr - 1.5 * (q3_arr - q1_arr)                  AS lower_arr,
    q3_arr + 1.5 * (q3_arr - q1_arr)                  AS upper_arr,

    q1_dep,
    q3_dep,
    q3_dep - q1_dep                                   AS iqr_dep,
    q1_dep - 1.5 * (q3_dep - q1_dep)                  AS lower_dep,
    q3_dep + 1.5 * (q3_dep - q1_dep)                  AS upper_dep
FROM stats;