CREATE OR REPLACE VIEW train_observations_features AS
SELECT
    t.*,

    EXTRACT(ISODOW FROM t.date_of_departure)::integer
        AS weekday_num,

    (EXTRACT(ISODOW FROM t.date_of_departure) IN (6, 7))
        AS is_weekend,

    CASE
        WHEN EXTRACT(HOUR FROM t.planned_departure_time) BETWEEN 6 AND 9
            THEN 'morning_peak'
        WHEN EXTRACT(HOUR FROM t.planned_departure_time) BETWEEN 10 AND 15
            THEN 'day'
        WHEN EXTRACT(HOUR FROM t.planned_departure_time) BETWEEN 16 AND 19
            THEN 'evening_peak'
        ELSE 'night'
    END AS time_period

FROM train_observations AS t;
