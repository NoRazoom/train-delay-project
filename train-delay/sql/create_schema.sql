CREATE TABLE IF NOT EXISTS train_observations (
    id BIGSERIAL PRIMARY KEY,

    date_of_departure DATE,
    train_num BIGINT,
    relation TEXT,
    railway_operator TEXT,
    departure_line VARCHAR(10),
    arr_line VARCHAR(10),

    real_arrival_time TIMESTAMP,
    real_departure_time TIMESTAMP,
    planned_arrival_time TIMESTAMP,
    planned_departure_time TIMESTAMP,

    delay_arr INTEGER,
    delay_dep INTEGER,

    relation_direction TEXT,
    stopping_place TEXT,

    planned_date_arr DATE,
    planned_date_dep DATE,
    real_date_arr DATE,
    real_date_dep DATE,

    loaded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT uq_train_observation UNIQUE (
        date_of_departure,
        train_num,
        stopping_place,
        planned_arrival_time,
        planned_departure_time
    )
);

CREATE INDEX IF NOT EXISTS idx_train_observations_departure_date
    ON train_observations(date_of_departure);

CREATE INDEX IF NOT EXISTS idx_train_observations_train_num
    ON train_observations(train_num);

CREATE INDEX IF NOT EXISTS idx_train_observations_stopping_place
    ON train_observations(stopping_place);
