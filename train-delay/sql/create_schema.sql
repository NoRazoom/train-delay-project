CREATE TABLE IF NOT EXISTS train_observations (
    id BIGSERIAL PRIMARY KEY,
    date_of_departure DATE,
    train_num INTEGER,
    relation TEXT,
    railway_operator TEXT,
    departure_line VARCHAR(10),
    arr_line VARCHAR(10),

    real_arrival_time TIMESTAMP,
    real_adeparture_time TIMESTAMP,
    planned_arrival_time TIMESTAMP,
    planned_adeparture_time TIMESTAMP,

    delay_arr INTEGER,
    delay_dep INTEGER,

    relation_direction TEXT,
    stopping_place TEXT,

    planned_date_arr DATE,
    planned_date_dep DATE,
    real_date_arr DATE,
    real_date_dep DATE,

    loaded_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
