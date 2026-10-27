library(dplyr)
library(ggplot2)
library(ragg)


get_delay_correlation <- function(con) {

    sql <- paste(
        readLines("sql/delay_in_min_and_no_anon.sql", warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
    )

    delays <- dbGetQuery(con, sql)

    message(sprintf("Загружено наблюдений: %d", nrow(delays)))

    cor_pearson <- cor(
        delays$delay_dep_min,
        delays$delay_arr_min,
        use = "complete.obs",
        method = "pearson"
    )

    cor_spearman <- cor(
        delays$delay_dep_min,
        delays$delay_arr_min,
        use = "complete.obs",
        method = "spearman"
    )

    message(sprintf(
        "Корреляция Пирсона: %.3f",
        cor_pearson
    ))

    message(sprintf(
        "Корреляция Спирмена: %.3f",
        cor_spearman
    ))

    p_correlation <- ggplot(
        delays,
        aes(x = delay_dep_min, y = delay_arr_min)
    ) +
        geom_point(
            alpha = 0.05,
            color = "#4C72B0"
        ) +
        geom_abline(
            slope = 1,
            intercept = 0,
            linetype = "dashed",
            color = "red"
        ) +
        coord_cartesian(
            xlim = c(-10, 30),
            ylim = c(-10, 30)
        ) +
        labs(
            title = "Связь между задержкой отправления и задержкой прибытия",
            x = "Задержка отправления, минуты",
            y = "Задержка прибытия, минуты"
        ) +
        theme_minimal()

    dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

    ggsave(
        filename = "output/plots/delay_correlation.png",
        plot = p_correlation,
        device = ragg::agg_png,
        width = 10,
        height = 7,
        dpi = 300
    )

    invisible(list(
        data = delays,
        pearson = cor_pearson,
        spearman = cor_spearman,
        plot = p_correlation
    ))
}