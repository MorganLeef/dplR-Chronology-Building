s.stats <- defol_stats(ef_defol)
e.stats <- get_defol_events(ef_defol)

e.stats <- get_defol_events(surge)

gete.stats$Severity <- cut(e.stats$ngsi_mean,
                        breaks = c(-Inf, break_vals[[1]], break_vals[[2]], Inf),
                        right = FALSE,
                        labels = c("Severe", "Moderate", "Minor"))
p <- ggplot2::ggplot(ef_defol, ggplot2::aes_string(x="year", y="series"))
p <- p + ggplot2::geom_segment(data = s.stats,
                               ggplot2::aes_string(x = "first",
                                                   xend = "last",
                                                   y = "series",
                                                   yend = "series"),
                               linetype = 'dotted')
p <- p + ggplot2::geom_segment(data = e.stats,
                               ggplot2::aes_string(x = "start_year",
                                                   xend = "end_year",
                                                   y = "series",
                                                   yend = "series",
                                                   colour = "Severity"),
                               linetype = 'solid',
                               size=1.25)
p <- p + ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(),
                 legend.position = "bottom")