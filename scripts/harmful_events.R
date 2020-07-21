
heath_data_set <- aggregate(formula = count ~ event_type + heath_type,
                            data = heath_data,
                            FUN = sum, na.rm = T)
with(heath_data_set,
        ggplot2::ggplot(heath_data_set,
                        aes(x = reorder(event_type, count),
                            y = count,
                            fill = event_type)
        ) +
        ggplot2::coord_flip() +
        ggplot2::geom_bar(stat = 'identity') +
        ggplot2::ggtitle('Heath problems caused by each event type') +
        ggplot2::labs(x = 'Event type', y = 'Deaths and injuries count') +
        ggplot2::theme(legend.position = 'none',
                       plot.title = element_text(hjust = 0.5)) + 
        ggplot2::facet_grid(. ~ heath_type, scales = 'free_x')
)

heath_data_set
