
sum_damage_by_event <- aggregate(damage ~ event_type,
                                 data = economic_data,
                                 FUN = sum)
sum_damage_by_event$damage <- round(sum_damage_by_event$damage / 1e6, 2)

with(sum_damage_by_event,
        ggplot2::ggplot(sum_damage_by_event,
                        aes(x = reorder(event_type, damage),
                            y = damage,
                            fill = event_type)) +
        ggplot2::coord_flip() +
        ggplot2::geom_bar(stat = 'identity') +
        ggplot2::ggtitle('Economic damage caused by each event type') +
        ggplot2::labs(x = 'Event type', y = 'Monetary damage (million dollars)') +
        ggplot2::theme(legend.position = 'none',
                       plot.title = element_text(hjust = 0.5))
)

sum_damage_by_event
