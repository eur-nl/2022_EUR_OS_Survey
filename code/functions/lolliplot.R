# custom lollipop plot
lolliplot <-
  function(data, x, y, labels, y_max_limit, title, title_size) {
    ggplot(data, aes({{ x }}, {{ y }})) +
      geom_point(size = 6, color = "#0C8066") +
      geom_segment(aes(x = {{ x }}, xend = {{ x }}, y = 0, yend = {{ y }}), color = "#012328") +
      geom_label_repel(aes({{ x }}, {{ y }}, label = {{ labels }}), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
      scale_y_continuous(
        breaks = seq(0, {{ y_max_limit }}, 10),
        limits = c(0, {{ y_max_limit }})
      ) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
      labs(
        title = {{ title }},
        x = ""
      ) +
      coord_flip() +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(size = {{ title_size }}, hjust = .5),
        plot.subtitle = element_text(size = 20, hjust = .5),
        plot.background = element_rect(fill = "white", color = "transparent"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.box.background = element_rect(color = "transparent"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "right"
      )
  }
