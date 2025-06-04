shotchart_sector <- function(df, player = NULL, team = NULL) {
  filtered <- df
  if (!is.null(player)) filtered <- filtered %>% filter(Player == player)
  if (!is.null(team)) filtered <- filtered %>% filter(Team == team)
  if (nrow(filtered) == 0) stop("Brak danych po filtrowaniu.")

  zones <- data.frame(
    zone = c("RIM", "PAINT", "MID", "C3", "L3", "H3"),
    made = c(sum(filtered$RIM_M), sum(filtered$PAINT_M), sum(filtered$MID_M),
             sum(filtered$`3C_M`), sum(filtered$`3L_M`), sum(filtered$`3H_M`)),
    att = c(sum(filtered$RIM_A), sum(filtered$PAINT_A), sum(filtered$MID_A),
            sum(filtered$`3C_A`), sum(filtered$`3L_A`), sum(filtered$`3H_A`))
  ) %>%
    mutate(
      FG_perc = ifelse(att > 0, round(100 * made / att, 1), NA),
      label = ifelse(att > 0, paste0(FG_perc, "%\n(", made, "/", att, ")"), "n/a")
    )

  # Dane do geometrii
  rim <- zones %>% filter(zone == "RIM")
  paint <- zones %>% filter(zone == "PAINT")
  mid <- zones %>% filter(zone == "MID")
  c3 <- zones %>% filter(zone == "C3")
  l3 <- zones %>% filter(zone == "L3")
  h3 <- zones %>% filter(zone == "H3")

  # Pozycje etykiet
  labels <- data.frame(
    zone = c("RIM", "PAINT", "MID", "C3L", "C3R", "L3", "H3"),
    x = c(0, 0, 0, -21, 21, 0, 0),
    y = c(5, 12, 20, 3, 3, 30, 43)
  )
  label_data <- bind_rows(
    rim %>% mutate(x = 0, y = 5),
    paint %>% mutate(x = 0, y = 14),
    mid %>% mutate(x = 0, y = 23),
    c3 %>% mutate(x = -25, y = 3),
    c3 %>% mutate(x = 25, y = 3),
    l3 %>% mutate(x = 0, y = 31),
    h3 %>% mutate(x = 0, y = 41.5)
  )

  draw_court <- function() {
    list(
      # Obrys
      geom_rect(aes(xmin = -28, xmax = 28, ymin = 0, ymax = 47), fill = NA, color = "black", size = 1.2),
      geom_segment(aes(x = -28, y = 0, xend = 28, yend = 0), color = "black", size = 1.2),

      # Trumna
      geom_rect(aes(xmin = -8, xmax = 8, ymin = 0, ymax = 19), fill = NA, color = "black", size = 1.2),

      # Linia rzutów wolnych i półkole
      geom_segment(aes(x = -8, y = 19, xend = 8, yend = 19), color = "black", size = 1.2),
      geom_path(data = data.frame(
        x = 6 * cos(seq(0, pi, length.out = 100)),
        y = 6 * sin(seq(0, pi, length.out = 100)) + 19
      ), aes(x = x, y = y), color = "black", size = 1.2),

      # Półkole pod koszem
      geom_path(data = data.frame(
        x = 1.25 * cos(seq(0, pi, length.out = 100)),
        y = 1.25 * sin(seq(0, pi, length.out = 100)) + 5
      ), aes(x = x, y = y), color = "black", size = 1.2),

      # Łuk za 3
      geom_path(data = data.frame(
        x = 22.15 * cos(seq(0.1, pi - 0.1, length.out = 100)),
        y = 22.15 * sin(seq(0.1, pi - 0.1, length.out = 100)) + 5
      ), aes(x = x, y = y), color = "black", size = 1.2),

      # Corner 3
      geom_segment(aes(x = -22, y = 0, xend = -22, yend = 7.5), color = "black", size = 1.2),
      geom_segment(aes(x = 22, y = 0, xend = 22, yend = 7.5), color = "black", size = 1.2)
    )
  }

  ggplot() +
    draw_court() +

    # STREFY
geom_rect(data = rim,
  aes(xmin = -6, xmax = 6, ymin = 2, ymax = 8, fill = FG_perc),
  color = NA, alpha = 0.85) +

geom_rect(data = paint,
  aes(xmin = -6, xmax = 6, ymin = 9, ymax = 18, fill = FG_perc),
  color = NA, alpha = 0.85) +

geom_arc_bar(data = mid,
  aes(x0 = 0, y0 = 5, r0 = 15, r = 21.5,
      start = pi * (-0.5), end = pi * 0.5, fill = FG_perc),
  color = NA, alpha = 0.85) +


geom_rect(data = c3,
  aes(xmin = -28, xmax = -22, ymin = 0, ymax = 7.5, fill = FG_perc),
  color = NA, alpha = 0.85) +

geom_rect(data = c3,
  aes(xmin = 22, xmax = 28, ymin = 0, ymax = 7.5, fill = FG_perc),
  color = NA, alpha = 0.85) +

geom_arc_bar(data = l3,
  aes(x0 = 0, y0 = 5, r0 = 22.75, r = 30,
      start = pi * (-0.35), end = pi * 0.35, fill = FG_perc),
  color = NA, alpha = 0.85) +

geom_rect(data = h3,
  aes(xmin = -25, xmax = 25, ymin = 37, ymax = 46, fill = FG_perc),
  color = NA, alpha = 0.85) +

# Etykiety
geom_text(data = label_data,
  aes(x = x, y = y, label = label),
  color = "white", size = 4) +

    scale_fill_gradient(low = "red", high = "green", na.value = "grey80", name = "FG%") +
    coord_fixed(xlim = c(-28, 28), ylim = c(0, 47)) +
    theme_void() +

    theme(
       plot.title = element_text(hjust = 0, margin = margin(l = 23), size = 14),
       plot.subtitle = element_text(hjust = 0, margin = margin(l = 30), size = 11)
    ) + 
        
    labs(
      title = if (!is.null(player)) paste("Shot Chart –", player) else if
       (!is.null(team)) paste("Shot Chart –", team) else "Shot Chart – Wszyscy gracze"
    )
}
