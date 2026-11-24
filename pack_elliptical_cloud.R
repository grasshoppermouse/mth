pack_elliptical_cloud <- function(
  df,
  width_mm = 220,
  height_mm = 145,
  size_range_mm = c(2.5, 11.5),
  pad_x_mm = 0.8,
  pad_y_mm = 0.5,
  squash = 1.1
) {
  df <- assign_cvd_safe_2d(df)

  # 0. Clean NAs and map frequency (`size`) to exact ggplot2 millimeter text sizes
  df <- df[!is.na(df$word) & !is.na(df$x) & !is.na(df$y) & !is.na(df$size), ]

  # Square-root scaling prevents top frequencies (750) from dwarfing mid-tier words
  s_norm <- sqrt(df$size)
  df$font_size_mm <- scales::rescale(s_norm, to = size_range_mm)

  # Sort largest-first so anchor words claim their neighborhood core first
  # df <- df[order(df$font_size_mm), ]
  df <- df[order((df$x^2 + df$y^2) / df$size), ]
  rownames(df) <- NULL

  # 1. Exact bounding boxes in millimeters (1 mm ≈ 2.835 pt; glyph width ≈ 0.55 * height)
  # Note: ggplot2 `size` is cap-height in mm, which is ~0.72 of full em-height
  em_mm <- df$font_size_mm / 0.72
  w <- nchar(as.character(df$word)) * em_mm * 0.53 + pad_x_mm
  h <- em_mm * 0.92 + pad_y_mm

  # 2. Auto-calibrate ellipse semi-axes in mm with a tight 1.18x packing factor
  aspect <- width_mm / height_mm
  total_area <- sum(w * h) * 1.18
  ry <- sqrt(total_area / (pi * aspect))
  rx <- ry * aspect

  # 3. Normalize semantic (x, y) coordinates into a compact core (0.55 of ellipse)
  # Keeping targets slightly compact lets words pack inward without leaving central voids
  cx <- median(df$x)
  cy <- median(df$y)
  sx <- max(quantile(abs(df$x - cx), 0.90), 1e-6)
  sy <- max(quantile(abs(df$y - cy), 0.90), 1e-6)

  nx <- (df$x - cx) / sx
  ny <- (df$y - cy) / sy

  r <- sqrt(nx^2 + ny^2)
  scale_factor <- ifelse(r == 0, 0, (1 - exp(-squash * r)) / r)

  target_x <- nx * scale_factor * (rx * 0.55)
  target_y <- ny * scale_factor * (ry * 0.55)

  # 4. Fine-grained elliptical spiral placement in mm
  placed_x <- numeric(nrow(df))
  placed_y <- numeric(nrow(df))

  inside_ellipse <- function(x, y, bw, bh, curr_rx, curr_ry) {
    cx_vec <- c(x - bw / 2, x + bw / 2)
    cy_vec <- c(y - bh / 2, y + bh / 2)
    grid <- expand.grid(cx = cx_vec, cy = cy_vec)
    all((grid$cx / curr_rx)^2 + (grid$cy / curr_ry)^2 <= 1.0)
  }

  for (i in seq_len(nrow(df))) {
    tx <- target_x[i]
    ty <- target_y[i]
    theta <- 0
    found <- FALSE
    attempts <- 0

    while (!found) {
      # Fine radial step (0.18 mm per radian) lets small words tuck right beside large words
      rad <- 0.18 * theta
      cand_x <- tx + rad * cos(theta) * aspect
      cand_y <- ty + rad * sin(theta)

      if (inside_ellipse(cand_x, cand_y, w[i], h[i], rx, ry)) {
        if (
          i == 1 ||
            !any(
              abs(cand_x - placed_x[1:(i - 1)]) < (w[i] + w[1:(i - 1)]) / 2 &
                abs(cand_y - placed_y[1:(i - 1)]) < (h[i] + h[1:(i - 1)]) / 2
            )
        ) {
          placed_x[i] <- cand_x
          placed_y[i] <- cand_y
          found <- TRUE
        }
      }

      theta <- theta + 0.35
      attempts <- attempts + 1

      if (rad > max(rx, ry) * 1.4 || attempts > 2000) {
        tx <- tx * 0.85
        ty <- ty * 0.85
        rx <- rx * 1.04
        ry <- ry * 1.04
        theta <- 0
        attempts <- 0
      }
    }
  }

  df$packed_x <- placed_x
  df$packed_y <- placed_y
  attr(df, "rx") <- rx
  attr(df, "ry") <- ry
  df
}

packed_df <-
  mth200 |>
  rename(
    word = lemma,
    size = n
  ) |>
  pack_elliptical_cloud(
    width_mm = 220,
    height_mm = 145,
    size_range_mm = c(2.4, 11.0),
    pad_x_mm = 0.6,
    pad_y_mm = 0.4
  )

rx <- attr(packed_df, "rx")
ry <- attr(packed_df, "ry")

p <- ggplot(
  packed_df,
  aes(
    x = packed_x,
    y = packed_y,
    label = word,
    size = font_size_mm,
    # colour = cluster
  )
) +
  geom_text(family = "sans") +
  scale_size_identity() +
  # guides(colour = guide_none()) +
  coord_fixed(ratio = 1, xlim = c(-rx, rx), ylim = c(-ry, ry), expand = FALSE) +
  theme_void()

# Export using the exact millimeter dimensions of the ellipse (plus small margin)
ggsave(
  "wordcloud.ellipse.svg",
  plot = p,
  width = (rx * 1.4) + 6,
  height = (ry * 1.4) + 6,
  units = "mm"
)
