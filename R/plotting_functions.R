#' VR Threat ggplot theme
#'#'
#' @param base_size Base font size
#' @param base_family Base font familt
#'
#' @return A ggplot theme object.
#' @export
#'
#' @examples
theme_vrthreat <- function(base_size=14, base_family="sans") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family)
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = rel(1.2),
      hjust = 0.5,
      margin = ggplot2::margin(0, 0, 20, 0)
    ),
    text = ggplot2::element_text(),
    panel.background = ggplot2::element_rect(colour = NA),
    plot.background = ggplot2::element_rect(colour = NA),
    axis.title = ggplot2::element_text(face = "bold", size = rel(1)),
    axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
    axis.title.x = ggplot2::element_text(vjust = -0.2),
    axis.text = ggplot2::element_text(),
    axis.line.x = ggplot2::element_line(colour = "black"),
    axis.line.y = ggplot2::element_line(colour = "black"),
    axis.ticks = ggplot2::element_line(),
    panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
    panel.grid.minor = ggplot2::element_blank(),
    legend.key = ggplot2::element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vetical",
    legend.key.size = ggplot2::unit(0.5, "cm"),
    plot.margin = unit(c(10, 5, 5, 5), "mm"),
    panel.spacing = unit(5, "mm"),
    strip.background = ggplot2::element_rect(colour = "#f0f0f0",
                                             fill = "#f0f0f0"),
    strip.text = ggplot2::element_text(
      face = "bold",
      size = base_size * 0.8,
      margin = ggplot2::margin(3, 3, 3, 3)
    )
  )
}


#' VR Threat Planning Time ggplot scale
#'
#' A fill/color scale used across planning time values (`1.5`, `5`).
#'
#' @param ... Arguments passed to `scale_fill_manual` and `scale_color_manual`.
#'
#' @return A set of scales to be added to a ggplot.
#' @export
#'
#' @examples
scale_vrthreat_planningtime <- function(...) {
  values <- c(
    "1.5" = "#7384E6",
    "5" = "#D68989"
  )

  list(
    ggplot2::scale_fill_manual(values = values, name = "Planning time", ...),
    ggplot2::scale_color_manual(values = values, name = "Planning time", ...)
  )
}

#' VR Threat Scenario Type ggplot scale
#'
#' A fill/color scale used across scenario types (`"Chase"`, `"Divert"`).
#'
#' @param ... Arguments passed to `scale_fill_manual` and `scale_color_manual`.
#'
#' @return A set of scales to be added to a ggplot.
#' @export
#'
#' @examples
scale_vrthreat_scenariotype <- function(...) {
  values <- c(
    "Chase" = "#FF8C42",
    "Divert" = "#935FA7"
  )

  list(
    ggplot2::scale_fill_manual(values = values, name = "Scenario type", ...),
    ggplot2::scale_color_manual(values = values, name = "Scenario type", ...)
  )
}

#' VR Threat End State ggplot scale
#'
#' A fill/color scale used across end state values (`"Safe"`,
#' `"ConfrontedThreat"`, `"Survived"`).
#'
#' @param ... Arguments passed to `scale_fill_manual` and `scale_color_manual`.
#'
#' @return A set of scales to be added to a ggplot.
#' @export
#'
#' @examples
scale_vrthreat_endstate <- function(...) {
  values <- c(
    "Safe" = "#50C863",
    "ConfrontedThreat" = "#C86350",
    "Survived" = "#6350C8"
  )

  list(
    ggplot2::scale_fill_manual(values = values, name = "End state", ...),
    ggplot2::scale_color_manual(values = values, name = "End state", ...)
  )
}


#' Pastel colored ggplot scale
#'
#' A fill/color hue scale with pastel colours.
#'
#' @param ... Arguments passed to `scale_fill_hue` and `scale_color_hue`.
#'
#' @return A set of scales to be added to a ggplot.
#' @export
#'
#' @examples
scale_pastel <- function(...) {
  list(
    ggplot2::scale_color_hue(h = c(0, 360) - 7, c = 70, l = 70, ...),
    ggplot2::scale_fill_hue(h = c(0, 360) - 7, c = 70, l = 70, ...)
  )
}


#' Plot 3D movements for one trial
#'
#' Any movement data frame (tracker, threat) is plotted in an interactive 3D plot.
#'
#' @param df a movement data frame
#' @param xlim The left-right limit of the plot (default: ```c(-2, 2)```)
#' @param ylim The top-bottom limit of the plot (default: ```c(-0.2, 2.5)```)
#' @param zlim The forward-backward limit of the plot (default: ```c(-3, 3)```)
#'
#' @return
#' @export
#'
#' @examples
plot_3D_movement <- function(df,
                             xlim = c(-2, 2),
                             ylim = c(-.2, 2.5),
                             zlim = c(-3, 3)
                             ) {
  ## Use blue color spectrum to track people's movement, you can tell that...
  ## the earlier the movement the lighter the blue color, and the later the movement the darker the blue color
  ## Note that here I add 400 more points and then discarded them, ...
  ## because the blue color for the earlier movements is really light and hard to see.
  num <- nrow(df) + 400
  k <- colorRampPalette(RColorBrewer::brewer.pal(7, "Blues"))(num)
  k1 <- k[401:length(k)]

  ## Open a 3D environment and plot the track
  rgl::open3d()

  ## Plot points that follow the timeline on this 3D environment
  rgl::plot3d(
    x = df$pos_x,
    y = df$pos_z,
    z = df$pos_y,
    xlab = "left-right (m)",
    ylab = "forward-backward (m)",
    zlab = "Height (m)",
    type = "p",
    col = k1,
    aspect = "iso",
    ticktype = "simple"
  )

  ## ensure correct axis limits by drawing transparent axis lines
  rgl::rgl.lines(xlim, 0, 0, color = "grey")
  rgl::rgl.lines(0, 0,  ylim, color = "grey")
  rgl::rgl.lines(0, zlim, 0, color = "grey")

  ## add the starting and end points
  rgl::text3d(
    x = df$pos_x[1],
    y = df$pos_z[1],
    z = df$pos_y[1],
    text = "Start",
    pos = 1,
    color = "blue"
  )

  rgl::text3d(
    x = df[nrow(df),]$pos_x,
    y = df[nrow(df),]$pos_z,
    z = df[nrow(df),]$pos_y,
    text = "End",
    pos = 1,
    color = "blue"
  )
}




