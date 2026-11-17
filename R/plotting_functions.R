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
#' @param xlim The left-right limit of the plot (default: `c(-2, 2)`)
#' @param ylim The top-bottom limit of the plot (default: `c(-0.2, 2.5)`)
#' @param zlim The forward-backward limit of the plot (default: `c(-3, 3)`)
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
  rgl::segments3d(xlim, 0, 0, color = "grey")
  rgl::segments3d(0, 0,  ylim, color = "grey")
  rgl::segments3d(0, zlim, 0, color = "grey")

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

#' Plot a graph using community bubbles
#'
#' Creates a community-aware network plot in which each community is shown as
#' a circular bubble, with its vertices laid out inside the bubble. Community
#' centres are positioned using a graph layout and subsequently separated to
#' prevent bubbles from overlapping. Larger communities therefore occupy more
#' space in the overall layout.
#'
#' Several layouts are available for vertices within communities. `"circle"`
#' places vertices evenly around the bubble, `"fr"` uses a force-directed
#' layout, and `"external"` places vertices according to their connectivity
#' to other communities. `"external_fr"` combines the latter with the angular
#' structure of a force-directed layout. In the external layouts, vertices
#' with many or strong connections outside their community are placed closer
#' to the community centre, highlighting potential inter-community bridges.
#'
#' External connectivity can be based on edge counts or edge weights, and can
#' account for incoming, outgoing, or both types of connections in directed
#' graphs.
#' 
#' Certain data like node sizes, node names etc are passed on as igraph 
#' vertex attributes in `g`. The names of these attributes are then passed
#' via parameters like `node_size` or `edge_width` as strings. 
#' Some attribute names are used automatically if they exist (like `name`)
#'
#' The `name` vertex attribute is used for producing labels for each vertex.
#' Plots with and witout labels are both automatically produced.
#' 
#' @param g An igraph graph object.
#' @param comm Community membership, either an igraph `communities` object or
#'   a vector with one community ID per vertex. Defaults to
#'   [igraph::cluster_infomap()].
#' @param layout Layout used for community centres. Supported values include
#'   `"fr"`, `"kk"`, `"circle"`, `"drl"`, `"lgl"`, `"graphopt"`, and
#'   `"nicely"`, or a user-supplied layout function.
#' @param inside_layout Layout used within each community: `"circle"`,
#'   `"external"`, `"fr"`, or `"external_fr"`.
#' @param external_measure How external connectivity is measured:
#'   `"count"` counts external edges, while `"weighted"` sums their weights.
#' @param direction For directed graphs, whether external connectivity is
#'   based on `"both"`, `"out"`, or `"in"` edges.
#' @param node_size Optional vertex attribute name used to scale node size.
#' @param node_size_ranges Range within which to scale the nodes using `node_size`. (Default `c(4, 16)`)
#' @param node_size_color Display node size with a monochrome colorbar (Default `FALSE`)
#' @param node_title Optional title for node sizing on the legend.
#' @param edge_width Optional edge attribute name used to scale edge width.
#' @param edge_opacity_multiplier Tweak opacity of edges by a multiplier. (any float, Default 1 for no tweak, 0 for transparent, 2 for doubling opacity)
#' @param edge_title Optional title for edge scale on the legend.
#' @param label_card Enable a card behind the label for visibility (Default `TRUE`)
#' @param nudge_label Nudge in position of label from node (Default `c(-0.1, -0.3)`)
#' @param text_size Set text size on labels (Default 4.5)
#' @param text_face Set text type on labels as passed to `ggraph::geom_node_label` (Default `"plain"`)
#' @param bubble_scale Numeric multiplier controlling bubble size.
#' @param node_spacing Numeric multiplier controlling spacing of vertices
#'   within bubbles.
#' @param bubble_gap Minimum gap between community bubbles.
#' @param centre_iterations Number of iterations used to resolve overlapping
#'   community bubbles.
#' @param node_clouds Enable community colored clouds around nodes (Default `FALSE`)
#' @param cloudiness Intensity of community representing clouds drawn around a node.
#' @param show_bubbles Enable drawing of bubble backdrop for communities (Default `TRUE`)
#' @param fill_bubbles Enable filling bubble with shade corresponding to communtiy (Default `TRUE`)
#' @param plot_margin Margins passed to `ggplot2::theme`. (Default `margin(0, 15, 0, 0)`)
#' @param legend_position Legend position parameter passed to `ggplot2::theme()`. (Default `NULL`, compute legend position)
#' @param community_legend Enable color legend for communities (Default: TRUE)
#' @param size_legend Enable size legend for node sizes (Default: TRUE)
#' @param edge_legend Enable edge legend fo edge widths (Default: TRUE)
#' 
#' @details
#' The graph is represented at two levels. First, communities are contracted
#' into a community-level graph whose vertices are laid out using `layout`.
#' Community centres are then iteratively separated according to their bubble
#' radii, preserving the broad structure of the selected layout while
#' preventing overlap.
#'
#' For `"external"` and `"external_fr"`, each vertex receives an
#' external-connectivity score based on edges connecting it to other
#' communities. The score is normalized within each community and controls
#' the vertex's radial position: highly externally connected vertices are
#' drawn near the centre, while vertices with predominantly internal
#' connections are placed toward the perimeter.
#'
#' @return A `ggraph`/`ggplot` object.
#'
#' @examples
#'
#' @export

plot_community_bubbles <- function(
    g,
    comm = igraph::cluster_infomap(g),
    layout = "fr",
    inside_layout = "external_fr", # "circle", "external", "fr", "external_fr"
    external_measure = "count",
    direction = "both",
    node_size = NULL,
    node_title = NULL,
    node_size_ranges = c(4, 16),
    node_size_color = FALSE,
    edge_width = NULL,
    edge_opacity_multiplier = 1,
    edge_title = NULL,
    label_card = TRUE,
    nudge_label = c(-0.1, -0.3),
    text_size = 4.5,
    text_face = "plain",
    bubble_scale = 1,
    node_spacing = 1,
    bubble_gap = 1,
    centre_iterations = 100,
    node_clouds = FALSE,
    cloudiness = 2,
    show_bubbles = TRUE,
    fill_bubble = TRUE,
    plot_margin = margin(0, 15, 0, 0),
    legend_position = NULL,
    community_legend = TRUE,
    size_legend = TRUE,
    edge_legend = TRUE
) {
  if(!igraph::is_igraph(g)){
    warning("[plot_community_bubbles] input graph not an igraph object", call. = FALSE)
    return(NULL)
  }

  if(!validate_option(inside_layout, "circle", "external", "fr", "external_fr", .fun_name = "plot_community_bubbles") ||
     !validate_option(external_measure, "count", "weighted", .fun_name = "plot_community_bubbles") ||
     !validate_option(direction, "both", "out", "in", .fun_name = "plot_community_bubbles"))
  return(NULL)

  # ============================================================
  # 1. Community membership
  # ============================================================
  if (inherits(comm, "communities")) {
    membership <- igraph::membership(comm)
  } else if (length(comm) == igraph::vcount(g)) {
    membership <- as.integer(comm)
  } else {
    warning(
      "[plot_community_bubbles] `comm` must be an igraph communities object or a vector ",
      "with one community ID per vertex.", .call = FALSE
    )
    return(NULL)
  }

  membership <- as.integer(factor(membership))
  n_comm <- max(membership)


  # ============================================================
  # 2. Community-level graph coordinates and sizing
  # ============================================================

  g_comm <- igraph::contract(
    g,
    mapping = membership,
    vertex.attr.comb = "ignore"
  )

  g_comm <- igraph::simplify(
    g_comm,
    remove.multiple = TRUE,
    remove.loops = TRUE
  )

  # Compute layout of centers as per user specification
  if (is.function(layout)) {
    comm_xy <- layout(g_comm)
  } else {
    layout_fun <- switch(layout,
      fr       = igraph::layout_with_fr,
      kk       = igraph::layout_with_kk,
      circle   = igraph::layout_in_circle,
      drl      = igraph::layout_with_drl,
      lgl      = igraph::layout_with_lgl,
      graphopt = igraph::layout_with_graphopt,
      nicely   = igraph::layout_nicely,
      stop("Unknown layout: ", layout)
    )
    comm_xy <- layout_fun(g_comm)
  }

  colnames(comm_xy) <- c("cx", "cy")

  # Compute Community sizes and radii
  community_sizes <- tabulate(membership)
  bubble_radius <- bubble_scale * sqrt(community_sizes)

  # ============================================================
  # 3. Separate community centres
  # ============================================================
  if (n_comm > 1) {
    comm_xy <- igraph::norm_coords(
      comm_xy,
      xmin = -1,
      xmax = 1,
      ymin = -1,
      ymax = 1
    )

    # Iteratively nudge centers away, until no overlap
    for (iter in seq_len(centre_iterations)) {
      displacement <- matrix(
        0,
        nrow = n_comm,
        ncol = 2
      )

      any_overlap <- FALSE

      for (i in seq_len(n_comm - 1)) {
        for (j in (i + 1):n_comm) {
          dx <- comm_xy[j, 1] - comm_xy[i, 1]
          dy <- comm_xy[j, 2] - comm_xy[i, 2]

          distance <- sqrt(dx^2 + dy^2)
          if (distance < 1e-10) {
            angle <- 2 * pi * i / n_comm
            dx <- cos(angle)
            dy <- sin(angle)
            distance <- 1
          }

          min_distance <- bubble_radius[i] + bubble_radius[j] + bubble_gap

          if (distance < min_distance) {
            any_overlap <- TRUE
            ux <- dx / distance
            uy <- dy / distance
            
            overlap <- min_distance - distance
            displacement[i, ] <- displacement[i, ] - c(ux, uy)*overlap/2
            displacement[j, ] <- displacement[j, ] + c(ux, uy)*overlap/2
          }
        }
      }

      comm_xy <- comm_xy + displacement * 0.5
      if (!any_overlap)
        break
    }
  }

  # ============================================================
  # 4. Calculate external connectivity
  # ============================================================
  n <- igraph::vcount(g)
  external_score <- numeric(n)

  # Edge List as Matrix
  ends <- igraph::ends(g, E(g), names = FALSE)

  # Edge weights
  if ("weight" %in% igraph::edge_attr_names(g)) {
    edge_weights <- igraph::E(g)$weight
    if (external_measure == "weighted") {
      edge_weights <- abs(edge_weights)
    }
  } else {
    if (external_measure == "weighted") {
      warning("[plot_community_bubbles] No `weight` edge attribute found; using edge counts instead.", call. = FALSE)
    }
    edge_weights <- rep(1, igraph::ecount(g))
  }

  # Add external connectivity to each vertex
  for (e in seq_len(nrow(ends))) {
    from <- ends[e, 1]
    to   <- ends[e, 2]

    if (membership[from] == membership[to])
      next

    w <- edge_weights[e]

    if (!igraph::is_directed(g) || direction == "both") {
      external_score[from] <- external_score[from] + w
      external_score[to] <- external_score[to] + w
    } else if (direction == "out") {
      external_score[from] <- external_score[from] + w
    } else if (direction == "in") {
      external_score[to] <-external_score[to] + w
    }
  }

  # ============================================================
  # 5. Normalise external connectivity within communities
  # ============================================================
  external_norm <- numeric(n)
  for (k in seq_len(n_comm)) {
    members <- which(membership == k)
    x <- external_score[members]

    if (max(x) > min(x)) {
      external_norm[members] <- (x - min(x)) / (max(x) - min(x))
    } else {
      external_norm[members] <- 0
    }
  }

  # ============================================================
  # 6. Generate coordinates inside each bubble
  # ============================================================
  coords <- matrix(NA_real_, nrow = n, ncol = 2)
  colnames(coords) <- c("x", "y")

  for (k in seq_len(n_comm)) {
    members <- which(membership == k)
    nk <- length(members)

    centre <- comm_xy[k, ]

    # ----------------------------------------------------------
    # Singleton
    # ----------------------------------------------------------
    if (nk == 1) {
      coords[members, ] <- centre
      next
    }

    # ----------------------------------------------------------
    # Circle layout
    # ----------------------------------------------------------
    if (inside_layout == "circle") {
      theta <- seq(0, 2 * pi, length.out = nk + 1)[-(nk + 1)]
      r <- 0.55*bubble_radius[k]*node_spacing

      coords[members, 1] <- centre[1] + r*cos(theta)
      coords[members, 2] <- centre[2] + r*sin(theta)
      next
    }

    # ----------------------------------------------------------
    # External-connectivity radial layout
    # ----------------------------------------------------------
    if (inside_layout == "external") {
      # Highly externally connected nodes get small radius.
      # external_norm = 1 -> centre
      # external_norm = 0 -> perimeter
      theta <- seq(0, 2 * pi, length.out = nk + 1)[-(nk + 1)]
      radial_fraction <- 0.12 + 0.72 * (1 - external_norm[members])

      r <- bubble_radius[k]*radial_fraction*node_spacing
      coords[members, 1] <- centre[1] + r*cos(theta)
      coords[members, 2] <- centre[2] + r*sin(theta)
      next
    }

    # ----------------------------------------------------------
    # Force-directed layout inside community
    # ----------------------------------------------------------
    g_sub <- igraph::induced_subgraph(g, vids = members)
    local_xy <- igraph::layout_with_fr(g_sub)

    # Normalise local coordinates
    if (nk > 1) {
      local_xy <- igraph::norm_coords(
        local_xy,
        xmin = -1,
        xmax = 1,
        ymin = -1,
        ymax = 1
      )
    }

    # ----------------------------------------------------------
    # Standard FR
    # ----------------------------------------------------------
    if (inside_layout == "fr") {
      local_xy <- local_xy / max(sqrt(rowSums(local_xy^2))) * (0.75*bubble_radius[k]*node_spacing)
      coords[members, ] <- local_xy + centre
      next
    }

    # ----------------------------------------------------------
    # External-FR
    # ----------------------------------------------------------
    # Preserve the angular structure generated by FR, but
    # replace the radial coordinate according to external
    # connectivity.

    theta <- atan2(local_xy[, 2], local_xy[, 1])

    radial_fraction <- 0.10 + 0.78 * (1 - external_norm[members])

    r <- bubble_radius[k]*radial_fraction*node_spacing

    coords[members, 1] <- centre[1] + r*cos(theta)
    coords[members, 2] <- centre[2] + r*sin(theta)
  }

  # ============================================================
  # 7. Node data
  # ============================================================
  # Instantiate Node
  node_df <- data.frame(
    x = coords[, 1],
    y = coords[, 2],
    community = factor(membership),
    external_connectivity = external_score,
    external_connectivity_normalized = external_norm
  )

  # Set Node Name
  if ("name" %in% igraph::vertex_attr_names(g)) {
    node_df$name <- igraph::V(g)$name
  } else {
    node_df$name <-
      as.character(seq_len(n))
  }

  # Set Node Size
  if (!is.null(node_size)) {
    if (!node_size %in% igraph::vertex_attr_names(g)) {
      stop(stringr::str_glue("Node attribute {node_size} was not found in the graph."))
    }
    node_df$node_size <- igraph::vertex_attr(g, node_size)
  } else {
    node_df$node_size <- 5
  }

  # ============================================================
  # 8. Bubble data
  # ============================================================
  # Store Community centers etc
  bubble_df <- data.frame(
    x = comm_xy[, 1],
    y = comm_xy[, 2],
    r = bubble_radius,
    community = factor(seq_len(n_comm))
  )

  # ============================================================
  # 9. Build graph
  # ============================================================
  # Convert snake_case column names to readable labels
  format_name <- function(x) {tools::toTitleCase(stringr::str_replace_all(x, "_", " "))}

  p <- ggraph::ggraph(g, layout = "manual", x = coords[, 1], y = coords[, 2])
  # ------------------------------------------------------------
  # Bubble Outlines
  # ------------------------------------------------------------
  if (show_bubbles) {
    p <- p +
      ggforce::geom_circle(
      data = bubble_df,
      ggplot2::aes(
        x0 = x,
        y0 = y,
        r = r,
        fill = if(fill_bubble) community, 
        group = community,
      ),
      inherit.aes = FALSE,
      alpha = 0.11,
      linewidth = 0.1
    ) +
    scale_fill_discrete() + 
    ggplot2::labs(fill = "Community",)
  }

  # ------------------------------------------------------------
  # Edges
  # ------------------------------------------------------------
  if (!is.null(edge_width)) {
    if (!edge_width %in% igraph::edge_attr_names(g))
      stop(stringr::str_glue("Edge attribute {edge_width} was not found in the graph."))
    p <- p +
      ggraph::geom_edge_link(
        ggplot2::aes(edge_width = .data[[edge_width]]),
        alpha = 0.28*edge_opacity_multiplier
      ) +
      ggraph::scale_edge_width_continuous(
        name = if(is.null(edge_title)) format_name(edge_width) else edge_title,
        range = c(0.3,1.3)
      )
  } else {
    p <- p +
      ggraph::geom_edge_link(
        edge_width = 0.5,
        alpha = 0.5*edge_opacity_multiplier
      )
  }

  if(!edge_legend){
    p <- p + ggplot2::guides(edge_width = "none")
  }

  # ------------------------------------------------------------
  # Community Clouds
  # ------------------------------------------------------------
  if(node_clouds){
    for(i in 1:cloudiness){
      p <- p +
        ggraph::geom_node_point(
          data = node_df,
          ggplot2::aes(
            x = x,
            y = y,
            colour = community
          ),
          size = 7*(i + i^2/100),
          alpha = 0.02*(i + i^2/100),
          stroke = 0 
        )
    }

    p <- p + scale_colour_discrete(guide = "none")
  }


  if(!community_legend){
    p <- p + ggplot2::guides(fill = "none")
  }

  # ------------------------------------------------------------
  # Nodes
  # ------------------------------------------------------------
  if(node_size_color){
    p <- p +ggnewscale::new_scale_fill()
  }
  
  p <- p + ggraph::geom_node_point(
      data = node_df,
      ggplot2::aes(
        x = x,
        y = y,
        colour = community,
        size = node_size,
        fill = if(node_size_color) node_size else community
      ),
      shape = 21,
      colour = "black"
  ) + 
    ggplot2::scale_size_continuous(
      range = node_size_ranges
  )

  if(node_size_color){
    p <- p + ggplot2::guides(
      size = "none",
      fill = ggplot2::guide_colorbar(title = if(is.null(node_title)) format_name(node_size) else node_title)
    )
    p <- p +
      ggplot2::scale_fill_continuous(
        low = "gray80",
        high = "gray30",
        name = format_name(node_size) 
      )
  } else {
    p <- p +
      ggplot2::guides(
        size = ggplot2::guide_legend(title = if(is.null(node_title)) format_name(node_size) else node_title),
        fill = "none"
      )
  }

  # Hide artefact from legend
  if(!isTRUE(node_size %in% igraph::vertex_attr_names(g)))
    p <- p + ggplot2::guides(size = "none")
  # ------------------------------------------------------------
  # Final styling
  # ------------------------------------------------------------
  if(!is.null(legend_position)){
    legend_position = "right"
  }

  p <- p +
    ggplot2::theme_void() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      legend.position = legend_position,
      plot.margin = plot_margin,
      legend.title = ggplot2::element_text(
        face = "bold",      # bold, italic, plain
        size = 12,
        colour = "black"
      )
    )
  
  if(!size_legend){
    p <- p + ggplot2::guides(fill = "none")
  }
  
  nudge_x <- nudge_label[1]
  nudge_y <- nudge_label[2]

  # ------------------------------------------------------------
  # Labels (done last since we produce two plots)
  # ------------------------------------------------------------
  if (label_card) {
    p_labelled <- p +
      ggraph::geom_node_label(
        data = node_df,
        ggplot2::aes(
          x = x,
          y = y,
          label = format_name(name)
        ),
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        label.r = unit(0.2, "lines"),
        size = text_size,
        fill = "white",
        fontface = text_face
      )
  } else {
    p_labelled <- p +
      ggraph::geom_node_text(
        data = node_df,
        ggplot2::aes(
          x = x,
          y = y,
          label = format_name(name)
        ),
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        size = text_size,
        fill = "white",
        fontface = text_face
      )
  }
  
  return(list(p, p_labelled))
}
