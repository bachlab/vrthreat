#' Generate position list
#' 
#' This helper function takes a scenario hierarchy object containing values for 
#' `position` and `euler_angles`, and combines these into a standardised 
#' position/rotation list usable for other functions.
#'
#' @param scenario_object 
#'
#' @returns A list with 6 items, `"pos_x"`, `"pos_y"`, and `"pos_z"`,
#'   the x, y, and z position of the object in Unity units (m), as well as  the
#'   Euler Angles  
#' @export
#'
#' @examples
generate_position_list <- function(scenario_object) {
  names(scenario_object[["position"]])     <- c("pos_x", "pos_y", "pos_z")
  names(scenario_object[["euler_angles"]]) <- c("rot_x", "rot_y", "rot_z")
  
  c(scenario_object[["position"]], scenario_object[["euler_angles"]])
}

#' Find Start marker position and rotation
#'
#' Finds the position and rotation of the `"Stand Here"` marker at trial start
#'
#' @param scenario_data The scenario data list (should be read from scenario
#'   .json file)
#'
#' @return A list representing the position of the Stand Here marker at trial
#'   start. The list contains 6 items, `"pos_x"`, `"pos_y"`, and `"pos_z"`,
#'   the x, y, and z position of the object in Unity units (m), as well as  the
#'   Euler Angles  `"rot_x"`, `"rot_y"`, and `"rot_z"`. NULL if none found.
#' @export
#'
#' @examples
find_start_position <- function(scenario_data) {
  start_pos <- find_hierarchy_object_any(
    scenario_data,
    "StartPoint",
    include_children = TRUE
  )
  
  generate_position_list(start_pos)
}


#' Find fruit position
#'
#' Finds the position of the `"Stand Here"` marker of the first found object
#' that matches the fruit task object names (`fruit_gameobject_names`). This
#' looks for child objects with name `"WalkHere"`.
#'
#' @param scenario_data The scenario data list (should be read from scenario
#'   .json file)
#'
#' @return A list representing the position of the Stand Here marker of the
#'   first found fruit task object in the scenario data. The list contains 6 items,
#'    `"pos_x"`, `"pos_y"`, and `"pos_z"`,
#'   the x, y, and z position of the object in Unity units (m), as well as  the
#'   Euler Angles  `"rot_x"`, `"rot_y"`, and `"rot_z"`. NULL if none found.
#' @export
#'
#' @examples
find_fruit_position <- function(scenario_data) {
  
  fruit_task <- find_hierarchy_object_any(
    scenario_data,
    fruit_gameobject_names,
    include_children = TRUE
  )
  
  if (is.null(fruit_task)) return(NULL)
  
  walkhere <- find_hierarchy_object(fruit_task, "WalkHere")
  
  if (is.null(walkhere)) return(NULL)
  
  generate_position_list(walkhere)
  
}


#' Find safe position
#'
#' Finds the position of the Safe House. This looks objects with name
#' `"SafeHouse"`. If there are multiple, returns the first one found.
#'
#' @param scenario_data The scenario data list (should be read from scenario
#'   .json file)
#'
#' @return A list representing the position of the first found Safe House object
#'   in the scenario data.  The list contains 6 items, `"pos_x"`, `"pos_y"`, and `"pos_z"`,
#'   the x, y, and z position of the object in Unity units (m), as well as  the
#'   Euler Angles  `"rot_x"`, `"rot_y"`, and `"rot_z"`. NULL if none found.
#' @export
#'
#' @examples
find_safe_position <- function(scenario_data) {
  safehouse <- find_hierarchy_object(scenario_data, "SafeHouse")
  
  if (is.null(safehouse)) safehouse <- find_hierarchy_object(scenario_data, "Safe No Cracks")
  if (is.null(safehouse)) return(NULL)
  
  generate_position_list(safehouse)
}

#' Find scenario position
#'
#' Finds the position of the scenario wrt world coordinates. All tracker data and
#' scenario data are expressed wrt to this position. The scenario position is 
#' only needed when relating to world coordinates (e.g. external motion capture).
#'
#' @param scenario_data The scenario data list (should be read from scenario
#'   .json file)
#'
#' @return A list representing the scenario position with 6 items, `"pos_x"`, 
#'   `"pos_y"`, and `"pos_z"`,  the x, y, and z position of the object in Unity 
#'   units (m), as well as  the Euler Angles  `"rot_x"`, `"rot_y"`, and `"rot_z"`. 
#'   NULL if none found.
#' @export
#'
find_scenario_position <- function(scenario_data) {
  generate_position_list(scenario_data)
}


#' Find Unity's internal threat name
#'
#' Finds the name that Unity uses for the threat object. This is useful for
#' analysing eyetracking data.
#'
#' @param scenario_data The scenario data list (should be read from scenario
#'   .json file)
#'
#' @return
#' @export
#'
#' @examples
find_unity_threat_name <- function(scenario_data) {

    threat_name <-
      find_hierarchy_object(scenario_data,
                          " Threat",
                          include_children = FALSE,
                          partial_match = TRUE)

    if(is.null(threat_name)) return(NA)

    threat_name$name

}

#' Find initial threat position from programmatic scenario planner
#'
#' This refers to the front most part of the threat and does not correspond to
#' threat position in the threat movement data frame which is in reference to
#' the centre of the threat
#'
#' @param scenario_data The scenario data list (should be read from scenario
#'   .json file)
#'
#' @return A list representing the threat position with 6 items, `"pos_x"`, 
#'   `"pos_y"`, and `"pos_z"`,  the x, y, and z position of the object in Unity 
#'   units (m), as well as  the Euler Angles  `"rot_x"`, `"rot_y"`, and `"rot_z"`. 
#'   NULL if none found.
#' @export
#' @export
#'
#' @examples
find_initial_threat_position <- function(scenario_data) {

  threat <- find_unity_threat_name(scenario_data)

  if (is.null(threat) || is.na(threat)) return(NULL)
  threat_pos <- find_hierarchy_object(scenario_data,
                                      threat,
                                      include_children = TRUE,
                                      partial_match = FALSE)

  if (is.null(threat_pos)) return(NULL)

  generate_position_list(threat_pos)

}

#' Get Time Of First Sequence Event
#'
#' Looks in the sequence data list to find the time at which the first
#' occurrence of a specified event name is called.
#'
#' @param sequence_data The sequence data list (i.e. `sequence0_T00X.json`
#'   parsed to a `list`).
#' @param event_type A vector containing the names event types. Should be
#'   one of `"Wait"`, `"Event Call"`, `"Animate Behaviour"`, `"Move Towards`,
#'   `"Rotate Towards"` `"Head Look"`. It can also be `"VerbaliseClip"`,
#'   a specific animation of type `"Animate Behaviour"`. The time of the first
#'   detected event is returned.
#'
#' @return A numeric value of the time (in Unity time, seconds since start-up)
#'   of the event. NA if none found.
#' @export
#'
#' @examples
get_time_of_first_event <- function(sequence_data, event_type) {

  if (is.null(sequence_data) ||
      all(is.na(sequence_data)))
    return(NA_real_)

  if (is.list(event_type)) {
    event_type <- unlist(event_type)
  }

  if ("VerbaliseClip" %in% event_type) {
    event_type <- c(event_type, "Animate Behaviour")
    animation_check <- "VerbaliseClip"
  } else {
    animation_check <- NA
  }


results <- sequence_data$results

for (result in sequence_data$results) {
  if (result$event_type %in% event_type) {
     if ((
      result$event_type == "Animate Behaviour" &&
      !is.na(animation_check) &&
      !is.null(result$animation) &&
      result$animation == animation_check
    ) ||
    result$event_type != "Animate Behaviour" ||
    is.na(animation_check)
    ) {
      time <- result$start_time
      if (is.null(time))
        time = result$call_time

      if (!is.null(time))
        return(time)
    }
  }
}
return(NA_real_)
}

#' Get Time Of First Sequence Event across multiple sequence columns
#'
#' Looks in the specified rows find the time at which the first
#' occurrence of a specified event name is called, and takes the first across
#' all sequence columns
#'
#' @param df The entire trial_results data frame
#' @param event_type A string containing the name of the event type column. This
#' should contain a character vector of `"Wait"`, `"Event Call"`,
#' `"Animate Behaviour"`, `"Move Towards`, `"Rotate Towards"` `"Head Look"`.
#'
#' @return The entire trial_results data frame with columns `first_event_time`
#' and `first_event_index`  added
#' @export
#'
#' @examples
#'
get_time_of_first_event_multi <- function(df, event_type) {
  event_type <- pull(df, event_type)

  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("sequence") &
          tidyselect::ends_with("_data_0"),
        ~ get_time_of_first_event(.x, event_type[cur_group_rows()]),
        .names = "first_event_{.col}"
      ),
      first_event_time = find_min(dplyr::c_across(
        tidyselect::starts_with("first_event_sequence")
      )),
      first_event_index = find_argmin(dplyr::c_across(
        tidyselect::starts_with("first_event_sequence")
      ))
    ) %>%
    select(!tidyselect::starts_with("first_event_sequence"))
}


#' Flatten a nested "scene json" list into a tidy data frame
#'
#' Walk a nested scene (Unity-style) and return one row per node with:
#' path, name, pos_x/y/z, eul_x/y/z, and (optionally) depth (root = 0).
#'
#' @param x             A scene node, a list of nodes, or a container list.
#' @param root_path     Starting label for the path column. Default: "root".
#' @param as_tibble     Return a tibble (TRUE) or data.frame (FALSE).
#' @param include_depth Include a `depth` column (root = 0). Default: TRUE.
#' @return A tibble/data.frame with one row per node.
#' @examples
#' df_nested <- df %>%
#'  ungroup() %>%
#'  mutate(
#'    scenario_data_0_flat = map(
#'      scenario_data_0,
#'      ~ flatten_scenario_data(.x)
#'    )
#'  )
#' @export
flatten_scenario_data <- function(
  x,
  root_path = "root",
  as_tibble = TRUE,
  include_depth = TRUE
) {
  # ---- small helpers ----

  # simple, readable "default if NULL" helper
  default_if_null <- function(x, default) if (is.null(x)) default else x
  num_or_na <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }
    as.numeric(x)
  }

  is_scene_node <- function(x) {
    is.list(x) &&
      length(intersect(
        names(x),
        c("name", "position", "euler_angles", "children")
      )) >
        0
  }

  make_empty_tbl <- function() {
    out <- tibble::tibble(
      path = character(0),
      name = character(0),
      pos_x = numeric(0),
      pos_y = numeric(0),
      pos_z = numeric(0),
      eul_x = numeric(0),
      eul_y = numeric(0),
      eul_z = numeric(0)
    )
    if (include_depth) {
      out$depth <- integer(0)
    }
    out
  }

  row_from_node <- function(node, path, depth) {
    tib <- tibble::tibble(
      path = path,
      name = default_if_null(node$name, NA_character_),
      pos_x = num_or_na(node$position$x),
      pos_y = num_or_na(node$position$y),
      pos_z = num_or_na(node$position$z),
      eul_x = num_or_na(node$euler_angles$x),
      eul_y = num_or_na(node$euler_angles$y),
      eul_z = num_or_na(node$euler_angles$z)
    )
    if (include_depth) {
      tib$depth <- depth
    }
    tib
  }

  # ---- recursive walker ----
  rec <- function(obj, path, depth) {
    if (!is.list(obj)) {
      return(make_empty_tbl())
    }

    if (is_scene_node(obj)) {
      here <- row_from_node(obj, path, depth)
      children <- default_if_null(obj$children, list())

      if (length(children) == 0) {
        return(here)
      }

      kids <- purrr::map_dfr(
        seq_along(children),
        function(i) {
          child_path <- sprintf("%s$children[[%d]]", path, i)
          rec(children[[i]], path = child_path, depth = depth + 1L)
        }
      )
      return(dplyr::bind_rows(here, kids))
    }

    # container list: iterate elements at the same depth
    if (length(obj) == 0) {
      return(make_empty_tbl())
    }
    purrr::map_dfr(
      seq_along(obj),
      function(i) {
        elt_path <- sprintf("%s[[%d]]", path, i)
        rec(obj[[i]], path = elt_path, depth = depth)
      }
    )
  }

  out <- rec(x, path = root_path, depth = 0L)
  if (isTRUE(as_tibble)) out else as.data.frame(out)
}
