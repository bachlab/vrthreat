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

  names(start_pos[["position"]])     <- c("pos_x", "pos_y", "pos_z")
  names(start_pos[["euler_angles"]]) <- c("rot_x", "rot_y", "rot_z")

  c(start_pos[["position"]], start_pos[["euler_angles"]])

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

  names(walkhere[["position"]])     <- c("pos_x", "pos_y", "pos_z")
  names(walkhere[["euler_angles"]]) <- c("rot_x", "rot_y", "rot_z")

  c(walkhere[["position"]], walkhere[["euler_angles"]])
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

  names(safehouse[["position"]])  <- c("pos_x", "pos_y", "pos_z")
  names(safehouse[["euler_angles"]]) <- c("rot_x", "rot_y", "rot_z")

  c(safehouse[["position"]], safehouse[["euler_angles"]])
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
#' @return
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

  threat_pos$position

}

#' Get Time Of First Sequence Event
#'
#' Looks in the sequence data list to find the time at which the first
#' occurrence of a specified event name is called.
#'
#' @param sequence_data The sequence data list (i.e. `sequence0_T00X.json`
#'   parsed to a `list`).
#' @param event_type A string containing the name of the event type. Should be
#'   one of `"Wait"`, `"Event Call"`, `"Animate Behaviour"`, `"Move Towards`,
#'   `"Rotate Towards"` `"Head Look"`.
#'
#' @return A numeric value of the time (in Unity time, seconds since start-up)
#'   of the event. NA if none found.
#' @export
#'
#' @examples
get_time_of_first_event <- function(sequence_data, event_type) {

  if (is.null(sequence_data) || all(is.na(sequence_data))) return(NA_real_)

  results <- sequence_data$results

  for (result in sequence_data$results) {
    if (result$event_type == event_type) {
      time <- result$start_time
      if (is.null(time)) time = result$call_time

      if (!is.null(time)) return(time)
    }
  }

  return(NA_real_)
}

