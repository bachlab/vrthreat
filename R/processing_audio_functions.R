#' Play audio section
#'
#' Plays an audio recording from 'from' to 'to', using the tuneR library and a native media player.
#' For Windows Media Player, player needs to be closed manually
#' tuneR needs write permissions, so a temporary file is copied to the current directory and deleted afterwards
#' This allows playing sounds from a write-protected directory but may make the function slow
#'
#' @param audiofile A wave file
#' @param from      Play from this time point (in seconds after file start), default 0 (play from file start)
#' @param to        Play to this time point  (in seconds after file start), default NA (play to file end)
#'
#'
#' @return Returns no value
#' @export
#'
#' @examples
play_audio_section <- function(audiofile,
                               from = 0,
                               to = NA) {
  print(audiofile)
  file.copy(audiofile, basename(audiofile))
  from <- ifelse(is.na(from), 0, from)
  sound <- tuneR::readWave(basename(audiofile))
  start_indx <- max(c(1, round(from * sound@samp.rate)))
  stop_indx  <-
    ifelse(is.na(to), length(sound@left), max(c(
      round(to * sound@samp.rate), length(sound@left)
    )))
  sound@left <- sound@left[start_indx:stop_indx]
  tuneR::play(sound)
  # tuneR tries to close the player but this does  not seem to work for WMP used on our fellows PC. I could not identify a command line parameter
  # that closes the player: https://docs.microsoft.com/en-us/windows/win32/wmp/command-line-parameters
  file.remove(basename(audiofile))
}

#' Play audio recording from threat appearance to the end
#'
#' Finds threat start and location of microphone recording in a data frame, and plays the audio from the start of the threat, using 'play_audio_section'.
#' Expects columns 'threat_appear_time', 'start_time', 'mic.wav_location_0'. Expects absolute paths, use `update_file_pointers` to
#' update them if necessary.
#'
#' @param df        A trial results data frame
#'
#' @return Returns no value
#' @export
#'
#' @examples
play_audio_from_threat <- function(df) {

  start_time <- df$threat_appear_time - df$start_time
  purrr::map2(df$mic.wav_location_0, start_time, play_audio_section)
}


#' Automatically detect sounds in audio recording using a volume/duration threshold
#'
#' This function takes vectors of volume and duration thresholds and uses them to
#' detect above-threshold sounds. The duration threshold is evaluated cumulatively
#' across the entire recording (i.e. not necessarily contiguously). Default for
#' the threshold pair are optimised values from a labelled data set
#'
#' @param soundfile A wave file name
#' @param start_time A start time (default 0)
#' @param volume_threshold A (vector of) volume threshold(s)
#' @param duration_threshold A (vector of) duration threshold(s) of same size
#'
#' @return a logical vector of same length as the threshold vectors
#' @export
#'
#' @examples
detect_sound <-
  function(soundfile,
           start_time = 0,
           volume_threshold = 700,
           duration_threshold = .06) {

    detect_sound_in_audio_object <-
      function(volume_threshold,
               duration_threshold,
               sound,
               start_indx,
               stop_indx
      ) {
        vocs <-
          which(abs(sound@left[start_indx:stop_indx]) > volume_threshold)
        (length(vocs) > (duration_threshold * sound@samp.rate))
      }

    cat(soundfile, "\n")
    file.copy(soundfile, "tempfile.wav")
    if (is.na(start_time)) {
      sound_detected <- rep(FALSE, times = length(volume_threshold))
    } else {
      sound <- tuneR::readWave("tempfile.wav")
      start_indx <- max(c(1, round(start_time * sound@samp.rate)))
      stop_indx <- length(sound@left)

      sound_detected <-
        purrr::map2_lgl(volume_threshold,
                 duration_threshold,
                 detect_sound_in_audio_object,
                 sound,
                 start_indx,
                 stop_indx)
    }

    file.remove("tempfile.wav")
    return(sound_detected)
  }


#' Manually classify sounds
#'
#' Play previously detected sounds from threat appearance to end for manual
#' classification
#'
#' @param df A trial results data frame
#' @param trials An integer selection of trials to play (corresponding to rows in output file), default: all rows
#' @param sound_classification_file An output file name (existing files will be re-used; otherwise a new file is created), default: `sound_classification.csv`
#' @param sound_detection_file A file with previously detected sounds (expected columns: `sound_detected` with 0/1 entries, `sound_comment`), default: use all sounds
#'
#' @return
#' @export
#'
#' @examples
sound_classification_manual <-
  function (df,
            audio_path,
            trials = 1:nrow(df),
            sound_classification_file = "sound_classification.csv",
            sound_detection_file = "") {

    if (any(is.na(trials))) trials <- 1:nrow(df)

    # write (or read) output file
    if (!file.exists(sound_classification_file)) {
      trial_sound <- trial_results %>%
        # merge with ground truth analysis on full data set
        {
          if (file.exists(sound_detection_file)) {
            dplyr::left_join(.,
                             readr::read_csv(sound_detection_file),
                             by = "mic.wav_location_0",
                             keep = FALSE) %>%
              dplyr::filter(sound_detected == 1) %>%
              dplyr::filter(!is.na(threat_appear_time)) %>%
              dplyr::select(
                c(
                  "mic.wav_location_0",
                  "sound_comment",
                  "start_time",
                  "threat_appear_time"
                )
              )
          } else{
            dplyr::select(.,
                          c(
                            "mic.wav_location_0",
                            "start_time",
                            "threat_appear_time"
                          ))
          }
        }  %>%
        dplyr::mutate(
          sound_cat = NA,
          sound_transcription = "",
          transcription_comments = ""
        ) %>%
        readr::write_csv(file = sound_classification_file)
    } else {
      trial_sound <- readr::read_csv(file = sound_classification_file)
    }

    # play audio
    trial_sound %>%
      slice(trials) %>%
      rowwise() %>%
      play_audio_from_threat()
  }
