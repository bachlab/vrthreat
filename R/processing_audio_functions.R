#' Join a sound classification file into trial_results tibble
#' 
#' This function reads a sound classification file with column 
#' `mic.wav_location_0` (potentially pointing to a different study path than the 
#' trial_results) and joins it. It then parses the `sound_cat` column and parses
#' the classification to add the following one-hot-encoded columns: `any_voice`,
#' `undiff_voice`, `laughter`, `shriek`, `talk_to_threat`, `exclamation`, 
#' `talk_to_self`.
#'
#' Sound classification file must have the following columns: `mic.wav_location_0` and `sound_cat`. 
#' The following columns are added if they exist: `sound_comments`, `transcription`
#' The expected classification key for `sound_cat` is:
#' 0. No sound
#' 1. Non-differentiable (vowels, growls, long hissing)
#' 2. Laughing
#' 3. Shrieking
#' 4. Speech apparently directed at the threat 
#' 5. Exclamation of surprise, including expletives
#' 6. Talking to oneself
#' 7. Talking to the experimenter
#' 8. Breathing sounds (to be used as filter: only classified when none of category 1-7 applies)
#' 9. Ambient sounds (to be used as filter: only classified when none of category 1-7 applies)
#'
#' @param df A trial results tibble
#' @param sound_fn A sound classification file (alternatively: a preprocessed data frame)
#'
#' @returns The trial results tibble with added columns
#' @export
#'
#' @examples
join_sound_classification <- function(df, sound_fn) {
  if (is.character(sound_fn)) {
    sound_classification_data <-
      read_sound_classification(sound_fn)
  } else {
    sound_classification_data <- sound_fn
  }
  
  sound_classification_data <- sound_classification_data %>%
    dplyr::select(tidyselect::all_of(c("mic.wav_location_0", "sound_cat")), 
                  tidyselect::any_of(c("sound_comments", "transcription")))
  
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(new_mic = remove_study_path(mic.wav_location_0)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(sound_classification_data, by = dplyr::join_by(new_mic == mic.wav_location_0)) %>%
    dplyr::select(!c(new_mic)) %>%
    dplyr::mutate(
      sound_cat = if_else(is.na(sound_cat), "0", sound_cat),
      any_voice = if_else(str_detect(sound_cat, "[123456]"), 1, 0),
      undiff_voice = if_else(str_detect(sound_cat, "1"), 1, 0),
      laughter = if_else(str_detect(sound_cat, "2"), 1, 0),
      shriek = if_else(str_detect(sound_cat, "3"), 1, 0),
      talk_to_threat = if_else(str_detect(sound_cat, "4"), 1, 0),
      exclamation = if_else(str_detect(sound_cat, "5"), 1, 0),
      talk_to_self = if_else(str_detect(sound_cat, "6"), 1, 0)
    )
}



#' Read  manual sound detection file
#' 
#' Manual sound detection or classification files typically contain file pointers to study paths 
#' that are different from the original trial_results tibble. This function 
#' reads a sound classification csv into a tibble and and removes the study path 
#' from the audio file pointer
#'
#' @param fn 
#'
#' @returns
#' @export
#'
#' @examples
read_sound_classification <- function(fn) {
  readr::read_csv(fn) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(mic.wav_location_0 = remove_study_path(mic.wav_location_0)) %>%
    dplyr::ungroup()
}

#' Get audio section
#'
#' Retrieves an audio recording from 'from' to 'to', using the tuneR library. Returns a wave object.
#'
#' @param audiofile A wave file
#' @param from      Retrieve from this time point (in seconds after file start), default 0 (from file start)
#' @param to        Retrieve to this time point  (in seconds after file start), default NA (to file end)
#'
#'
#' @return Returns no value
#' @export
#'
#' @examples
get_audio_section <- function(audiofile,
                              from = 0,
                              to = NA) {
  from <- ifelse(is.na(from), 0, from)
  sound <- tuneR::readWave(basename(audiofile))
  start_indx <- max(c(1, round(from * sound@samp.rate)))
  stop_indx  <-
    ifelse(is.na(to), length(sound@left), max(c(
      round(to * sound@samp.rate), length(sound@left)
    )))
  sound@left <- sound@left[start_indx:stop_indx]

  return(sound)
}

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
  local_audiofile <- basename(audiofile)
  file.copy(audiofile, local_audiofile)

  get_audio_section(local_audiofile, from = from, to = to) %>%
   tuneR::play()

  # tuneR tries to close the player but this does  not seem to work for WMP used on our fellows PC. I could not identify a command line parameter
  # that closes the player: https://docs.microsoft.com/en-us/windows/win32/wmp/command-line-parameters
  file.remove(local_audiofile)
}

#' Write audio section
#'
#' Writes an audio recording from 'from' to 'to', using the tuneR library
#'
#' @param audiofile A wave file
#' @param newfile   A new wave file
#' @param from      Write from this time point (in seconds after file start), default 0 (from file start)
#' @param to        Write to this time point  (in seconds after file start), default NA (to file end)
#'
#'
#' @return Returns no value
#' @export
#'
#' @examples
write_audio_section <- function(audiofile,
                                newfile,
                                from = 0,
                                to = NA) {
  local_audiofile <- basename(audiofile)
  file.copy(audiofile, local_audiofile)


  get_audio_section(local_audiofile, from = from, to = to) %>%
    tuneR::writeWave(filename = newfile)
  file.remove(local_audiofile)
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
    sts <- file.copy(soundfile, "tempfile.wav")
    # deal with faulty files and edge cases
    if (is.na(start_time) | sts == FALSE | file.info("tempfile.wav")$size == 0) {
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
#' Play previously detected sounds from start or from threat appearance to end for manual
#' classification
#'
#' @param df A trial results data frame
#' @param trials An integer selection of trials to play (corresponding to rows in output file), default: all rows
#' @param sound_classification_file An output file name (existing files will be re-used; otherwise a new file is created), default: `sound_classification.csv`
#' @param sound_detection_file A file with previously detected sounds (expected columns: `sound_detected` with 0/1 entries, `sound_comment`), default: use all sounds
#' @param from A string indicating whether to play sound from "start" or from "threat" (default)
#'
#' @return
#' @export
#'
#' @examples
sound_classification_manual <-
  function (df,
            trials = 1:nrow(df),
            sound_classification_file = "sound_classification.csv",
            sound_detection_file = "",
            from = "threat") {

    if (any(is.na(trials))) trials <- 1:nrow(df)

    # write (or read) output file
    if (!file.exists(sound_classification_file)) {
      trial_sound <- df %>%
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
    play_sound <- function(df, from) {
      if (from == "start") {
        walk(df$mic.wav_location_0, play_audio_section)
      } else {
        play_audio_from_threat(df)
      }
    }

    trial_sound %>%
      slice(trials) %>%
      play_sound(from = from)

  }


#' Manually detect sounds
#'
#' Play all sounds of a study from start to end for manual
#' classification
#'
#' @param df A trial results data frame
#' @param trials An integer selection of trials to play (corresponding to rows in output file), default: all rows
#' @param sound_detection_file An output file name (existing files will be re-used; otherwise a new file is created), default: `sound_classification.csv`
#' @param sound_classification_file An output file name from an existing manual sound
#'  classification. Only used if sound_detection_file does not exist. In this case, all trials with already detected sounds will be set to 1 rather than NA.
#'
#' @return
#' @export
#'
#' @examples
sound_detection_manual <-
  function (df,
            trials = NA,
            sound_detection_file = "sound_detection.csv",
            sound_classification_file = NA) {
    if (any(is.na(trials)))
      trials <- NA

    # write (or read) output file
    if (!file.exists(sound_detection_file)) {
      trial_sound <- df %>%
        dplyr::select(c("mic.wav_location_0",
                        "start_time",
                        "res_path")) %>%
        dplyr::mutate(sound_detected = NA,
                      sound_comments = NA)


      if (!is.na(sound_classification_file)) {
        update_mic_location <- function(old_path) {
          old_path %>%
            split_path() %>%
            .[c(4, 3, 2, 1)] %>%
            as.list() %>%
            do.call(file.path, .)
        }

        old_sound <- read_csv(sound_classification_file) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(new_mic_location = update_mic_location(mic.wav_location_0)) %>%
          dplyr::select(c('new_mic_location', 'sound_cat'))

        trial_sound <- trial_sound %>%
          dplyr::rowwise() %>%
          dplyr::mutate(new_mic_location = update_mic_location(mic.wav_location_0)) %>%
          dplyr::left_join(old_sound, by = 'new_mic_location') %>%
          dplyr::mutate(
            sound_detected = if_else(str_detect(sound_cat, "[1234567]"), 1, sound_detected),
            sound_comments = if_else(
              sound_detected == 1,
              "detected from previous sound classification",
              sound_comments
            )
          ) %>%
          dplyr::select(!c(sound_cat, new_mic_location)) %>%
          update_file_pointers()
      }

      trial_sound %>%
        dplyr::select(!res_path) %>%
        readr::write_csv(file = sound_detection_file)
    } else {
      trial_sound <- readr::read_csv(file = sound_detection_file)
    }
    # assign trials
    if (is.na(trials)) {
      trials <- trial_sound %>%
        dplyr::mutate(row_number = dplyr::row_number()) %>%
        dplyr::filter(is.na(sound_detected)) %>%
        dplyr::pull(row_number)
    }

    trial_sound <- trial_sound %>%
      dplyr::slice(trials)

    walk(trial_sound$mic.wav_location_0, play_audio_section)
  }
