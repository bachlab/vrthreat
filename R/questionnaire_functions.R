#' Replace Redcap id in questionnaires by VR ppid
#'
#' Replaces the Redcap record id of the questionnaire data by the ppid of the VR data.
#' Removes records of participants that did the questionnaire several times or didn't do the VR study.
#'
#' @param df raw questionnaire data frame
#' @param redcap_ppid_map redcap id (survey_id) to VR id (ppid) map
#'
#' @return raw questionnaire data frame with updated ppid_quest
#' @export
#'
#' @examples
replace_redcapid_ppid <- function(df, redcap_ppid_map){

  df  %>%
    dplyr::rename(ppid_quest = colnames(df)[1])%>%
    # only keep last instance if participant repeated quest
    dplyr::filter(!duplicated(ppid_quest, fromLast=TRUE)) %>%
    # remove participants that didn't do the VR
    dplyr::filter(!dplyr::row_number() %in%  which(ppid_quest %in% redcap_ppid_map$survey_id == F)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ppid_quest = with(redcap_ppid_map,
                        ifelse(ppid_quest %in% survey_id,
                               as.character(ppid[which(survey_id == ppid_quest)]),
                               NA)))

}

#' Read raw (csv) questionnaire data
#'
#' Read the questionnaire data csv files.
#' Removes the participants that didn't finish or that were excluded.
#' Removes unnecessary columns.
#' Maps ppid using fct replace_redcapid_ppid.
#'
#' @param rawquestpath The path that contains the questionnaires raw data files
#' @param prefix_fn Beginning of the questionnaire file name. Does not have to be exact as it
#' will be matched with the files in the rawquestpath.
#' 3 csv files from Redcap exist:
#' - Ad
#' - Pr
#' - Po
#' @param participants_excluded list of excluded participants id (ppid)
#' @param redcap_ppid_map redcap id (survey_id) to VR id (ppid) map
#'
#' @return A data frame with the raw questionnaire data where each row is a participant and each column is a questionnaire item.
#' @export
#'
#' @examples
read_quest_raw <- function(rawquestpath, prefix_fn, participants_excluded, redcap_ppid_map){

  # find complete file name with the given prefix file name
  comp_fn = grep(prefix_fn, list.files(rawquestpath), value = TRUE)

  readr::read_csv(
    file.path(rawquestpath, comp_fn),
    show_col_types = FALSE)  %>%
    # remove participants that didn't finish (when last column is not 2)
    dplyr::filter(!dplyr::row_number() %in%  which(.[,ncol(.)] != 2)) %>%
    # remove unnecessary columns
    dplyr::select(-c("redcap_survey_identifier", "record_id",
                     grep("_complete" , names(.)),
                     grep("_timestamp", names(.)))) %>%
    replace_redcapid_ppid(redcap_ppid_map)%>%
    # filter out excluded participants
    dplyr::anti_join(participants_excluded, by = c("ppid_quest" = "ppid")) %>%
    dplyr::rename(ppid = ppid_quest)
}

#' Get MSSQ subscale score (Motion Sickness; Golding, 2006)
#'
#' @param qn MSSQ subscale name (mssq_a or mssq_c)
#' @param df Data frame containing all the column names of the questionnaire data
#'
#' @return Score of MSSQ subscale
#' @export
#' @examples
sum_mssq <- function(qn, df){
  if (any(grepl(qn,colnames(df)))) {
    qn = dplyr::c_across(starts_with(qn))
    sum((qn[qn > 0]*9)/(9 - sum(qn == -1)))}
  else {
    NA
  }
}

#' Get sum of a questionnaire if questionnaire exists
#'
#' @param qn Name of the questionnaire
#' @param df Data frame containing all the column names of the questionnaire data
#'
#' @return Sum of all the items of that questionnaire
#' @export
#' @examples
sum_quest <- function(qn, df) {
  ifelse(any(grepl(qn,colnames(df))),
         sum(dplyr::c_across(starts_with(qn))),
         NA)
}

#' Get sum of a questionnaire subscale
#'
#' @param qn Name of the questionnaire
#' @param qnb Index of the items to include
#' @param df Data frame containing all the column names of the questionnaire data
#'
#' @return Sum of the specified questionnaire items
#' @export
#'
#' @examples
sub_sum_quest <- function(qn, qnb, df){
  ifelse(any(grepl(qn,colnames(df))),
         sum(dplyr::c_across(num_range(qn, qnb))),
         NA)
}

#' Read and score questionnaire data
#'
#' @param rawquestpath The path that contains the questionnaires raw data files
#' @param metapath The path that contains the meta data. This must contain the
#' following 2 csv files: participants_excluded.csv, ppid_map.csv
#' @param raw (by default = FALSE) Determines the output. Read "Value" for more information.
#'
#' @return By default (when raw = FALSE): data frame containing the scored data of all the questionnaires.
#' Each row is a participant and each column is the score or subscore of a questionnaire
#'
#' If raw = TRUE: data frame containing the raw data of all the questionnaires.
#' Each row is a participant and each column is the raw answer to a questionnaire item
#'
#' @export
#'
#' @examples
read_quest_data <- function(rawquestpath, metapath, raw = FALSE){

  # read excluded participants
  participants_excluded <-
    readr::read_csv(
      file.path(metapath,
                "participants_excluded.csv"),
      col_types = readr::cols(ppid = readr::col_character()),
      n_max = 500
    )

  # read redcap id to ppid map
  redcap_ppid_map <-
    readr::read_csv(
      file.path(metapath,
                "ppid_map.csv"),
      col_types = readr::cols(
        ppid = readr::col_character(),
        survey_id = readr::col_character()),
      n_max = 500
    )

  # --- Advance questionnaire (a few days before the VR experiment)
  quest_adv <- read_quest_raw(rawquestpath, "Ad",
                              participants_excluded, redcap_ppid_map)

  quest_adv_pp <- quest_adv%>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ppid = ppid,

      # MSSQ - Motion Sickness (Golding, 2006)
      mssq_a = sum_mssq("mssq_a", .),
      mssq_c = sum_mssq("mssq_c", .),
      mssq = mssq_a + mssq_c,

      # STICSA - Anxiety Trait (Ree et al., 2000)
      sticsa_t = sum_quest("sticsa_t", .),
      sticsa_t_somatic = sub_sum_quest("sticsa_t_", c(1, 2, 6, 7, 8, 12, 14, 15, 18, 20, 21), .),
      sticsa_t_cog =  sub_sum_quest("sticsa_t_",  c(3, 4, 5, 9, 10, 11, 13, 16, 17, 19), .),

      # BSSS - Sensation Seeking (Hoyle et al., 2002)
      bsss = sum_quest("bsss", .),
      bsss_exp = sub_sum_quest("bsss_", c(1, 5), .),
      bsss_bored = sub_sum_quest("bsss_", c(1, 5), .),
      bsss_thrill = sub_sum_quest("bsss_", c(3, 7), .),
      bsss_disin = sub_sum_quest("bsss_", c(4, 8), .),

      # DPSS-12 - Disgust (Fergus and Valentiner, 2009)
      dpss = sum_quest("dpss", .),
      dpss_pro = sub_sum_quest("dpss_", c(1, 4, 5, 6, 8, 10), .),
      dpss_sen = sub_sum_quest("dpss_", c(2, 3, 7, 9, 11, 12), .),

      # FSS-III - Fear (Arrindell et al., 2003)
      fss = sum_quest("fss", .),
      fss_bod = sub_sum_quest("fss_", c(1, 4, 12, 14, 22, 32, 36, 38, 39, 43, 46, 50), .),
      fss_ago = sub_sum_quest("fss_", c(2, 3, 6, 7, 11, 17, 18, 19, 23, 24, 35, 40, 42), .),
      fss_soc = sub_sum_quest("fss_", c(5, 8, 9, 10, 15, 20, 27, 33, 41, 44, 47, 51, 52), .),
      fss_anim = sub_sum_quest("fss_", c(13, 16, 21, 29, 37, 45), .),
      fss_sex = sub_sum_quest("fss_", c(25, 26, 28, 30, 31, 34, 48, 49), .),

      # General Fear Factor (Valadao-Dias et al., 2016)
      fss_bod_gff = ifelse(any(grepl("fss",colnames(.))),
                           mean(c(fss_1*.62, fss_4*.52, fss_12*.46, fss_14*.48, fss_22*.59, fss_36*.70, fss_38*.69)),
                           NA),
      fss_ago_gff = ifelse(any(grepl("fss",colnames(.))),
                           mean(c(fss_3*.63, fss_6*.47, fss_11*.58, fss_23*.53, fss_40*.67, fss_42*.46)),
                           NA),
      fss_soc_gff = ifelse(any(grepl("fss",colnames(.))),
                           mean(c(fss_5*.44, fss_8*.56, fss_9*.59, fss_27*.51, fss_33*.66, fss_41*.78, fss_44*.73, fss_47*.73, fss_52*.71)),
                           NA),
      fss_anim_gff = ifelse(any(grepl("fss",colnames(.))),
                            mean(c(fss_13*.68, fss_16*.69, fss_21*.60, fss_29*.71, fss_37*.59, fss_45*.64)),
                            NA),
      fss_agg_gff = ifelse(any(grepl("fss",colnames(.))),
                           mean(c(fss_25*.71, fss_26*.65, fss_28*.50, fss_30*.72, fss_34*.64)),
                           NA),
      fss_gff = ifelse(any(grepl("fss",colnames(.))),
                       mean(c(fss_ago_gff*.91, fss_bod_gff*.79, fss_anim_gff*.84, fss_agg_gff*.89, fss_soc_gff*.75)),
                       NA),

      #SNAQ-12 - Snake (Zsido et al., 2018)
      snaq = sum_quest("snaq", .),

      #SPQ-12 Spider (Zsido et al., 2018)
      spq = sum_quest("spq", .),

      # BMI
      bmi = ifelse(any(grepl("general_weight",colnames(.))),
                   round(general_weight / ((general_height/100)^2), 2),
                   round(body_weight / ((body_height/100)^2), 2)),

      # How many hours per week do you spend working out, playing sports, or training?
      sport_hours_week = ifelse(any(grepl("general_active",colnames(.))),
                                general_active,
                                NA),

      # Martial Art (1: 1 - 6 months, 2: 1 year, 3: 1 - 2 years, 4: 3 - 4 years, 5: > 5 years)
      martial_art_years = ifelse(any(grepl("m_a_1",colnames(.))),
                                 pmax(m_a_5, m_a_8, na.rm = TRUE),
                                 NA),

      # VGUQ - Video Game (Tolchinsky, 2013)
      vg_hours_week = ifelse(any(grepl("vg_hours_week",colnames(.))),
                             vg_hours_week,
                             NA),

      .keep = "none")

  # --- Pre questionnaire (a few min before the VR experiment)
  quest_pre <-read_quest_raw(rawquestpath, "Pr",
                             participants_excluded, redcap_ppid_map)

  quest_pre_pp <- quest_pre%>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ppid = ppid,

      # STICSA - Anxiety State (Ree et al., 2000)
      sticsa_s = sum_quest("sticsa_s", .),
      sticsa_s_somatic = sub_sum_quest("sticsa_s_", c(1, 2, 6, 7, 8, 12, 14, 15, 18, 20, 21), .),
      sticsa_s_cog =  sub_sum_quest("sticsa_s_",  c(3, 4, 5, 9, 10, 11, 13, 16, 17, 19), .),

      # Diagnosed mental illness:
      # 1: depression 2: dysthymia 3: adjustment disorder 4: dissociative disorder
      # 5: somatoform disorder 6: eating disorder 7: sleep disorder
      mental_illness =
        if(any(grepl("mental_health",colnames(.)))) {
          ifelse(mental_health___0 == 1,
                 NA,
                 list(which(dplyr::c_across(starts_with("mental_health")) != 0)))},

      # Orthopedic health:
      # 1: strained ankle 2: fractured lower limb 3: other fracture 4: surgery lower limb
      # 5: other lower limb 6: back 7: other condition
      orthopedic_health =
        if(any(grepl("orthopedic_health",colnames(.)))) {
          ifelse(orthopedic_health___0 == 1,
                 NA,
                 list(which(dplyr::c_across(starts_with("orthopedic_health")) != 0)))},

      .keep = "unused")

  # ====== Post questionnaires (a few min after the VR experiment)

  quest_post <-read_quest_raw(rawquestpath, "Po",
                              participants_excluded, redcap_ppid_map)

  quest_post_pp <- quest_post%>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ppid = ppid,
      # SSQ
      ssq = (sub_sum_quest("ssq_", c(1, 6, 7, 8, 9, 15, 16), .) +
               sub_sum_quest("ssq_", c(1, 2, 3, 4, 5, 9, 11),. ) +
               sub_sum_quest("ssq_", c(5, 8, 10, 11, 12, 13, 14),.)) * 3.74,

      # VRSQ (Kim et al., 2018)
      vrsq_dis = sub_sum_quest("ssq_", c(3, 10, 11, 13),.)/15 * 100, # Disorientation
      vrsq_oculo = sub_sum_quest("ssq_", c(1, 2, 4, 5),.)/12 * 100, # Oculomotor
      vrsq = (vrsq_oculo + vrsq_dis)/2,

      # CSQ (Stone III, 2017)
      csq_dizz = csq_f1, # Dizziness
      csq_focus = csq_f2,  # Difficulty in focusing

      # Unused also include SSQ Nausea, Oculomotor, and Disorientation
      .keep = "unused")

  ifelse(
    raw == TRUE,
    quest <- quest_adv %>%
      dplyr::full_join(quest_pre, by ="ppid") %>%
      dplyr::full_join(quest_post, by ="ppid")
    ,
    quest <- quest_adv_pp %>%
      dplyr::full_join(quest_pre_pp, by ="ppid") %>%
      dplyr::full_join(quest_post_pp, by ="ppid")
  )

  return(quest)

}
