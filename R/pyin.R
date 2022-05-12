
#' Test pyin is working
#'
#' @return
#' @export
#'
#' @examples
test_pyin <- function() {
  pyin(file_name = system.file('extdata/test.wav', package = 'pyin'))
}

#' Compute pYIN on an audio track
#'
#' @param file_name
#' @param transform_file
#' @param normalise
#' @param hidePrint
#' @param type
#'
#' @return
#' @export
#'
#' @examples
pyin <- function(file_name, transform_file = NULL,
                 normalise = FALSE, hidePrint = TRUE, type = "notes") {

  if(.Platform$OS.type == "unix") {

    op_sys <- "linux64" # NB only one working for now, but eventually update to host multiple OS's

    vamp_cmd <- get_correct_vamp_cmd(type)

    args <- pyin_construct_args(transform_file, vamp_cmd, file_name, normalise)

    sa_out <- pyin_construct_command(args, hidePrint, op_sys)

    if(length(sa_out) == 0) {
      res <- pyin_handle_null(type, file_name)
    } else {

      res <- read.csv(text = sa_out, header = FALSE) %>% tibble::as_tibble()
      res <- pyin_tidy(res, type)

      file_name <- res$V1[[1]]

      res <- res %>% dplyr::select(-V1)

      res <- tibble::tibble(file_name, res)
    }
  } else {
    warning('Currently only unix supported.')
  }
}

pyin_tidy <- function(res, type) {
  if(type == "notes") {
    res <- res %>%
      dplyr::rename(onset = V2, dur = V3, freq = V4) %>%
      dplyr::mutate(
        onset = round(onset, 2),
        dur = round(dur, 2),
        freq = round(freq, 2),
        note = round(hrep::freq_to_midi(freq)))
  } else {
    res <- res %>%
      dplyr::rename(onset = V2, freq = V3) %>%
      dplyr::mutate(
        onset = round(onset, 2),
        freq = round(freq, 2))
  }
}

pyin_construct_args <- function(transform_file, vamp_cmd, file_name, normalise) {
  if(is.null(transform_file)) {
    args <- c("-d",
              vamp_cmd,
              file_name,
              "-w",
              "csv --csv-stdout")
  } else {
    args <- c(paste0('-t ', transform_file),
              file_name,
              "-w",
              "csv --csv-stdout")
  }

  if(normalise == 1) {
    args <- c(args, "--normalise")
  }
  args
}

pyin_construct_command <- function(args, hidePrint, os) {

  cmd <- system.file(paste0('bin/', os, '/sonic-annotator'), package = 'pyin')

  print(cmd)

  if(hidePrint) {
    sa_out <- system2(command = cmd,
                      args = args,
                      stdout = TRUE, stderr = FALSE)
  } else {
    sa_out <- system2(command = sonic_annotator_location,
                      args = args,
                      stdout = TRUE)
  }
}


pyin_handle_null <- function(type, file_name) {
  if(type == "notes") {
    res <- tibble::tibble(onset = NA, dur = NA, freq = NA, note = NA, file_name = file_name)
  } else {
    res <- tibble::tibble(onset = NA, freq = NA, file_name = file_name)
  }
}

get_correct_vamp_cmd <- function(type) {

  if(type == "pitch_track") {
    "vamp:pyin:pyin:smoothedpitchtrack"
  } else if(type == "notes") {
    "vamp:pyin:pyin:notes"
  } else {
    stop("Unknown type")
  }
}

# t <- test_pyin()

