

#' Test pyin is working
#'
#' @param if_bad_result_return_single_na
#'
#' @return
#' @export
#'
#' @examples
test_pyin <- function(if_bad_result_return_single_na = TRUE) {
  pyin(file_name = system.file('extdata/test.wav', package = 'pyin'),
       if_bad_result_return_single_na = if_bad_result_return_single_na)
}


#' Compute pYIN on an audio track
#'
#' @param file_name
#' @param transform_file
#' @param normalise
#' @param hidePrint
#' @param type
#' @param if_bad_result_return_single_na
#'
#' @return
#' @export
#'
#' @examples
pyin <- function(file_name,
                 transform_file = NULL,
                 normalise = FALSE,
                 hidePrint = TRUE,
                 type = c("notes", "pitch_track", "both"),
                 if_bad_result_return_single_na = TRUE) {

  if(length(type) > 1) {
    type <- type[1]
  }

  stopifnot(assertthat::is.string(file_name),
            assertthat::is.string(transform_file) | is.null(transform_file),
            is.logical(normalise),
            is.logical(hidePrint),
            assertthat::is.string(type),
            is.logical(if_bad_result_return_single_na),
            type %in% c("notes", "pitch_track", "both"))

  if(type == "both") {

    notes_res <- pyin_single(file_name,
                       transform_file,
                       normalise,
                       hidePrint,
                       "notes",
                       if_bad_result_return_single_na)

    pitch_track_res <- pyin_single(file_name,
                             transform_file,
                             normalise,
                             hidePrint,
                             "pitch_track",
                             if_bad_result_return_single_na)

    res <- list(notes = notes_res,
                pitch_track = pitch_track_res)

  } else {
    res <- pyin_single(file_name,
                       transform_file,
                       normalise,
                       hidePrint,
                       type,
                       if_bad_result_return_single_na)
  }


  return(res)
}


pyin_single <- function(file_name,
                        transform_file,
                        normalise,
                        hidePrint,
                        type,
                        if_bad_result_return_single_na) {

  op_sys <- get_os()


  if(op_sys %in% c("osx", "linux", "windows")) {

    set_vamp_variable(op_sys)

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
    warning('OS not supported.')
  }


  cond <- is.null(res$freq) | all(is.na(res$freq))
  if(if_bad_result_return_single_na & cond) {
    res <- NA
  }
  return(res)
}

set_vamp_variable <- function(os) {

  if(os == "linux") {
    set_linux()
  } else if(os == "osx") {
    set_osx()
  } else if(os == "windows") {
    set_windows()
  } else {
    warning("Only Linux or Windows 64 bit is currently supported")
  }

}



set_windows <- function() {

  # package library path
  pkg_path <- system.file('bin/windows64', package = 'pyin')

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path one
  vamp_path1 <- homePath <- paste0(fs::path_home(), 'C:\\Program Files\\Vamp Plugins')

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, sep = ";")

  Sys.setenv(VAMP_PATH = dirs)
  Sys.getenv("VAMP_PATH")
}

set_osx <- function() {

  # package library path
  pkg_path <- system.file('bin/osx', package = 'pyin')

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path one
  vamp_path1 <- paste0(fs::path_home(), '/Library/Audio/Plug-Ins/Vamp')

  # potential library path 2
  vamp_path2 <- '/Library/Audio/Plug-Ins/Vamp'

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, vamp_path2, sep = ":")

  Sys.setenv(VAMP_PATH = dirs)
}


set_linux <- function() {

  # package library path
  pkg_path <- system.file('bin/linux64', package = 'pyin')

  # in case the user already has VAMP plugins installed

  vamp_path0 <- system2("echo", args = "$VAMP_PATH")

  # potential library path 1
  vamp_path1 <- paste0('/usr/local/lib/vamp')

  # put all together separated by a colon
  dirs <- paste(pkg_path, vamp_path0, vamp_path1, sep = ":")

  Sys.setenv(VAMP_PATH = dirs)
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

  if(os == "osx") {
    cmd <- system.file('bin/osx/sonic-annotator', package = 'pyin')
  } else if(os == "windows") {
    cmd <- system.file('bin/windows64/sonic-annotator64.exe', package = 'pyin')
  } else if(os == "linux") {
    cmd <- system.file('bin/linux64/sonic-annotator', package = 'pyin')
  } else {
    warning('OS not supported.')
  }

  if(hidePrint) {
    sa_out <- system2(command = cmd,
                      args = args,
                      stdout = TRUE, stderr = FALSE)
  } else {
    sa_out <- system2(command = cmd,
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


get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# t <- test_pyin()

# for multiple files...
