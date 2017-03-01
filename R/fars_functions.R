#' Read in data from a csv file
#'
#'\code{fars_read} returns a data.frame with data from the supplised csv file.
#'
#' The function takes a character vector of a path of a filename to read into R.
#' If the filename does not exist the function will stop and an error will be thrown.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @note fars_read depends on read_csv and tbl_df from the readr and dplyr packages respectively.
#'
#' @param filename a character vector of filename of a csv file to read
#'
#' @return this function returns a dataframe of the data read in from a csv file
#'
#' @examples
#' fars_read("data/filename.csv")
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' Creates a filename based on year input
#'
#'\code{make_filename} will create a character vector filename with an assocaited year input
#'
#' This takes a year input and creates a filename with this year.
#' If the year is does not pass \code{as.integer} it will have a value of NA, and the function
#' will throw an error after being passed to sprintf.
#'
#' @param year The year to be in the name of the file.
#'
#' @note This package does not depends on any extensions
#'
#' @return A character vector filename
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read in multiple files by year
#'
#'\code{fars_read_years} takes a vector of years it iterates over them to
#'create filenames to read in and return with only the MONTH and year columns selected
#'
#' The function will create a filename depending on the year input, if the file does not exist an error will be thrown.
#' If it does exist, it will attempt to read them in, mutate a new column with the year and then select
#' the columns MONTH and year.
#'
#' @param years A vector of years to read in
#'
#' @importFrom dplyr mutate select
#'
#' @note this function depends on dplyr mutate and select functions
#'
#' @return returns a list of dataframes with columns MONTH and year
#'
#' @examples
#' fars_read_years(c(2013, 2014))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarse data by year
#'
#' \code{fars_summarize_years} takes a list of years and reads them in using the \code{fars_read_years}
#' it then binds these dataframes together and summarises the data.
#'
#' The function will take in a vector of years and read in the data using the fars_summarize_years function,
#' it then binds these rows together and groups by the year and MONTH column creating a count column: n.
#' The data is then converted from a long format to a wide format using the spread function in tidyr.
#'
#' @param years The years to read in and summarise
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @note This functions depends on the bind_rows, group_by, summarize functions from dplyr and spread from tidyr
#'
#' @return a data.frame of summarised data which is converted to a wide format
#'
#' @examples
#' fars_summarize_years(c(2013, 2014))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots points where accidents for given state.num
#'
#' \code{fars_map_state} plots the state given as an integer
#' argument and plots accidents for a given year
#'
#' The function will take a year and state and attempt to plot accidents for a given state number.
#' If there is no data for a state, the function will stop and throw an error. If there are no accidents
#' a message will be returning indicating this.
#'
#' @param state.num state number to plot accidents for
#' @param year year of data to plot accidents that occured
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @note This function depends on map and points functions from the maps and graphics package respectively.
#'
#' @return a data.frame of filtered data
#'
#' @examples
#' far_map_state(1, 2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
