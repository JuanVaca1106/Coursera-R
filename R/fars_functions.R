#' Read csv file
#'
#' This is a simple function that will attempt to file into tbl df, if the file does not exist will return an error.
#'
#' @param filename a character vector of filename to read
#'
#' @return this function returns a dataframe of the data
#'
#' @examples
#' fars_read(2013)
#'
#' @import readr read_csv
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
#' This will create a character vector filename based on the year input
#'
#' @param year The year to be in the name of the file
#'
#' @return A character vector filename
#'
#' @examples
#' make_filename(2013)
#'
#' @import readr read_csv
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read in multiple files by year
#'
#' This function will apply over a list of years and read in the data, if year is invalid error will trigger
#'
#' @param year The year to be in the name of the file
#'
#' @return year
#'
#' @examples
#' fars_read_years(c(2013, 2014))
#'
#' @import dplyr mutate
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
#' THis function will summarise a list of year data
#'
#' @param years The years to summarise by
#'
#' @return a data.frame of summarised data
#'
#' @examples
#' fars_summarize_years(c(2013, 2014))
#'
#' @import dplyr mutate
#' @import tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Filter data by state number, if state number is invalid error will trigger
#'
#' THis function will summarise a list of year data
#'
#' @param state.num state numbers to filter
#' @param year year of data to filter by
#'
#' @return a data.frame of filtered data
#'
#' @examples
#' far_map_state()
#'
#' @import dplyr mutate
#' @import maps map
#' @import graphics points
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
