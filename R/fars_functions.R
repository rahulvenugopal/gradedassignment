#' fars_read
#'
#' Second function to be called in fars_map_state function.
#' Loads the data as a dataframe from current working directory
#' If file doesn't exist in current directory, this throws a message "file <filename> does not exist"
#'
#' @param filename Actual filename with extension as a character string
#'
#' @return A dataframe containing the full data
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom  dplyr as_tibble
#'
#' @note Warning is given when file is not available in current working directory
#' @note The filename should be a character string
#'
#' @examples data <- fars_read("accident_2013.csv.bz2")
fars_read <- function(filename) {
  data <- suppressMessages({
    readr::read_csv(system.file("extdata",filename, package = "gradedassignment"))
  })
  dplyr::as_tibble(data)
}

#' Make filename
#'
#' This is the first function to be called in fars_map_state function.
#' The function accepts year as input and returns a filename with bzip2 extension
#'
#' @param year 4 digit year as character or number string
#'
#' @return A character string which is the filename
#' @export
#'
#' @note Any character input will throw an error since input is a number
#' @note The function will throw a spanner later if we input say 20211 instead of 2021
#'
#' @examples filename <- make_filename(2013)
#' filename <- make_filename("2015")

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This function is used by fars_summarize_years for count summary on monthly basis
#'
#' @param years A character vector of years for which we want to subset data
#'
#' @return A list of dataframes where each list item has a dataframe for one year
#' The dataframe has two columns, MONTH and year
#'
#' @export
#'
#' @note Any year outside the ones in datasets will throw an invalid year messsage
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples fars_read_years(years = c(2013, 2014))
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

#' fars_summarize_years
#'
#' Generates a count summary of fatality numbers per month for years specified as output
#'
#' @param years A character vector of years for which we want count summary of fatality
#'
#' @return A summary dataframe with counts of fatality per month for years specified
#' @export
#'
#' @import magrittr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @note Error can happen when the year mentioned is not available in dataset
#'
#' @examples fars_summarize_years(2013:2015)
#' fars_summarize_years(c(2013,2014))
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}


#' fars_map_state
#'
#' Plots the map of the state based on state num and year
#' Each accident is a data point which is plotted at its geospatial coordinates
#'
#' @param state.num The digit code assigned to each state
#' @param year Year for which data to be fetched as number or character
#'
#' @return A map plot with each dot representing the geospatial location where
#' accident happned within each state in that year.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @note If state number is invalid i.e when the state code is not there in data file
#' @note When there are no accidents to plot, it displays "no accidents to plot"
#'
#' @examples fars_map_state(49,2013)
#' fars_map_state("1","2015")
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
