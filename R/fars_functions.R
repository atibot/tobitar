#' Read Fatality Analysis Reporting System data by the file name
#'
#' This function reads a csv file of Fatality Analysis Reporting
#' System data (using the \code{filename} argument) and returns
#' a data.frame of the data.
#'
#' @param filename A character string of the name of a file to read.
#'
#' @return a data.frame of the data that is read in.
#'
#' @note Error occurs if the spcified file does not exist.
#'
#' @importFrom read_csv from readr, tbl_df from Dplyr
#'
#' @examples
#' librray(readr)
#' library(dplyr)
#' data <- fars_read("accident_2013.csv.bz2")
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

#' Make file name from year
#'
#' This function creates a file name of the specified year's
#' Read Fatality Analysis Reporting System data.
#'
#' @param year A numeric number repsresenting the year (e.g. 2013)
#'
#' @return A character string of the file name in the form of
#' "accident_yyyy.csv.bz2"
#'
#' @note Validity of the year is not checked
#'
#' @importFrom None
#'
#' @examples
#' file <- make_filename(2013)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read Fatality Analysis Reporting System data for the years
#'
#' This function reads csv files of Fatality Analysis Reporting
#' System data for the years (using the \code{years} argument)
#' and returns a list of data.frames corresponding to the years.
#'
#' @param years A vector of years
#'
#' @return A list of data.frames corresponding to the years
#'
#' @note Any invalid year in the vector of years results in Null
#' item of the returned list with warning messages "invalid year: yyyy"
#'
#' @importFrom mutate and select from dplyr
#'
#' @import tidyr
#'
#' @examples
#'  dat_list <- fars_read_years(c(2013, 2014, 2015))
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

#' Summarize Fatality Analysis Reporting System data for the years
#'
#' This function summarizes Fatality Analysis Reporting System
#' data for the years (using the \code{years} argument) and returns
#' a data.frame of the number of fatality cases (accidents) per each month
#' for the years specified.
#'
#' @param years A vector of years
#'
#' @return A data.frame of the number of fatality cases per
#' each month (rows) for the years(columns)
#'
#' @note Any invalid year in the vector of years is ignored for
#' summary, with warning messages "invalid year: yyyy"
#'
#' @import readr, dplyr, tidyr
#'
#' @examples
#' head(fars_summarize_years(c(2013,2014,2015)))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot fatality case points
#'
#' This function plots fatality case (accident) points for the state
#' (using \code{state.num}) and the year (using \code{year}).
#'
#' @param state.num A numeric number identifying the state
#'
#' @param year A numeric number repsresenting the year (e.g. 2013)
#'
#' @return A map of the state and plots of fatality case points
#'
#' @note  An error occurs if the state number is invalid.
#' Nothing is plotted if no fatality cases exist.
#'
#' @importFrom filter from dplyr, map from maps, points from graphics
#'
#' @examples
#' fars_map_state(1, 2013)
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
