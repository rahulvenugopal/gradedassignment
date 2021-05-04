## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(gradedassignment)

## ----2015_data----------------------------------------------------------------
filename <- make_filename('2015')
data_2015 <- fars_read(filename) 
knitr::kable(head(data_2015))


## ----make_filename------------------------------------------------------------
filename <- make_filename(2015)
filename

## ----fars_read----------------------------------------------------------------
data <- fars_read(make_filename(2015) )
knitr::kable(head(data))

## ----fars_read_years----------------------------------------------------------
data <- fars_read_years(2015)
data

## ----fars_summarize_years-----------------------------------------------------
summary <- fars_summarize_years(2013:2015)
summary

## ----fars_summarize_years single----------------------------------------------
summary <- fars_summarize_years(2013)
summary

## ----fars_map_state-----------------------------------------------------------
fars_map_state(49,2015)

## ----fars_map_state another---------------------------------------------------
fars_map_state(4,2014)

