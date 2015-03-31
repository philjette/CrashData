#' hadleyverse version of plane scraping by @hrbrmstr

library(dplyr)
library(rvest)
library(magrittr)
library(stringr)
library(pbapply)

crash_base <- "http://www.planecrashinfo.com/%d/%s.htm"

#' retrieve crash data for a given year
#' defaults to current year
#' earliest year in the database is 1920
get_data <- function(year=as.numeric(format(Sys.Date(), "%Y"))) {

  if (year < 1920 | year > as.numeric(format(Sys.Date(), "%Y"))) {
    stop("year must be >=1920 and <=current year", call.=FALSE)
  }

  # get crash date

  pg <- html(sprintf(crash_base, year, year))
  pg %>%
    html_nodes("table > tr > td:nth-child(1)") %>%
    html_text() %>%
    extract(-1) %>%
    as.Date(, format="%d %b %Y") %>%
    data_frame(date=.) -> date

  # get location and operator

  loc_op <- bind_rows(lapply(1:length(date), function(i) {

    pg %>%
      html_nodes(xpath=sprintf("//table/tr/td[2]/*/br[%d]/preceding-sibling::text()", i)) %>%
      html_text() %>%
      gsub("(^[[:space:]]*|[[:space:]]*$)", "", .) %>%
      gsub("^(Near|Off) ", "", .) -> loc

    pg %>%
      html_nodes(xpath=sprintf("//table/tr/td[2]/*/br[%d]/following-sibling::text()", i)) %>%
      html_text() %>%
      gsub("(^[[:space:]]*|[[:space:]]*$|\\n)", "", .) -> op

    data_frame(location=loc, operator=op)

  }))

  # get type & registration

  type_reg <- bind_rows(lapply(1:length(date), function(i) {

    pg %>%
      html_nodes(xpath=sprintf("//table/tr/td[3]/*/br[%d]/preceding-sibling::text()", i)) %>%
      html_text() %>%
      gsub("(^[[:space:]]*|[[:space:]]*$|\\n)", "", .) %>%
      ifelse(.=="?", NA, .) -> typ

    pg %>% html_nodes(xpath=sprintf("//table/tr/td[3]/*/br[%d]/following-sibling::text()", i)) %>%
      html_text() %>%
      gsub("(^[[:space:]]*|[[:space:]]*$|\\n)", "", .) %>%
      ifelse(.=="?", NA, .) -> reg

    data_frame(type=typ, registration=reg)

  }))

  # get fatalties

  pg %>% html_nodes("table > tr > td:nth-child(4)") %>%
    html_text() %>%
    str_match_all("([[:digit:]]+)/([[:digit:]]+)\\(([[:digit:]]+)\\)") %>%
    lapply(function(x) {
      data_frame(aboard=as.numeric(x[2]), fatalties=as.numeric(x[3]), ground=as.numeric(x[4]))
    }) %>%
    bind_rows %>% tail(-1) -> afg

  bind_cols(date, loc_op, type_reg, afg)

}

# get a bunch of them
# use pblapply to get a free progress bar
crashes <- bind_rows(pblapply(1950:2015, get_data))

# save them out
write.csv(crashes, "crashes.csv", row.names=FALSE)




