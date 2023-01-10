#  check to make sure WORMS database running

wrms_url <- "https://www.marinespecies.org/"

tt <- getOption("timeout")
w <- getOption("warn")
options(timeout = 10, warn = -1)

wrms_con <- try(readLines(con = wrms_url), silent = TRUE)

options(timeout = tt, warn = w)


# remove unnecessary objects after code run
rm(wrms_url, tt, w)


if(class(wrms_con) == "try-error") {
  cat("WORMS database down currently. Recommend running code at a different time. Switching database to itis for faster code.")
  
  db <- "itis"
}

