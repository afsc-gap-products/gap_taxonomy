#  check to make sure WORMS database running
if(db == "worms") {
url <- "https://www.marinespecies.org/"
}
if(db == "itis"){
  url <- "https://www.itis.gov/"
}

tt <- getOption("timeout")
w <- getOption("warn")
options(timeout = 10, warn = -1)

con <- try(readLines(con = url), silent = TRUE)

options(timeout = tt, warn = w)


# remove unnecessary objects after code run
rm(url, tt, w)


if(class(con) == "try-error") {
  cat(paste(db, "database down currently. Recommend running code at a different time."))

}

