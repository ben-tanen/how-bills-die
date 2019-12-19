
################################
### LOAD PACAKGES, SET PATHS ###
################################

rm(list = ls())

library(data.table)
library(tidyr)

setwd("/Users/ben-tanen/Desktop/Projects/how-bills-die")

################################
### LOAD AND FORMAT RAW DATA ###
################################

actions <- data.table(read.csv("data/2019-10-09_s116_actions_1-2594.csv"))

# remove "X_" added to column headers
names(actions) <- gsub("X_", "", names(actions))

# clean up date and text variables
actions$date <- as.Date(actions$date, tz = "America/New_York")
actions$action_text <- toupper(actions$action_text)
actions$type <- toupper(actions$type)

# format bill id for consistency
actions$bill <- sprintf("s%04d-116", as.integer(gsub("s", "", gsub("-116", "", actions$bill))))

#########################################
### PRELIMINARY STATUS IDENTIFICATION ###
#########################################

# identify 1. introduction
actions[grepl("(INTRODUCED IN THE SENATE|READ [A-Z]+ AND REFERRED|ORIGINAL MEASURE)", action_text), status := "A. INTRODUCTION IN SENATE"]

# identify 2. senate committee consideration
actions[is.na(status) & (type %in% c("COMMITTEE", "DISCHARGE") | grepl("BY SENATOR [A-Z]+ FROM (SELECT )?COMMITTEE", action_text)), status := "B. SENATE COMMITTEE CONSIDERATION"]

# identify 3. on senate calendar
actions[is.na(status) & grepl("(PLACED ON SENATE LEGISLATIVE CALENDAR)", action_text), status := "C. ON SENATE CALENDAR"]

# identify 4. senate debate
actions[is.na(status) & grepl("(CONSIDERED BY SENATE|CLOTURE (ON THE )?MOTION|MOTION TO PROCEED TO (MEASURE CONSIDER|CONSIDERATION))", action_text), status := "D. SENATE DEBATE"]

# identify 5. passed/failed senate vote
actions[is.na(status) & grepl("(PASSED SENATE)", action_text), status := "E. PASSED SENATE VOTE"]

# actions[is.na(status) & ..., status := "E. FAILED SENATE VOTE"] # none of these?

# identify 6. referred to house
actions[is.na(status) & grepl("(RECEIVED IN THE HOUSE|MESSAGE ON SENATE ACTION)", action_text), status := "F. REFERRED TO HOUSE"]

# no manual identification of 7. house committee consideration

# identify 8. on house calendar
actions[is.na(status) & grepl("PLACED ON THE ([A-Z]+ )?CALENDAR", action_text), status := "H. ON HOUSE CALENDAR"]

# identify 9. house debate
actions[is.na(status) & grepl("(DEBATE)", action_text), status := "I. HOUSE DEBATE"]

# identify 10. passed/failed house vote
actions[is.na(status) & 
            grepl("(ON PASSAGE|ON MOTION TO SUSPEND THE RULES AND PASS THE BILL)", action_text) & 
            grepl("(PASSED|AGREED TO)", action_text), status := "J. PASSED HOUSE VOTE"]

actions[is.na(status) &
            grepl("(ON PASSAGE|ON MOTION TO SUSPEND THE RULES AND PASS THE BILL)", action_text) &
            grepl("(FAILED)", action_text), status := "J. FAILED HOUSE VOTE"]

# identify 11. resolving differences
actions[is.na(status) & type == "RESOLVINGDIFFERENCES", status := "K. RESOLVING DIFFERENCES"]

# identify 12. given to president
actions[is.na(status) & grepl("(PRESENTED TO PRESIDENT|BECAME PUBLIC LAW)", action_text), status := "L. TO PRESIDENT"]

#############################
### MANUAL STATUS UPDATES ###
#############################

actions[bill == "s0094-116" & is.na(status) & date == as.Date("2019-04-30"), status := "C. ON SENATE CALENDAR"]
actions[bill == "s0483-116" & (is.na(status) | status == "F. REFERRED TO HOUSE") & date %in% as.Date(c("2019-02-26", "2019-03-04")), status := "K. RESOLVING DIFFERENCES"]
actions[bill == "s1790-116" & status == "F. REFERRED TO HOUSE" & date == as.Date("2019-09-19"), status := "K. RESOLVING DIFFERENCES"]
actions[bill == "s1790-116" & status == "I. HOUSE DEBATE" & date == as.Date("2019-09-17"), status := "K. RESOLVING DIFFERENCES"]

##############################
### SECONARY STATUS TWEAKS ###
##############################

# make table by date / action type
days <- actions[, .(n = .N), by = c("bill", "date", "status")]
days[, n_statuses := .N, by = c("bill", "date")]

# check if any dates have no statuses assigned to them
# if yes, include in above identification/updates
# if no, remove NAs
stopifnot(nrow(days[is.na(status) & n_statuses == 1]) == 0)
days <- days[!is.na(status)]

# make table of status type showing the range of dates associated
days.range <- days[, .(min_date = min(date, na.rm = T), max_date = max(date, na.rm = T)), by = c("bill", "status")]

# need to reassign B. SENATE COMMITTEE CONSIDERATION to G. HOUSE COMMITTEE CONSIDERATION if it occurs after F. REFERRED TO HOUSE
# check whether to use min or max date (or if it matters), if nrow = 0, then doesn't matter
days2 <- merge(days, days.range[status == "F. REFERRED TO HOUSE"], by = "bill", all.x = T)
# stopifnot(nrow(days.range[status == "F. REFERRED TO HOUSE" & min_date != max_date]) == 0)
days2[status.x == "B. SENATE COMMITTEE CONSIDERATION" & date > max_date, status.x := "G. HOUSE COMMITTEE CONSIDERATION"]

# need to reassign I. HOUSE DEBATE or D. HOUSE DEBATE to K. RESOLVING DIFFERENCES if it occurs after J. PASSED HOUSE VOTE
# check whether to use min or max date (or if it matters), if nrow = 0, then doesn't matter
days2 <- merge(days2[, c("bill", "date", "status.x")], days.range[status == "J. PASSED HOUSE VOTE"], by = "bill", all.x = T)
stopifnot(nrow(days.range[status == "J. PASSED HOUSE VOTE" & min_date != max_date]) == 0)
days2[status.x %in% c("D. SENATE DEBATE", "I. HOUSE DEBATE") & date > min_date, status.x := "K. RESOLVING DIFFERENCES"]

days2 <- unique(days2[, c("bill", "date", "status.x")])[order(bill, date, status.x)]
names(days2) <- c("bill", "date", "status")

# confirm switches were implemented correctly (date and status are in same order, so order1 = order2 = order3)
days2.range <- days2[, .(n = .N, min_date = min(date, na.rm = T), max_date = max(date, na.rm = T)), by = c("bill", "status")]

days2.range <- days2.range[order(bill, min_date, status)]
days2.range$order1 <- 1:nrow(days2.range)
days2.range <- days2.range[order(bill, status, min_date)]
days2.range$order2 <- 1:nrow(days2.range)
days2.range <- days2.range[order(bill, max_date, status)]
days2.range$order3 <- 1:nrow(days2.range)

# see which bills are in weird order; some will be OKAY (see below)
# s94-116    - further committee consideration - OKAY
# s163-116   - further committee consideration - OKAY
# s268-116   - further committee consideration - OKAY
# s310-116   - further committee consideration - OKAY
# s349-116   - further committee consideration - OKAY
# s383-116   - further committee consideration - OKAY
# s747-116   - further committee consideration - OKAY
# s862-116   - further committee consideration - OKAY
# s1014-116  - further committee consideration - OKAY
# s1061-116  - further committee consideration - OKAY
# s1345-116  - further committee consideration - OKAY
# s1589-116  - further committee consideration - OKAY
# s1689-116  - further committee consideration - OKAY
days2.range[(order1 != order2 | order1 != order3 | order2 != order3) & 
            !(bill %in% paste0("s", c("0094", "0163", "0268", "0310", "0349", "0383", "0747", "0862", "1014", "1061", "1345", "1589", "1689"), "-116"))]

########################
### CHECKS ON STATUS ###
########################

# merge on date's status to compare against individual actions on the date
days2[, status.n := substr(status, 1, 1)]
actions2 <- merge(actions, spread(days2[, c("bill", "date", "status", "status.n")], key = status, value = status.n), by = c("bill", "date"))

###############################
### REFORMAT FOR OUTPUT/VIZ ###
###############################

# lag status to show previous status
days3 <- days2[, c("bill", "date", "status")][order(bill, date, status)]
days3[, status.prev := c(NA, status[-.N]), by = bill]

# remove rows where status = status.prev (no change)
days3 <- days3[status != status.prev | is.na(status.prev)]

# export
write.csv(days3, paste0("data/", Sys.Date(), "_s116_status-days_", substr(min(days3$bill), 2, 5), "-", substr(max(days3$bill), 2, 5), ".csv"), row.names = F)
