
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

actions.raw1 <- data.table(read.csv("data/2019-10-09_hr116_actions_1-199.csv"))
actions.raw2 <- data.table(read.csv("data/2019-10-09_hr116_actions_200-999.csv"))
actions.raw3 <- data.table(read.csv("data/2019-10-09_hr116_actions_1000-1999.csv"))
actions.raw4 <- data.table(read.csv("data/2019-10-09_hr116_actions_2000-4630.csv"))

actions <- rbind(actions.raw1, actions.raw2, actions.raw3, actions.raw4)

# remove "X_" added to column headers
names(actions) <- gsub("X_", "", names(actions))

# clean up date and text variables
actions$date <- as.Date(actions$date, tz = "America/New_York")
actions$action_text <- toupper(actions$action_text)
actions$type <- toupper(actions$type)

# format bill id for consistency
actions$bill <- sprintf("hr%04d-116", as.integer(gsub("hr", "", gsub("-116", "", actions$bill))))

#########################################
### PRELIMINARY STATUS IDENTIFICATION ###
#########################################

# identify 1. introduction
actions[grepl("^REFERRED TO THE (HOUSE )?COMMITTEE", action_text), status := "A. INTRODUCTION IN HOUSE"]

# identify 2. house committee consideration
actions[is.na(status) & type %in% c("COMMITTEE", "DISCHARGE") | grepl("MOTION TO PLACE BILL ON CONSENSUS CALENDAR", action_text), status := "B. HOUSE COMMITTEE CONSIDERATION"]

# identify 3. on house calendar
actions[is.na(status) & grepl("(PLACED ON THE [A-Z]+ CALENDAR|ASSIGNED TO THE CONSENSUS CALENDAR|RULES COMMITTEE RESOLUTION|RULE H. RES. [0-9]+ PASSED HOUSE)", action_text), status := "C. ON HOUSE CALENDAR"]

# identify 4. house debate
actions[is.na(status) & grepl("DEBATE", action_text), status := "D. HOUSE DEBATE"]

# identify 5. passed/failed house vote
actions[is.na(status) & 
        grepl("(ON PASSAGE|ON MOTION TO SUSPEND THE RULES AND PASS THE BILL)", action_text) & 
        grepl("(PASSED|AGREED TO)", action_text), status := "E. PASSED HOUSE VOTE"]

actions[is.na(status) &
        grepl("(ON PASSAGE|ON MOTION TO SUSPEND THE RULES AND PASS THE BILL)", action_text) &
        grepl("(FAILED)", action_text), status := "E. FAILED HOUSE VOTE"]

# identify 6. referred to senate
actions[is.na(status) & grepl("RECEIVED IN THE SENATE", action_text), status := "F. REFERRED TO SENATE"]

# no manual identification of 7. senate committee consideration

# identify 8. on senate calendar
actions[is.na(status) & grepl("PLACED ON SENATE ([A-Z]+ )?CALENDAR", action_text), status := "H. ON SENATE CALENDAR"]

# identify 9. senate debate
actions[is.na(status) & grepl("(CONSIDERED BY SENATE|CLOTURE (ON THE )?MOTION)", action_text), status := "I. SENATE DEBATE"]

# identify 10. passed/failed senate vote
actions[is.na(status) & grepl("PASSED SENATE", action_text), status := "J. PASSED SENATE VOTE"]

# identify 11. resolving differences
actions[is.na(status) & (type == "RESOLVINGDIFFERENCES" | grepl("MESSAGE ON SENATE ACTION SENT TO THE HOUSE", action_text)), status := "K. RESOLVING DIFFERENCES"]

# identify 12. given to president
actions[is.na(status) & grepl("(PRESENTED TO PRESIDENT|BECAME PUBLIC LAW)", action_text), status := "L. TO PRESIDENT"]

#############################
### MANUAL STATUS UPDATES ###
#############################

actions[bill == "hr0268-116" & is.na(status) & date == as.Date("2019-03-27"), status := "I. SENATE DEBATE"]
actions[bill == "hr0463-116" & is.na(status) & date == as.Date("2019-09-20"), status := "B. HOUSE COMMITTEE CONSIDERATION"]
actions[bill == "hr0464-116" & is.na(status) & date == as.Date("2019-09-24"), status := "B. HOUSE COMMITTEE CONSIDERATION"]
actions[bill == "hr0648-116" & is.na(status) & date == as.Date("2019-01-18"), status := "B. HOUSE COMMITTEE CONSIDERATION"]
actions[bill == "hr0965-116" & is.na(status) & date == as.Date("2019-05-16"), status := "C. ON HOUSE CALENDAR"]
actions[bill == "hr2157-116" & is.na(status) & date %in% as.Date(c("2019-05-24", "2019-05-28", "2019-05-30")), status := "K. RESOLVING DIFFERENCES"]
actions[bill == "hr2590-116" & is.na(status) & date == as.Date("2019-09-18"), status := "H. ON SENATE CALENDAR"]
actions[bill == "hr2665-116" & date == as.Date("2019-04-10"), status := "A. INTRODUCTION IN HOUSE"]
actions[bill == "hr3088-116" & is.na(status) & date == as.Date("2019-09-27"), status := "B. HOUSE COMMITTEE CONSIDERATION"]
actions[bill == "hr3401-116" & date == as.Date("2019-06-27") & status == "C. ON HOUSE CALENDAR", status := NA]
actions[bill == "hr3432-116" & date == as.Date("2019-06-16"), status := "A. INTRODUCTION IN HOUSE"]
actions[bill == "hr3630-116" & date == as.Date("2019-06-12"), status := "A. INTRODUCTION IN HOUSE"]
actions[bill == "hr3631-116" & date == as.Date("2019-06-20"), status := "A. INTRODUCTION IN HOUSE"]

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

# need to reassign B. HOUSE COMMITTEE CONSIDERATION to G. SENATE COMMITTEE CONSIDERATION if it occurs after F. REFERRED TO SENATE
# check whether to use min or max date (or if it matters), if nrow = 0, then doesn't matter
days2 <- merge(days, days.range[status == "F. REFERRED TO SENATE"], by = "bill", all.x = T)
stopifnot(nrow(days.range[status == "F. REFERRED TO SENATE" & min_date != max_date]) == 0)
days2[status.x == "B. HOUSE COMMITTEE CONSIDERATION" & date > min_date, status.x := "G. SENATE COMMITTEE CONSIDERATION"]

# need to reassign D. HOUSE DEBATE to K. RESOLVING DIFFERENCES if it occurs after J. PASSED SENATE VOTE
# check whether to use min or max date (or if it matters), if nrow = 0, then doesn't matter
days2 <- merge(days2[, c("bill", "date", "status.x")], days.range[status == "J. PASSED SENATE VOTE"], by = "bill", all.x = T)
stopifnot(nrow(days.range[status == "J. PASSED SENATE VOTE" & min_date != max_date]) == 0)
days2[status.x == "D. HOUSE DEBATE" & date > min_date, status.x := "K. RESOLVING DIFFERENCES"]

days2 <- unique(days2[, c("bill", "date", "status.x")])
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
# hr549-116  - failed then pass - OKAY
# hr1112-116 - referred back to committee - OKAY
# hr1947-116 - supplemental findings - OKAY
# hr2500-116 - back and forth consideration - OKAY
# hr2621-116 - referred back to committee - OKAY
days2.range[(order1 != order2 | order1 != order3 | order2 != order3) & !(bill %in% paste0("hr", c("0549", "1112", "1947", "2500", "2621"), "-116"))]

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
write.csv(days3, paste0("data/", Sys.Date(), "_hr116_status-days_", substr(min(days3$bill), 3, 6), "-", substr(max(days3$bill), 3, 6), ".csv"), row.names = F)
