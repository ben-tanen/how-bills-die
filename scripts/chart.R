
################################
### LOAD PACAKGES, SET PATHS ###
################################

rm(list = ls())

library(data.table)
library(ggplot2)
library(manipulate)
library(tidyr)

setwd("/Users/ben-tanen/Desktop/Projects/how-bills-die")

################################
### LOAD AND FORMAT RAW DATA ###
################################

bills.hr <- data.table(read.csv("data/2019-10-12_hr116_status-days_0001-4630.csv"))
bills.s  <- data.table(read.csv("data/2019-10-12_s116_status-days_0001-2594.csv"))

# add chambers (root and current)
bills.hr[, chamber.root := "hr"]
bills.hr[substr(status, 1, 1) %in% LETTERS[1:5], chamber.curr := "hr"]
bills.hr[substr(status, 1, 1) %in% LETTERS[6:10], chamber.curr := "s"]
bills.hr[substr(status, 1, 1) %in% c("K", "L"), chamber.curr := "b"]
bills.s[, chamber.root := "s"]
bills.s[substr(status, 1, 1) %in% LETTERS[1:5], chamber.curr := "s"]
bills.s[substr(status, 1, 1) %in% LETTERS[6:10], chamber.curr := "hr"]
bills.s[substr(status, 1, 1) %in% c("K", "L"), chamber.curr := "b"]

# stack for single dataset
bills <- rbind(bills.hr, bills.s)

# reformat date
bills$date <- as.Date(bills$date, tz = "America/New_York")

##################################
### CALCULATE CURRENT STATUSES ###
##################################

# clean status to ignore root chamber 
bills[, status.clean := gsub("[A-Z][.] ", "", status)]

# add bucket status to compare INTRODUCTION IN HOUSE to INTRODUCTION IN SENATE, etc.
bills[substr(status, 1, 1) %in% c("A", "F"), status.bucket := "A. INTRODUCTION"]
bills[substr(status, 1, 1) %in% c("B", "G"), status.bucket := "B. COMMITTEE"]
bills[substr(status, 1, 1) %in% c("C", "H"), status.bucket := "C. CALENDAR"]
bills[substr(status, 1, 1) %in% c("D", "I"), status.bucket := "D. DEBATE"]
bills[substr(status, 1, 1) %in% c("E", "J"), status.bucket := "E. VOTE"]
bills[status == "K. RESOLVING DIFFERENCES", status.bucket := "F. RESOLVING DIFF"]
bills[status == "L. TO PRESIDENT", status.bucket := "G. TO PRESIDENT"]

# take the last status of each bill
bills[, chamber.num := ifelse(chamber.root == chamber.curr, 1, 2)]
bills[chamber.curr == "b", chamber.num := 3]
bills <- bills[order(bill, date, chamber.num, status.bucket)]
bills$id <- 1:nrow(bills)
bills[, max_id := max(id), by = bill]
bills.latest <- bills[id == max_id, c("bill", "chamber.root", "chamber.curr", "chamber.num", "status.bucket")]

# for bills in their second chamber (already passed their root chamber), add duplicate row to show passed vote
bills.latest.add1 <- bills.latest[chamber.num == 2]
bills.latest.add1[, chamber.curr := chamber.root]
bills.latest.add1[, chamber.num := 1]
bills.latest.add1[, status.bucket := "E. VOTE"]

# for bills that passed both chambers, add duplciate rows to show both passed votes
bills.latest.add2 <- bills.latest[chamber.num == 3]
bills.latest.add2[, chamber.curr := chamber.root]
bills.latest.add2[, chamber.num := 1]
bills.latest.add2[, status.bucket := "E. VOTE"]

bills.latest.add3 <- copy(bills.latest.add2)
bills.latest.add3[, chamber.curr := ifelse(chamber.root == "hr", "s", "hr")]
bills.latest.add3[, chamber.num := 2]

bills.latest.all <- rbind(bills.latest, bills.latest.add1, bills.latest.add2, bills.latest.add3)[order(bill, chamber.num)]

# aggregate by chamber and status
status_agg <- merge(unique(bills[, c("chamber.curr", "status.bucket")]),
                    bills.latest.all[, .(N.curr = .N), by = c("chamber.curr", "status.bucket")],
                    by = c("chamber.curr", "status.bucket"), all = T)[order(chamber.curr, status.bucket)]

# sum up rows to get count of bills that have ever been at status
status_agg$N <- 0
for (i in 1:nrow(status_agg)) {
    status_agg[i]$N <- sum(status_agg[chamber.curr == status_agg[i]$chamber.curr & status.bucket >= status_agg[i]$status.bucket]$N.curr, na.rm = T)
}

# calculate share of bills in particular status (by house)
status_agg[, N.chamber := sum(N.curr, na.rm = T), by = chamber.curr]
status_agg[, share.chamber := N / N.chamber]
status_agg[, label := paste0(format(N, big.mark = ","), " (", scales::percent(share.chamber), ")")]

########################################
### GET CURRENT STATUS COUNTS BY DAY ###
########################################

# create function to do it by date
get_status_counts <- function(lim_date) {
    # limit bills based on input date and then get latest action for bill
    bills.lim <- bills[date <= as.Date(lim_date)]
    bills.lim[, max_id := max(id), by = bill]
    bills.lim.latest <- bills.lim[max_id == id, c("bill", "chamber.root", "chamber.curr", "chamber.num", "status.bucket")]
    
    # for bills in their second chamber (already passed their root chamber), add duplicate row to show passed vote
    bills.lim.latest.add1 <- bills.lim.latest[chamber.num == 2]
    bills.lim.latest.add1[, chamber.curr := chamber.root]
    bills.lim.latest.add1[, chamber.num := 1]
    bills.lim.latest.add1[, status.bucket := "E. VOTE"]
    
    # for bills that passed both chambers, add duplciate rows to show both passed votes
    bills.lim.latest.add2 <- bills.lim.latest[chamber.num == 3]
    bills.lim.latest.add2[, chamber.curr := chamber.root]
    bills.lim.latest.add2[, chamber.num := 1]
    bills.lim.latest.add2[, status.bucket := "E. VOTE"]
    
    bills.lim.latest.add3 <- copy(bills.lim.latest.add2)
    bills.lim.latest.add3[, chamber.curr := ifelse(chamber.root == "hr", "s", "hr")]
    bills.lim.latest.add3[, chamber.num := 2]
    
    bills.lim.latest.all <- rbind(bills.lim.latest, bills.lim.latest.add1, bills.lim.latest.add2, bills.lim.latest.add3)[order(bill, chamber.num)]
    
    # aggregate by chamber and status
    status_agg.lim <- merge(unique(bills[, c("chamber.curr", "status.bucket")]),
                            bills.lim.latest.all[, .(N.curr = .N), by = c("chamber.curr", "status.bucket")],
                            by = c("chamber.curr", "status.bucket"), all = T)[order(chamber.curr, status.bucket)]
    status_agg.lim[is.na(N.curr), N.curr := 0]
    
    # sum up rows to get count of bills that have ever been at status
    status_agg.lim$N <- 0
    for (i in 1:nrow(status_agg.lim)) {
        status_agg.lim[i]$N <- sum(status_agg.lim[chamber.curr == status_agg.lim[i]$chamber.curr & status.bucket >= status_agg.lim[i]$status.bucket]$N.curr, na.rm = T)
    }
    
    # calculate share of bills in particular status (by house)
    status_agg.lim[, N.chamber := sum(N.curr, na.rm = T), by = chamber.curr]
    status_agg.lim[, share.chamber := N / N.chamber]
    status_agg.lim[, label := paste0(format(N, big.mark = ","), " (", scales::percent(share.chamber), ")")]
    
    # add date field
    status_agg.lim$date <- lim_date
    
    return(data.table(status_agg.lim))
}
get_status_counts(max(bills$date))

# get dataset for all dates
status_agg.all <- rbindlist(lapply(seq(min(bills$date), max(bills$date), by = "day"), get_status_counts))

############
### PLOT ###
############

manipulate(
    ggplot(status_agg.all[date == date.slider], aes(x = status.bucket, y = ifelse(chamber.curr == "s", -1, 1) * N)) + 
        geom_bar(aes(fill = chamber.curr), stat = "identity") + 
        geom_text(aes(label = label, vjust = ifelse(chamber.curr == "s", 1.4, -0.4))) +
        scale_x_discrete(name = "Current status") +
        scale_y_continuous(name = "Number of bills") +
        # scale_fill_manual(name = NULL, labels = c("s" = "Senate", "hr" = "House", "b" = "Both Chambers"),
                            # values = c("s" = "#77bdee", "hr" = "#ff0000", "b" = "#00ff00")) +
        labs(title = as.Date(date.slider, origin = "1970-01-01")),
    date.slider = slider(as.numeric(min(bills$date)), as.numeric(max(bills$date))))

###########################
### REFORMAT FOR OUTPUT ###
###########################

# add key for spreading to wide
status_agg.all[, key := paste0(chamber.curr, "_", substr(status.bucket, 1, 1))]

# rename columns for reference in JS (replacing . with _)
names(status_agg.all) <- gsub("[.]", "_", names(status_agg.all))

write.csv(status_agg.all[, -c("share_chamber", "label", "key")], 
          paste0("data/", Sys.Date(), "_bill-counts-by-day-long.csv"), row.names = F)

write.csv(spread(status_agg.all[, c("date", "N", "key")], "key", "N"), 
          paste0("data/", Sys.Date(), "_bill-counts-by-day-wide.csv"), row.names = F)

