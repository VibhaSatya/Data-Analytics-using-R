ret <- subset(data,batsman == "AB de Villiers")
tot <- cbind(ret["total_runs"])
colSums(tot)
mean(as.numeric(unlist(tot)))

median(as.numeric(unlist(tot)))

getmode <- function(tot) {
   uniqv <- unique(tot)
   uniqv[which.max(tabulate(match(tot, uniqv)))]
}
mode <- getmode(as.numeric(unlist(tot)))
mode

q <- quantile(as.numeric(unlist(tot)))
q

iqr <- IQR(as.numeric(unlist(tot)))
iqr

sd <- sd(as.numeric(unlist(tot)))
sd

####################
ret <- subset(data,batsman == "MS Dhoni")
tot <- cbind(ret["total_runs"])
colSums(tot)
mean(as.numeric(unlist(tot)))

median(as.numeric(unlist(tot)))

getmode <- function(tot) {
   uniqv <- unique(tot)
   uniqv[which.max(tabulate(match(tot, uniqv)))]
}
mode <- getmode(as.numeric(unlist(tot)))
mode

q <- quantile(as.numeric(unlist(tot)))
q

iqr <- IQR(as.numeric(unlist(tot)))
iqr
sd <- sd(as.numeric(unlist(tot)))
sd

######################

ret <- subset(data,batsman == "RA Jadeja")
tot <- cbind(ret["total_runs"])
colSums(tot)
mean(as.numeric(unlist(tot)))

median(as.numeric(unlist(tot)))

getmode <- function(tot) {
   uniqv <- unique(tot)
   uniqv[which.max(tabulate(match(tot, uniqv)))]
}
mode <- getmode(as.numeric(unlist(tot)))
mode

q <- quantile(as.numeric(unlist(tot)))
q

iqr <- IQR(as.numeric(unlist(tot)))
iqr
sd <- sd(as.numeric(unlist(tot)))
sd
########################

ret <- subset(data,batsman == "SK Raina")
tot <- cbind(ret["total_runs"])
colSums(tot)
mean(as.numeric(unlist(tot)))

median(as.numeric(unlist(tot)))

getmode <- function(tot) {
   uniqv <- unique(tot)
   uniqv[which.max(tabulate(match(tot, uniqv)))]
}
mode <- getmode(as.numeric(unlist(tot)))
mode

q <- quantile(as.numeric(unlist(tot)))
q

iqr <- IQR(as.numeric(unlist(tot)))
iqr
sd <- sd(as.numeric(unlist(tot)))
sd

########################

ret <- subset(data,batsman == "V Kohli")
tot <- cbind(ret["total_runs"])
colSums(tot)
mean(as.numeric(unlist(tot)))

median(as.numeric(unlist(tot)))

getmode <- function(tot) {
   uniqv <- unique(tot)
   uniqv[which.max(tabulate(match(tot, uniqv)))]
}
mode <- getmode(as.numeric(unlist(tot)))
mode

q <- quantile(as.numeric(unlist(tot)))
q

iqr <- IQR(as.numeric(unlist(tot)))
iqr
sd <- sd(as.numeric(unlist(tot)))
sd

