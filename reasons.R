rm(list=ls())
library(readxl)

#### Data Screening ####
dat <- read_excel("4 Screen.xlsx")
table(dat$Exclude, useNA = "always")  #Exclusion reasons
sum(dat$N[which(dat$Exclude=="D")])  #Number of participants excluded for no relative frequency

#### Data Cleaning ####
dat <- read_excel("5 Extract.xlsx")
sum(dat$n[!duplicated(dat$stub)])  #Total number of respondents
tapply(dat$domain, dat$stub, function(v) length(unique(v)))  #Domains per sample
dat <- dat[which(dat$reason!="Pressure from family, friends, or society to not have children"),]  #Lowest rate item across all studies, doesn't fit in any domain

#### Harmonize values by percentile within study ####
dat$ptile <- ave(dat$value, dat$stub, FUN = function(x) {
  sapply(x, function(v) mean(x <= v) * 100)
})

#### Summarize, weighted by sample size ####
domains <- unique(dat$domain)
results <- data.frame(domain = character(), 
                      example = character(),
                      reasons = numeric(),
                      studies = numeric(),
                      wmean = numeric(),
                      wsd = numeric(),
                      mean = numeric(),
                      sd = numeric())
for (d in domains) {
  reasons <- nrow(dat[which(dat$domain==d),])
  studies <- length(unique(dat$stub[which(dat$domain==d)]))
  wm <- weighted.mean(dat$ptile[which(dat$domain==d)], dat$n[which(dat$domain==d)])
  wsd <- sqrt(sum(dat$n[which(dat$domain==d)] * (dat$ptile[which(dat$domain==d)] - wm)^2) / (sum(dat$n[which(dat$domain==d)]) - 1))
  results <- rbind(results, data.frame(domain = d, 
                                       example = dat$example[which(dat$domain==d)][1],
                                       reasons = reasons,
                                       studies = studies,
                                       wmean = wm,
                                       wsd = wsd,
                                       mean = mean(dat$ptile[which(dat$domain==d)]),
                                       sd = sd(dat$ptile[which(dat$domain==d)])))
}
rm(d, domains, reasons, studies, wm, wsd)
results[,c(5:8)] <- round(results[,c(5:8)],2)
results <- results[order(results$wmean, decreasing = TRUE), ]
write.csv(results, "table_results.csv")

#### Reasons Appendix ####
reasons <- dat[,c("domain", "reason", "stub")]
reasons$stub[which(reasons$stub=="minkin_2024a")] <- "minkin_2024"
reasons$stub[which(reasons$stub=="minkin_2024b")] <- "minkin_2024"
reasons <- reasons[order(reasons$domain, reasons$stub), ]
reasons$domain[duplicated(reasons$domain)] <- ""
reasons$reason <- paste0(reasons$reason, " \\citep{", reasons$stub, "}")
reasons$stub <- NULL
colnames(reasons) <- c("Domain", "Item (Source)")
write.csv(reasons, "table_reasons.csv")


