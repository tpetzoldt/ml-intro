## original data. spline interpolated but not scaled

rescale <- function(x, ymin=0, ymax=1) {
  x <- (x - min(x)) / (ymax - ymin)
}


dat <- read.csv("spl_cqc.csv")
dat$DATE <-  as.Date(as.character(dat$DATUM), format="%d.%m.%y")
summary(dat)

select <- c("DATE", "XBE", "TIEFE", "ZMIX", "ST", "TE", "TH",
            "PO4_PE", "PO4_PH", "NO3E", "NO3H",
            "SIE", "SIH", "O2_SATE", "O2_SATH")
dat <- dat[select]
names(dat)[3] <- "DEPTH"

## optional: rescale to [0, 1]
# nm <- names(dat)
# nm <- nm[3:length(nm)]
# dat2 <- dat
# for (n in nm) {
#  dat2[nm] <- rescale(dat[nm])
#}


write.csv(dat, file="phytoplankton-cqc.csv", quote=FALSE, row.names = FALSE)
