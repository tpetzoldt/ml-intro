## data already scaled to [0, 1]
dat <- read.csv("NEUTEST.TXT")
dat$DATE <-  as.Date(as.character(dat$DATUM), format="%Y%m%d")
summary(dat)


#y <- dat["XBE"]
#y <- sqrt(dat["XBE"])
y <- dat$XBE^0.5
y <- y / max(y)

dat$XBE <- y 

select <- c("DATE", "XBE", "TIEFE", "ZMIX", "ST", "TE", "TH", "PHE", "PHH",
            "PO4_PE", "PO4_PH", "NO3E", "NO3H",
            "SIE", "SIH", "O2_SATE", "O2_SATH")
dat <- dat[select]
names(dat)[3] <- "DEPTH"

write.csv(dat, file="phytoplankton.csv", quote=FALSE, row.names = FALSE)
