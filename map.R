rm(list = ls()); gc()
t0 <- proc.time()
library(data.table)
###############################################################################
#                                                                             #
#                 D I R                                                       #
#                                                                             #
###############################################################################
datadir <- "//40278DFWYM13/Na/test/"
dir.create(datadir, recursive = T ,showWarnings = F)
#source("//40278DFWYM13/Na/r_fn.R")

###############################################################################
#                                                                             #
#           R E A D    &  A D J                                               #
#                                                                             #
###############################################################################
dt<-fread(paste0("//corpaa.aa.com/campusshared/HDQ/HDQ_REVMGMT_Share/RMDEPT/NDeng/dp/MF_OUTPUT 21APR17.csv"), header=TRUE)
dt[, c("FCST_DOW", "FCST_ID", "RES_HLD", "PRICE_POINT_IND", "TD_NOW", "ID_NOW", "MWP_NOW_FCST_ERR", "TOT_REVENUE_NOW", "CHILD_FARE") := NULL]
setnames(dt, colnames(dt), c("lo", "ld", "d", "fid", "lfi", "au", "ac", "cls", "ach", "att", "f"))

dt<- dt[cls<=10]
dt[, f:=ifelse(f<0, -100, f)]

#Indexing
dt[, idx := .GRP, by = list(lo, ld, d, fid)]
setkey(dt, idx, lfi)
setorder(dt, lfi, cls)

#Fare
dt[, rev := f * ach]
dt[, tot_rev := cumsum(rev), by = .(idx, lfi)]
dt[, cd_ach := cumsum(ach), by = .(idx, lfi)]
dt[, avg_f := tot_rev/cd_ach]
dt[is.infinite(avg_f) | is.nan(avg_f), avg_f := f]

#SUR
dt[, sell := cd_ach/max(cd_ach), by = .(idx, lfi)]
dt[is.infinite(sell) | is.nan(avg_f), sell := 0]

#rounding
dt[, avg_f := round(avg_f, 5)]
dt[, sell := round(sell, 5)]

###############################################################################
#                                                                             #
#           M A P                                                             #
#                                                                             #
###############################################################################

map <- function(dt, node_){
saveRDS(dt, paste0(datadir, "dt_", node_, ".rds") )
return(node_)
}
dt[, list(map(.SD, node)), by = node]

system("//40278DFWYM13/Na/mapper.bat")
