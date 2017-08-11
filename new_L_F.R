#source("//corpaa.aa.com/campusshared/HDQ/HDQ_REVMGMT_Share/RMDEPT/R/projects/F/Func.R")
#source("//corpaa.aa.com/campusshared/HDQ/ORDS01_Share/Shared/R/F/func.R")


print("Process Started!" )

library(data.table)
library(Rcpp)


###############################################################################
#                                                                             #
#                 D I R                                                       #
#                                                                             #
###############################################################################
args <- commandArgs(trailingOnly = TRUE)
fr=args[1]
to=args[2]
#fr=ifelse(is.na(fr),1,fr)
#to=ifelse(is.na(to),5,to)


print(paste0( "Calculate Idx from ", fr, " to ", to, "...") )
print("Please Do not Close the window!")

datadir <- "//40278DFWYM13/Na/data/"
logdir <-  "//40278DFWYM13/Na/log/"
dir.create(logdir, recursive = T ,showWarnings = F)
dir.create(datadir, recursive = T ,showWarnings = F)


logfile <- paste0(logdir,fr,"-",to,".log")
log <- file(logfile, open = "at")
sink(log, type = "message")
message("Calculate Idx from ", fr, " to ", to, "..." )



##inputs data#######

####read the data###


temp<-fread(paste0("//corpaa.aa.com/campusshared/HDQ/HDQ_REVMGMT_Share/RMDEPT/NDeng/dp/MF_OUTPUT 21APR17.csv"), header=TRUE)



#temp <- temp[,c("LEG_ORIG", "LEG_DEST", "FLT_DPTR_DT", "FLT_ID", "LCL_FLW_IND", 
 #               "yd_cls", "ACH_DMD", "ATTAIN_DMD", "FARE_F", "TD_NOW", "ID_NOW", "AU"), with = F]

temp<-temp[yd_cls<=10]

temp[, FARE_F:=ifelse(FARE_F<0, -100, FARE_F)]



#temp<-temp[!is.na(FARE_F)]


temp[, rev:= FARE_F*ACH_DMD]


temp[, tot_rev:= cumsum(rev), by=list(LEG_ORIG, LEG_DEST, FLT_DPTR_DT, FLT_ID, LCL_FLW_IND)]


temp[, cd_achv:= cumsum(ACH_DMD), by=list(LEG_ORIG, LEG_DEST, FLT_DPTR_DT, FLT_ID, LCL_FLW_IND)]



temp[, cd_att:= cumsum(ATTAIN_DMD), by=list(LEG_ORIG, LEG_DEST, FLT_DPTR_DT, FLT_ID, LCL_FLW_IND)]


temp[, avg_fare:=ifelse(yd_cls==1, FARE_F, (ifelse(cd_achv==0, FARE_F, tot_rev/cd_achv)))]


#setnames(temp, colnames(temp), c("LEG_ORIG","LEG_DEST", "FLT_DPTR_DT", "FLT_ID","LCL_FLW" , "YD_CLS" , 
 #                                "MWP","ERR","FARE","TD","ID", "AU"))


temp[, idx := .GRP, by = list(LEG_ORIG, LEG_DEST, FLT_DPTR_DT, FLT_ID)]




cppFunction('void opt_C(NumericMatrix rev, NumericMatrix opt_L, NumericMatrix opt_F, NumericVector sell_L, NumericVector sell_F, NumericVector fare_L, NumericVector fare_F, double lambda_L, double lambda_F){
            int nrow = rev.nrow();
            int ncol = rev.ncol();
            
            for(int prd = 1; prd < ncol; prd++)
            {
            for(int rem = 1; rem < nrow; rem++)
            {
            double tmp = 0;
            int local = 0;
            int flow = 0;
            double tmp_new;
            
            for(int i = 0; i < 10; i++ )
            {
            for(int j = i; j < 10; j++ )
            {
            tmp_new = lambda_L*((sell_L[i]*(fare_L[i] + rev((rem-1),(prd-1)) ))+(1-sell_L[i]) * rev(rem,(prd-1)) )+
            lambda_F*((sell_F[j]*(fare_F[j] + rev((rem-1),(prd-1)) ))+(1-sell_F[j]) * rev(rem,(prd-1)) )+
            (1-lambda_L-lambda_F) * rev(rem,(prd-1));
            if( tmp_new > tmp)
            {
            tmp = tmp_new;
            local = i;
            flow = j;
            }
            }
            }
            opt_L(rem, prd) = local + 1;
            opt_F(rem, prd) = flow + 1;
            rev(rem, prd) = tmp;
            }
            }
            }')

message("finished Compiled!")

##########################################################
dp <- function(dt_inputs_){
  dt_inputs <- copy(dt_inputs_)
  #dt_inputs<-temp[idx %in% (180)]
  
  AU <- unique(dt_inputs$CAP)
  
  if(length(AU) > 1){stop("Wrong Idx!")}
  
  ########calculation for lambda and other parameters############
  
  tot_dmd<-sum(dt_inputs$ATTAIN_DMD)
  
  tot_dmd_L<-sum(dt_inputs[LCL_FLW_IND=="L"]$ATTAIN_DMD)
  
  tot_dmd_F<-sum(dt_inputs[LCL_FLW_IND=="F"]$ATTAIN_DMD)
  
  
  ###calculate these two in order to get sell-up probability
  tot_dmd_L_t<-sum(dt_inputs[LCL_FLW_IND=="L"]$ACH_DMD)
  
  tot_dmd_F_t<-sum(dt_inputs[LCL_FLW_IND=="F"]$ACH_DMD)
  
  # determine the number of periods####
  prd_num<-ceiling(tot_dmd)
  
  
  # calculate lambda for local and flow separately
  
  lambda_L<-tot_dmd_L/prd_num
  
  lambda_F<-tot_dmd_F/prd_num
  

  
  # calculate sell-up probability#####
  
 
  dt_inputs[, sell:= ifelse(yd_cls==10,1, ifelse(tot_dmd_L==0&LCL_FLW_IND=="L", 0, ifelse(tot_dmd_F==0&LCL_FLW_IND=="F", 0, ifelse(LCL_FLW_IND=="L", cd_achv/tot_dmd_L_t, cd_achv/tot_dmd_F_t)) ))]
  
  
  ####transpose the data from long to wide################
  dt<-dt_inputs[,.(LEG_ORIG, LEG_DEST,  FLT_DPTR_DT, FLT_ID, LCL_FLW_IND, yd_cls, ACH_DMD, ATTAIN_DMD, FARE_F, TD_NOW, ID_NOW, cd_achv, cd_att, tot_rev, avg_fare, sell)]
  
  dt<-dcast(setDT(dt), LEG_ORIG + LEG_DEST + FLT_DPTR_DT + FLT_ID + yd_cls ~ LCL_FLW_IND, value.var=c('ACH_DMD', 'ATTAIN_DMD', 'FARE_F', 'TD_NOW', 'ID_NOW', 'cd_achv', 'cd_att','tot_rev', 'avg_fare', 'sell'))
  
  
  #setnames(dt, c("FARE_F", "FARE_L","avg_fare_L", "avg_fare_F") , c("orig_FARE_F", "orig_FARE_L", "fare_L", "fare_F" ) )
  
  
  sell_F <- dt$sell_F; sell_L <- dt$sell_L; fare_F <- dt$avg_fare_F; fare_L <- dt$avg_fare_L
  
  ####initialize decision matrix: revenue matrix and selling local and selling flow matrix######
  rev=matrix(0,ncol=prd_num+1, nrow=AU+1)
  opt_L=matrix(0,ncol=prd_num+1, nrow=AU+1) #optimal selling decision for local
  opt_F=matrix(0,ncol=prd_num+1, nrow=AU+1) #optimal selling decision for flow
  
  
  ###########DP CORE: calculate revenue function ###################################
  
  
  ###i is selling cls for local
  ###j is selling cls for flow
  
  opt_C(rev,opt_L,opt_F,sell_L, sell_F,fare_L,fare_F,lambda_L,lambda_F)
  
  
  return(list(opt_L = opt_L, opt_F =opt_F, rev = rev))
}


####outputs#######

#i = 1
#max(temp$idx)
to = min(max(temp$idx), to)

for(i in fr: to){
  print(i)
  t0 <- proc.time()
  dt_tmp <- copy(temp[idx == i])[, idx:= NULL]
  res_tmp <- dp(dt_tmp)
  write.csv(res_tmp$opt_L, paste0(datadir, "OPT_L_", i, ".csv"))
  write.csv(res_tmp$opt_F, paste0(datadir, "OPT_F_", i, ".csv"))
  write.csv(res_tmp$rev, paste0(datadir, "rev_", i, ".csv"))
  message("Total Time for flight_", i, " is ", round((proc.time() - t0)[3]), " secs!")
}




#Not Run
# write.csv(opt_L, "Z:/premium/selling_coach_L_ceiling_2.csv")
# write.csv(opt_F, "Z:/premium/selling_coach_F_ceiling_2.csv")
# write.csv(rev, "Z:/premium/rev_coach_LF_2.csv")
# 
# 
