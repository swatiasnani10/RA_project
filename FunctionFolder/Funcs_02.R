# ============================================================
# METHODS TO GENERATE SIMULATED DATASETS
# ============================================================
Funcs = new.env()

# A function to generate market share for one physician
# TESTING: tmp = t(sapply(1:100,function(x){Funcs$gen_mktshr_baseline(5)}))
Funcs$gen_mktshr_baseline = function(NumSpe, MaxShrPreferredSpe = c(0.6,0.8)){
  # since the picked specialists are in random order,
  # we can directly pick the first specialist as the preferred one
  # we decide the total shares that these preferred specialists take
  ShrPrefer = runif(1, min = MaxShrPreferredSpe[1], max = MaxShrPreferredSpe[2])
  # then, we uniformly decide the market shares for all the other specialists
  MktShr4Others = runif(NumSpe-1)
  # and normalize all other guys' shares to have sum = 1-ShrPrefer
  MktShr4Others = MktShr4Others / sum(MktShr4Others) * (1-ShrPrefer)
  # finally, we return the market share vector
  return(c(ShrPrefer,MktShr4Others))
} # gen_mktshr_baseline

##########################################################################################################

# ------------------------------------------------------------
#[20210413 Update]------------------------------------------------------------
# A function to update market share given the latest successful rates

#Extract first element of list i.e. baseline data for the two relevant df for gen of updated mktshr
#View(lidf_patphyspe[[1]]): has only those spec to whom patients were sent(<=5)
#View(lidf_physpe[[1]]): has all 5 specialists assigned to phy

Funcs$update_mktshr = function(df_physpe, df_patphyspe, default_mktshr_for_nohistory_success = 0.5){
  # first, merge the two dataframes by (id_phy,id_spe), df_physpe as the left table as we want to retain all 5 spec
  df_merged = merge(df_physpe, df_patphyspe[,c("id_phy","id_spe","success_rate","totnum_pat_got")],  #keep columns relevant for gen of mkt shr
                    by = c("id_phy","id_spe"),
                    all.x = TRUE,          # if to keep all obs/rows in the left table (Read help)
                    all.y = FALSE,         # if to keep all obs/rows in the right table
                    suffixes = c("",".y")  # To avoid problem in future iterations
  )
  # then, drop duplicate rows
  df_merged = df_merged[!duplicated(df_merged),]
  # then, update successful rates with new information if available(replacing all non-missing success rates in both df)
  df_merged$success_rate[!is.na(df_merged$success_rate.y)] = df_merged$success_rate.y[!is.na(df_merged$success_rate.y)]
  # then, update missing total num of received patient
  df_merged$totnum_pat_got[!is.na(df_merged$totnum_pat_got.y)] = df_merged$totnum_pat_got.y[!is.na(df_merged$totnum_pat_got.y)]
  # drop intermediate new information vars
  df_merged$success_rate.y = NULL
  df_merged$totnum_pat_got.y = NULL
  
  # finally, we update the market shares using logit setup for each unique physician id(in merged data) that gets one patient
  df_merged = do.call("rbind", lapply(unique(df_merged$id_phy), function(pid){
    # slice the dataframe for the current physician(as before)
    df_tmp = df_merged[df_merged$id_phy == pid,]    #Now, we see dataframe for one particular phy at a time
    # define exp(success rate) 
    df_tmp$exp_rate = exp(df_tmp$success_rate)
    # normalize the exp() to be logit prob, as defined in summary.pdf
    df_tmp$logit_prob = df_tmp$exp_rate / sum(df_tmp$exp_rate)
    return(df_tmp)
  }))
  
  # replace the old market share with the updated one
  df_merged$mkt_shr = df_merged$logit_prob
  
  # return the dataframe but only containing necessary variables
  # NOTES: only drop intermediate columns but not judge the existing columns
  df_merged$exp_rate     = NULL
  df_merged$logit_prob   = NULL
  
  return(df_merged)
} # update_mktshr

##########################################################################################################
# ------------------------------------------------------------
# A function to generate new patients
Funcs$gen_new_pat = function(NumPat, StartID){
  return(data.frame(
    id_pat      = StartID + (1:NumPat) - 1,      # patient id
    pat_outcome = runif(NumPat)  # patient outcome from Uniform(0,1) in the benchmark period
  ))
} # gen_new_pat












