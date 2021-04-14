# GENERATING DATA IN A SEQUENTIAL WAY (PATIENT-BY-PATIENT)
# ---------------------------------------------------------

# first of all, generate baseline data as done in 'Simulation_07.R'
source("Code/Simulation_07.R")

# ---------------------------------------------------------
#Parameters
NumSeqGene = 20 # number of rounds to apply such a sequential generating method
                #(Assessing 20 patients one by one)

# ---------------------------------------------------------

# define an empty list to save the generated dataframe in each round (this is the outcome)
lidf_patphyspe = list()
# define an empty list to save phy-spe pair info and market share info
lidf_physpe = list()
# define an empty list to save pat-phy pair info
lidf_patphy = list()
# define an empty list to save newly generated patients info (since we want totally new patients in each round)
lidf_pat = list()
# define an empty list to save physicians (redundant for now but for better understanding)
lidf_phy = list()
# define an empty list to save specialists (redundant for now but for better understanding)
lidf_spe = list()


# ---------------------------------------------------------

# save the baseline dataframe and its component dataframes as the first elements of empty lists
lidf_patphyspe[[1]] = df_patphyspe_baseline
# save the baseline phy-spe
lidf_physpe[[1]] = df_physpe
# save the baseline pat-phy
lidf_patphy[[1]] = df_patphy
# save the baseline patient info
lidf_pat[[1]] = df_pat
# save the baseline phy info
lidf_phy[[1]] = df_phy
# save the baseline spe info
lidf_spe[[1]] = df_spe


# ---------------------------------------------------------

# remove intermediate baseline dataframes (to avoid misuse of information)
rm(df_pat,df_phy,df_spe,df_patphy,df_physpe,df_patphyspe,df_patphyspe_baseline)

# ---------------------------------------------------------

# run the following loop over patients one by one(sequentially) to generate data
for (iter in 1:NumSeqGene) {
  # iter = 1
  
  # update basic info: physician (underlying assumption: no change in physician across time)
  lidf_phy[[iter+1]] = lidf_phy[[iter]]
  # update basic info: specialist
  lidf_spe[[iter+1]] = lidf_spe[[iter]]
  
  # Step1. update market share of the specialists(Function of updating written in Func_02.R)
  lidf_physpe[[iter+1]] = Funcs$update_mktshr(
    lidf_physpe[[iter]], 
    lidf_patphyspe[[iter]], 
    default_mktshr_for_nohistory_success = 0.5   #Subject to change
  )
  
  # generate new distinct patients
  lidf_pat[[iter+1]] = Funcs$gen_new_pat(
    NumPhy, # one patient for one physician
    max(lidf_pat[[iter]]$id_pat) + 1 # start from the max of pat id of last round + 1 (to not have same patients as we had in baseline)
  )
  
  # pair patient to physician
  # NOTES: since the num of patients are exactly the same as the num of phy,
  #        and these patients are drawn independently from physicians,
  #        then we can just cbind the two dataframes
  lidf_patphy[[iter+1]] = cbind(
    lidf_pat[[iter+1]],
    lidf_phy[[iter+1]]
  )
  
  # send patient from phy to (random) spe
  tmpdf_patphyspe = do.call("rbind", lapply(1:nrow(lidf_patphy[[iter+1]]), function(ridx){
    # define several temp dataframes just inside this function call, to be able to copy baseline codes
    df_patphy = lidf_patphy[[iter+1]]
    df_physpe = lidf_physpe[[iter+1]]
    df_pat    = lidf_pat[[iter+1]]
    
    # get the current (physician, patient) pair
    tmpid_phy = df_patphy$id_phy[ridx]
    tmpid_pat = df_patphy$id_pat[ridx]
    # given pair (physician, patient), get all available specialists
    available_spe = df_physpe[df_physpe$id_phy == tmpid_phy, ]
    # randomly select one specialist according to their market shares (as prob)
    selected_spe  = sample(x = available_spe$id_spe, 1, replace = TRUE, prob = available_spe$mkt_shr)
    # first, get the obs of the current patient from `df_pat`
    info_pat  = df_pat[df_pat$id_pat == tmpid_pat,]
    # then, we expand (copy and paste) it to a same-size dataframe as `available_spe`
    info_pat = info_pat[ rep.int(1,nrow(available_spe)) , ]
    # then, we mark the specialist that matches the current patient in `available_spe`
    flag_matched = available_spe$id_spe == selected_spe
    # then, we missing-lize all observations that do not match the selected specialist
    info_pat[!flag_matched, ] = NA
    # merge the patient information with available specialists
    available_spe = cbind(available_spe, info_pat)
    
    return(available_spe[ !is.na(available_spe$id_pat) ,])
  }))
  # generates 0-1 treatment outcomes by if specialist quality > patient outcome
  # rule: if quality > outcome, then patient survives; otherwise, dies
  # NOTE: 0 means dead, 1 means survived
  tmpdf_patphyspe$treatment_outcome = as.numeric(tmpdf_patphyspe$spe_quality > tmpdf_patphyspe$pat_outcome)
  
  # update successful rates for each specialist given physician
  # first, count the number of all received patients this period for each (phy,spe) pair
  tmpdf_patphyspe = do.call("rbind", lapply(1:nrow(tmpdf_patphyspe), function(ridx){
    # get the current (physician, patient) pair
    tmpid_phy = tmpdf_patphyspe$id_phy[ridx]
    tmpid_spe = tmpdf_patphyspe$id_spe[ridx]
    
    # given pair (physician, specialist), get all available obs
    # sum them up to get the total number of patients that a (phy,spe) pair
    # received in THIS PERIOD
    flag_pat_thisperiod = (tmpdf_patphyspe$id_phy == tmpid_phy) & (tmpdf_patphyspe$id_spe == tmpid_spe)
    
    # make a small dataframe for the current (phy,spe) pair
    tmpdf = tmpdf_patphyspe[flag_pat_thisperiod,]
    tmpdf$num_pat_thisperiod = sum(flag_pat_thisperiod)
    
    # count the number of surviving patients this period
    tmpdf$num_success_thisperiod = sum(tmpdf$treatment_outcome == 1)
    
    return(tmpdf)
  }))
  
  # update successful rates
  tmpdf_patphyspe$totnum_pat_got = tmpdf_patphyspe$totnum_pat_got + tmpdf_patphyspe$num_pat_thisperiod
  tmpdf_patphyspe$success_rate = 
    (tmpdf_patphyspe$totnum_pat_got * tmpdf_patphyspe$success_rate + # num of historical successes
       tmpdf_patphyspe$num_success_thisperiod) / tmpdf_patphyspe$totnum_pat_got
  
  # finally, drop intermediate columns
  tmpdf_patphyspe[,c("num_pat_thisperiod","num_success_thisperiod")] = NULL
  
  # save
  lidf_patphyspe[[iter+1]] = tmpdf_patphyspe
  
} # for

##Note: The 'tmdf_patphyspe' dataframe is the 21st dataframe, so after performing loop
#for 20 rounds of incoming patient for all phy, we can see revision of market shares and success rates.
#This 'tmdf_patphyspe' df is the same as 'View(lidf_patphyspe[[21]])'.
#Can append all 21 dataframes in the lidf_patphyspe !!

df_patphyspe_seq <- do.call("rbind", lidf_patphyspe)
save(df_patphyspe_seq ,file = "df_patphyspe_seq.RData")

#We see 1771 observations from benchmark data and 2000 obs(20 rounds*100 phy) shows updating of beliefs


