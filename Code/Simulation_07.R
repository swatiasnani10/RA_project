rm(list=ls())

source("FunctionFolder/Funcs_02.r")

# PARAMETERS
#[20210406 Update]
RangePat4EachPhy = 15:20 # how many patients to each physician, interval
NumPhy = 100 # num of physicians
NumSpe = 50  # num of specialists
NumPat = NumPhy * max(RangePat4EachPhy) # upper bound of num of patients
NumSpe4EachPhy = 5 # num of specialists that one physician has
#[20210406 Update]
MaxShrPreferredSpe = c(0.6,0.8) # interval of market share that the preferred specialists can take, it is a uniform rand

# -------------------------------------------------------


# first, generates physicians dataframe
df_phy = data.frame(
  id_phy = 1:NumPhy # physician id
)
# then, generates specialists dataframe with specialist quality
df_spe = data.frame(
  id_spe = 1:NumSpe,          # specialist id
  spe_quality = runif(NumSpe) # quality drawn from Uniform(0,1)
)
# then, generates patients dataframe with patient outcome
#df_pat = data.frame(
  # id_pat         = 1:NumPat,   # patient id
  # pat_outcome = runif(NumPat)  # patient outcome from Uniform(0,1) 
# )

#[Update: 20210413]
# then, generates patients
df_pat = Funcs$gen_new_pat(NumPat,1)  #Randomly picking one id from 1:[no.of phy*max(pat)=2000]

# -------------------------------------------------------

# pairs physicians and specialists, and make a big dataframe
list1= lapply(df_phy$id_phy, function(pid){
  # for the given physician with id `pid`, select some specialists
  flag_picked = sample(df_spe$id_spe, NumSpe4EachPhy, replace = FALSE)
  #Pick all columns but only flag picked rows
  spe_picked  = df_spe[flag_picked, ]  
  # adds pid to the sampled dataframe
  spe_picked$id_phy  = pid
  # given the current physician and selected specialists,
  # generate market shares for each specialist from a Uniform(0,1)
  # spe_picked$mkt_shr = runif(nrow(spe_picked))                         
  # spe_picked$mkt_shr = spe_picked$mkt_shr / sum(spe_picked$mkt_shr)    
  # [20210406 update]: we now make the distribution of mkt. shr more 
  #                    skewed on one specialist
  # [20210413 pending update]: Beta distribution ?
  spe_picked$mkt_shr = Funcs$gen_mktshr_baseline(nrow(spe_picked), MaxShrPreferredSpe = MaxShrPreferredSpe)
  return(spe_picked)
})

df_physpe = do.call("rbind", list1)

# -------------------------------------------------------

# let's define a list recording all patients that have been selected by other physicians
tmpvec_picked_pat = c()

# now, let's send patients to physicians
# NOTE: note that `lapply` is a parallel function s.t. the `tmpvec_picked_pat`
#       will not be updated between two rounds of the loop
df_patphy = list()
for(pid in df_phy$id_phy){
  # first, the physician randomly determines how many patients to pick
  num_pat_pick  = sample(RangePat4EachPhy, 1)
  # determines all patients that have not been selected
  pat_notpicked = setdiff(df_pat$id_pat, tmpvec_picked_pat)
  # given a physician, randomly pick up patients who have not been picked by other physicians
  id_picked   = sample(df_pat$id_pat[df_pat$id_pat %in% pat_notpicked], num_pat_pick)
  # save the selected patients to the picked namelist
  tmpvec_picked_pat = c(tmpvec_picked_pat, id_picked)
  # find the selected patients and mark them by the physician
  pat_picked = df_pat[df_pat$id_pat %in% id_picked, ]
  pat_picked$id_phy = pid
  
  # save the results
  df_patphy[[pid]] = pat_picked
  
  # drop intermediate temp variables
  rm(num_pat_pick,pat_notpicked,id_picked,pat_picked)
} # for pid

df_patphy = do.call("rbind", df_patphy)

# as a double check, you may check if each patient only appears once
# method 1: length(unique(tmpvec_picked_pat)) == length(tmpvec_picked_pat)
# method 2: all(table(df_patphy$id_pat) == 1)

# we drop intermediate variable
rm(tmpvec_picked_pat)

# -------------------------------------------------------

# then, let's send patients from physician to specialists according to market shares
df_patphyspe = do.call("rbind", lapply(1:nrow(df_patphy), function(ridx){
  # get the current (physician, patient) pair
  tmpid_phy = df_patphy$id_phy[ridx]
  tmpid_pat = df_patphy$id_pat[ridx]
  # given pair (physician, patient), get all available specialists
  available_spe = df_physpe[df_physpe$id_phy == tmpid_phy, ]
  # randomly select one specialist according to their market shares (as prob)
  selected_spe  = sample(x = available_spe$id_spe, 1, replace = TRUE, prob = available_spe$mkt_shr)
  # now, we have got (patient, physician, specialist) combination,
  # we would like to merge the info of patients in `df_pat` to the
  # dataframe `available_spe`, where we want to:
  #   1. patient info matches only the selected specialist
  #   2. all the other rows should be missing
  # the idea is to firstly creates a dataframe of patient information
  # based on one obs from `df_pat`, expanding it to be a same-size 
  # dataframe as `available_spe`; then merge the two dataframes.
  
  # first, get the obs of the current patient from `df_pat`
  info_pat  = df_pat[df_pat$id_pat == tmpid_pat,]
  # then, we expand (copy and past) it to a same-size dataframe as `available_spe`
  info_pat = info_pat[ rep.int(1,nrow(available_spe)) , ]
  # then, we mark the specialist that matches the current patient in `available_spe`
  flag_matched = available_spe$id_spe == selected_spe
  # then, we missing-lize all observations that do not match the selected specialist
  info_pat[!flag_matched, ] = NA
  # merge the patient information with available specialists
  available_spe = cbind(available_spe, info_pat)
  
  return(available_spe[ !is.na(available_spe$id_pat) ,])
}))

# as a double check, you may sampling and estimate the discrete distribution with frequency
# to check if the sampling program is working well
# tmp = sample(x = available_spe$id_spe, 10000, replace = TRUE, prob = available_spe$mkt_shr)
# table(tmp) / 10000
# available_spe

# -------------------------------------------------------

# generates 0-1 treatment outcomes by if specialist quality > patient outcome
# rule: if quality > outcome, then patient survives; otherwise, dies
# NOTE: 0 means dead, 1 means survived
df_patphyspe$treatment_outcome = as.numeric(df_patphyspe$spe_quality > df_patphyspe$pat_outcome)


# -------------------------------------------------------

# now, we are going to calculate the success rate for a specialist given a physician
list2 = lapply(df_phy$id_phy, function(pid){
  # given a physician id, slice a dataframe containing information of (patient, physician, specialist) combination
  df_pps = df_patphyspe[df_patphyspe$id_phy == pid, ]
  # creates a new variable to save success rates
  df_pps$success_rate = NA
  # [20210413: Update] ------------------
  # creates a new variable to save the total number of patients for specific specialist
  df_pps$totnum_pat_got = NA
  #--------------------------------------
  # loop among specialists, computes success rates for them
  namelist_spe = unique(df_pps$id_spe) # get a set of specialists to loop on
  for(sid in namelist_spe){
    # first, determine on which rows we are going to work on
    # these rows must have id_spe == sid
    flag_match_spe_id = df_pps$id_spe == sid
    # counts the number of these rows (as denominator soon)
    num_total_pat = sum(flag_match_spe_id)
    # [20210413: Update] ------------------
    # save the total number for the current specialist
    df_pps$totnum_pat_got[flag_match_spe_id] = num_total_pat
    #--------------------------------------
    # counts the number of successes (i.e. treatment_outcome == 1, patients survived)
    num_succe_pat = sum(df_pps$treatment_outcome[flag_match_spe_id] == 1)
    # computes success rates for the current specialist and save it
    df_pps$success_rate[flag_match_spe_id] = num_succe_pat / num_total_pat
  } # for sid
  
  return(df_pps)
})
df_patphyspe_baseline = do.call("rbind", list2)

save(df_patphyspe_baseline ,file = "df_patphyspe_baseline.RData")

# -------------------------------------------------------
#[Update: 20210413]
# add baseline information to (phy,spe) pair
df_physpe = merge(
  df_physpe, 
  df_patphyspe_baseline[,c("id_phy","id_spe","success_rate","totnum_pat_got")],
  by = c("id_phy","id_spe"),
  all.x = TRUE, all.y = FALSE
)
df_physpe = df_physpe[!duplicated(df_physpe),]
# fill missing values by assumptions(Ask if this is okay?)
df_physpe$success_rate[is.na(df_physpe$success_rate)] = 0.5
df_physpe$totnum_pat_got[is.na(df_physpe$totnum_pat_got)] = 0













