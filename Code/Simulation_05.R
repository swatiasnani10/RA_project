rm(list=ls())

# PARAMETERS
RangePat4EachPhy = 4:6 # how many patients to each physician, interval
NumPhy = 100 # num of physicians
NumSpe = 50  # num of specialists
NumPat = NumPhy * max(RangePat4EachPhy) # upper bound of num of patients
NumSpe4EachPhy = 5 # num of specialists that one physician has

# -------------------------------------------------------

# first, generates physicians
df_phy = data.frame(
  id_phy = 1:NumPhy # physician id
)
# then, generates specialists
df_spe = data.frame(
  id_spe = 1:NumSpe,          # specialist id
  spe_quality = runif(NumSpe) # quality drawn from Uniform(0,1)
)
# then, generates patients
df_pat = data.frame(
  id_pat         = 1:NumPat,      # patient id
  pat_outcome = runif(NumPat)  # patient outcome from Uniform(0,1) in the benchmark period
  # pat_age        = sample(1:100, NumPat,replace = TRUE) # age of patient
)

# -------------------------------------------------------
# pairs physicians and specialists, and make a big dataframe
list1 = lapply(df_phy$id_phy, function(pid){
  # for the given physician with id `pid`, select some specialists
  flag_picked = sample(df_spe$id_spe, NumSpe4EachPhy, replace = FALSE)  #Pick 5 unique specialist id's(5 rows) from vector of 'id_phy' 
  spe_picked  = df_spe[flag_picked, ]                                   #Pick all columns of dataframe but only for flagpicked rows
  # adds pid to the sampled dataframe
  spe_picked$id_phy  = pid
  # given the current physician and selected specialists,
  # generate market shares for each specialist(=5 here) from a Uniform(0,1)
  spe_picked$mkt_shr = runif(nrow(spe_picked))                         #Market shares do not sum to 1
  spe_picked$mkt_shr = spe_picked$mkt_shr / sum(spe_picked$mkt_shr)    #For them to sum to 1 for each pid, we normalize it.
  return(spe_picked)
})

##Above function applied 'pid' function to each physician one by one, simultaneously not sequentially!
#Now append all these 100 small dataframes to have a big physician-specialist dataframe.
df_physpe = do.call("rbind", list1)

# -------------------------------------------------------

# let's define a list recording all patients that have been selected by other physicians
tmpvec_picked_pat = c()

# now, let's send patients to physicians
# NOTE: note that `lapply` is a parallel function s.t. the `tmpvec_picked_pat`
#       will not be updated between two rounds of the loop. So, don't use lapply here.
df_patphy = list()
for(pid in df_phy$id_phy){
  # first, the physician randomly determines how many patients to pick
  num_pat_pick  = sample(RangePat4EachPhy, 1)
  # determines all patients that have not been selected
  pat_notpicked = setdiff(df_pat$id_pat, tmpvec_picked_pat)
  # given a physician, randomly pick up 4-6 patients who have not been picked by other physicians
  id_picked   = sample(df_pat$id_pat[df_pat$id_pat %in% pat_notpicked], num_pat_pick)
  # save the selected patients to the picked namelist
  tmpvec_picked_pat = c(tmpvec_picked_pat, id_picked)
  # find the selected patients and mark them by the physician
  pat_picked = df_pat[df_pat$id_pat %in% id_picked, ]  #Display all columns for rows that have been picked
  pat_picked$id_phy = pid                              #Add pid to df
  
  # save the results
  df_patphy[[pid]] = pat_picked
  
  # drop intermediate temp variables
  rm(num_pat_pick,pat_notpicked,id_picked,pat_picked)
} # for pid
df_patphy = do.call("rbind", df_patphy)

# as a double check, you may check if each patient only appears once
# method 1: length(unique(tmpvec_picked_pat)) == length(tmpvec_picked_pat)
# method 2: all(table(df_patphy$id_pat) == 1). If true,then unique patients!

# we drop intermediate variable
rm(tmpvec_picked_pat)

# -------------------------------------------------------
# -------------------------------------------------------

# then, let's send patients from physician to specialists according to market shares
df_patphyspe = do.call("rbind", lapply(1:nrow(df_patphy), function(ridx=10){
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
  # the idea is to firstly create a dataframe of patient information
  # based on one obs from `df_pat`, expanding it to be a same-size 
  # dataframe as `available_spe`; then merge the two dataframes.
  
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

# as a double check, you may sampling and estimate the discrete distribution with frequency
# to check if the sampling program is working well
# tmp = sample(x = available_spe$id_spe, 10000, replace = TRUE, prob = available_spe$mkt_shr)
# table(tmp) / 10000
# available_spe

# -------------------------------------------------------

# generates 0-1 treatment outcomes by if specialist quality > patient outcome
# rule: if quality > outcome, then 1; otherwise, 0
# NOTE: 0 means success, 1 means death
df_patphyspe$treatment_outcome = as.numeric(df_patphyspe$spe_quality > df_patphyspe$pat_outcome_t0)


# -------------------------------------------------------

# now, we are going to calculate the success rate for a specialist given a physician
df_patphyspe_baseline = do.call("rbind", lapply(df_phy$id_phy, function(pid){
  # given a physician id, slice a dataframe containing information of (patient, physician, specialist) combination
  df_pps = df_patphyspe[df_patphyspe$id_phy == pid, ]
  # creates a new variable to save success rates
  df_pps$success_rate = NA
  # loop among specialists, computes success rates for them
  namelist_spe = unique(df_pps$id_spe) # get a set of specialists to loop on
  for(sid in namelist_spe){
    # first, determine on which rows we are going to work on
    # these rows must have id_spe == sid
    flag_match_spe_id = df_pps$id_spe == sid
    # counts the number of these rows (as denominator soon)
    num_total_pat = sum(flag_match_spe_id)
    # counts the number of successes (i.e. treatment_outcome == 0)
    num_succe_pat = sum(df_pps$treatment_outcome[flag_match_spe_id] == 0)
    # computes success rates for the current specialist and save it
    df_pps$success_rate[flag_match_spe_id] = num_succe_pat / num_total_pat
  } # for sid
  
  return(df_pps)
}))


save(df_patphyspe_baseline ,file = "df_patphyspe_baseline.RData")







