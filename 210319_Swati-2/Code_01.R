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
  # pat_age        = sample(1:100,NumPat,replace = TRUE) # age of patient
)

# -------------------------------------------------------
lapply(df_phy$id_phy, function(pid){
  # for the given physician with id `pid`, select some specialists
  flag_picked = sample(df_spe$id_spe, NumSpe4EachPhy, replace = FALSE)
  spe_picked  = df_spe[flag_picked, ]
  # adds pid to the sampled dataframe
  spe_picked$id_phy  = pid
  # given the current physician and selected specialists,
  # generates market shares for each specialist form a Uniform(0,1)
  spe_picked$mkt_shr = runif(nrow(spe_picked))
  spe_picked$mkt_shr = spe_picked$mkt_shr / sum(spe_picked$mkt_shr)
  return(spe_picked)
})




# pairs physicians and specialists, and make a big dataframe
df_physpe = do.call("rbind", lapply(df_phy$id_phy, function(pid){
  # for the given physician with id `pid`, select some specialists
  flag_picked = sample(df_spe$id_spe, NumSpe4EachPhy, replace = FALSE)
  spe_picked  = df_spe[flag_picked, ]
  # adds pid to the sampled dataframe
  spe_picked$id_phy  = pid
  # given the current physician and selected specialists,
  # generates market shares for each specialist form a Uniform(0,1)
  spe_picked$mkt_shr = runif(nrow(spe_picked))
  spe_picked$mkt_shr = spe_picked$mkt_shr / sum(spe_picked$mkt_shr)
  return(spe_picked)
}))
