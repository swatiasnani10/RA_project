# ============================================================
# METHODS TO GENERATE SIMULATED DATASETS
# ============================================================
Fn = new.env()

# ------------------------------------------------------------
# A function to generate market share for one physician
Fn$gen_mktshr_baseline = function(NumSpe, MaxShrPreferredSpe = c(0.6,0.8)){
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

# ------------------------------------------------------------

#Learn using sapply more!
#TESTING: 
#tmp = t(sapply(1:100,function(x){Fn$gen_mktshr_baseline(5)})) 
#Gen mkt share for all 5 specialists by running the above function 100 times,
# and obtaining output in matrix form using sapply and t to transpose it.
















