#Week 2#############################
#Assumptions for one market and one time period

N_pcp = 100;        #No. of physicians
N_spe=50;           #No. of specialists
N_pcp2spe= 5;       #No. of patients sent by a physician to a particular specialist
N_pat = N_pcp * N_spe * N_pcp2spe   #No. of patients

#Generate a random quality measure for each of the 25k patients
Patient_id = 1:N_pat                                  #Unique id for each patient

# Gen. phy-spec pairs. Eg.(1,1), (1,2),..(1,50),(2,1). (2,2)....(2,50),...,(100,1),(100,2)..(100,50)
df_01 = expand.grid(1:N_pcp,1:N_spe)                   #Gen. phy-spec pairs
nrow(df_01)                                            #This data frame has 5000 rows.
seq_len(nrow(df_01))                                   #creates a sequence from 1 to 5000 (= #rows in result.df)
df_01 = df_01[rep(seq_len(nrow(df_01)), each = N_pcp2spe),] #Repeat seq. from 1-5k for 5 times
Patient_id = 1:N_pat
df_01$id_patient  = Patient_id                          #Unique id for each patient

# Give row and column names
rownames(df_01) = 1:nrow(df_01)                        #Name the rows with whole numbers
colnames(df_01) = c("id_specialist","id_physician", "id_patient")    #Name the columns


random.spe = runif(50)     #Draw 50 random numbers uniformly (as we have 50 specialists and we want a random value for each specialist)
df_01$Specialist_quality = -1
for(index.spe in 1:N_spe){        #As we are operating on specialist level, loop will be based on specialist number
  id.current.spe = index.spe      # by our assumption of specialist id
  df_01$Specialist_quality [df_01$id_specialist == id.current.spe] = random.spe[index.spe]     #We pick up random number from random.spe acc. to the index of specialist
} #In variable quality.spe, filter those specialist ids that are equal the current specialist id, and assign rv)


quality.pat = runif(N_pat)           #Generates #N_pat (=25k) uniform random samples b/w 0 and 1
Patient_outcome = runif(N_pat)  
df_01$Patient_outcome = Patient_outcome


Quality = ifelse(df_01$Patient_outcome > df_01$Specialist_quality , 1, 0)     #If the patient outcome is more than specialist quality, gen 1, o/w 0.
df_01$Quality = Quality

###Week 3: 3/3/2021################################################################################################
# Run a loop on physicians (to keep all physician id)
li.result = lapply(1:N_pcp, function(pcp_id){
  
  # slice a sub-dataframe according to given physician id
  subdf = df_01[ df_01$id_physician == pcp_id ,]
  # generate unique random integers between 1 and nrow(subdf)
  # to slice `subdf`
  ratio.selectrow = runif(1,min=0.4,max=0.6)
  num.selectrow   = floor(nrow(subdf) * ratio.selectrow)
  rowidx = sample.int(nrow(subdf), num.selectrow)
  # return the sliced dataframe
  return(subdf[rowidx,])
})

# append all subdf into one df
df_03 = do.call('rbind',li.result)
save(df_03 ,file = "df_03.RData")









