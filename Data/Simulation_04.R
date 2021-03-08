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
colnames(df_01) = c("id_physician","id_specialist","id_patient")    #Name the columns

set.seed(1)
random.spe = runif(50)     #Draw 50 random numbers uniformly (as we have 50 specialists and we want a random value for each specialist)
df_01$Specialist_quality = -1
for(index.spe in 1:N_spe){        #As we are operating on specialist level, loop will be based on specialist number
  id.current.spe = index.spe      # by our assumption of specialist id
  df_01$Specialist_quality [df_01$id_specialist == id.current.spe] = random.spe[index.spe]     #We pick up random number from random.spe acc. to the index of specialist
} #In variable quality.spe, filter those specialist ids that are equal the current specialist id, and assign rv)

set.seed(2)
quality.pat = runif(N_pat)  
set.seed(3)#Generates #N_pat (=25k) uniform random samples b/w 0 and 1
Patient_outcome = runif(N_pat)  
df_01$Patient_outcome = Patient_outcome


Quality = ifelse(df_01$Patient_outcome > df_01$Specialist_quality , 1, 0)     #If the patient outcome is more than specialist quality, gen 1, o/w 0.
df_01$Quality = Quality


######Making the above dataset df_01 more realistic: One PCP sending 4-6 patients to a specialist & not to all specialists.

# Run a loop on physicians (to keep all physician id): For each id, lapply will apply this function and make a list for all ids
li.result = lapply(1:N_pcp, function(pcp_id){
  
  # slice a sub-dataframe according to given physician id
  subdf = df_01[ df_01$id_physician == pcp_id ,]
  # generate unique random integers between 1 and nrow(subdf)
  # to slice `subdf`
  set.seed(4)
  ratio.selectrow = runif(1,min=0.02,max=0.03) #Allocating 2-3 percent of one PCP's originally assumed 250 patients and doing this for all PCP's
  num.selectrow   = floor(nrow(subdf) * ratio.selectrow) #No. of rows in subdf is number of patients per PCP. Each subdf is for a single PCP id.
  rowidx = sample.int(nrow(subdf), num.selectrow)
  # return the sliced dataframe
  return(subdf[rowidx,])
})

# append all subdf into one df
df_04 = do.call('rbind',li.result)
save(df_04 ,file = "df_04.RData")






