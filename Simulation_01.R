#Assumptions for one market and one time period

N_pcp = 100;        #No. of physicians
N_spe=50;           #No. of specialists
N_pcp2spe= 5;       #No. of patients sent by a physician to a particular specialist
N_pat = N_pcp * N_spe * N_pcp2spe   #No. of patients

#Generate a random quality measure for each of the 25k patients

Patient_id = 1:N_pat                            #Unique id for each patient
Quality_01 = runif(N_pat)                       #Generates #N_pat (=25k) uniform random samples b/w 0 and 1
Quality_02 = ifelse(runif(N_pat)>0.5, 1, 0)     #If a number in Quality_01 is more than 0.5, gen 1, o/w 0.


# Gen. phy-spec pairs. Eg.(1,1), (1,2),..(1,50),(2,1). (2,2)....(2,50),...,(100,1),(100,2)..(100,50)
df_01 = expand.grid(1:N_pcp,1:N_spe)        #Gen. phy-spec pairs
nrow(df_01)                                 #This data frame has 5000 rows.
seq_len(nrow(df_01))                        #creates a sequence from 1 to 5000 (= #rows in result.df)
df_01 = df_01[rep(seq_len(nrow(df_01)), each = N_pcp2spe),] #Repeat seq. from 1-5k for 5 times

# Add columns to such a data frame
df_01$id_patient  = Patient_id         
df_01$quality_index = Quality_02

# Give row and column names
rownames(df_01) = 1:nrow(df_01)       #Name the rows with whole numbers
colnames(df_01) = c("id_physician","id_specialist","id_patient","quality_index")

save(df_01 ,file = "df_01.RData")
load('df_01.RData')

