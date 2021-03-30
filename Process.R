#Step 1: Generate maximum patient ids that is from 1-600
#Gen pcp ids-1-100
#Gen specialist ids 1-50


#Market: PCP1
#Physician-specialists pairs (One pcp repeats 5 fixed number of times, allocated random 5 spec out of 50 spec and other pcp may also get same spec.)
#1      #1      #0.312
#1      #50     #0.52
#1      #43     #0
#1      #32     #0.12
#1      #21     #0.14
                #(=1.00)
#Note: This share allocation to each of these specialists is done on a random basis(uniform)
###############
#2      #1      #0.32
#2      #50     #0.52
#2      #32     #0.80``
#2      #33     #0.85
#2      #27     #1.00
                #(=1.00)
###############

#Allocate 0.67%-1% of patients out of total possible 600 patients to PCP1. lIKEWISE, DO FOR ALL PCP's. Patient outcome b/w 0-1 randomly to all patients.
#So, data looks like 
#pcp1  Patient ids  Patient outcome
#1       #3          #0.2
#1       #432        #0.678
#1       #67         #0.123
#1       #342        #0
############
#2       #390
#2       #32
#2       #547
#2       #511
#2       #211
###########
#3       #39
#3       #324
#3       #54
#3       #51
#3       #267
#3       #234

##If the patient outcome is more than specialist quality, gen 1(bad outcome), o/w 0. This var is patient quality, but before this 
#allocate patients to specialists.

#If patient outcome falls under specialist cdf, then patient gets allocated to that specialist.
###################################################################################################

