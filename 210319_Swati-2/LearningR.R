##Remove function

# Creating a vector
vec <- c(1, 2, 3, 4)
vec
# Creating a list
list1 = list("Number" = c(1, 2, 3),
             "Characters" = c("a", "b", "c"))
list1
# Creating a matrix
#Syntax: matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
mat1 <- matrix(c(1:9), 3, 3, byrow=TRUE)
mat
mat2 <- matrix(c(1:9), 3, 3, byrow=FALSE)
mat
# Calling rm() Function
rm(list1)
# Calling ls() to check object list
ls()
##################################################################################
# To bind two different lists, we may use one of these. But, they actually return matrices, and not dataframes.
ls<-list(a=c(1:4),b=c(3:6))
do.call("rbind",ls)
#or
my.df<-rbind(ls)
str(ls)
#The outputs of the above functions are matrices, that contain list elements.
class(rbind(ls))
##################################################################################
##lapply: lapply applies a function to all elements of the input X and gives the output as a list.
#lapply(X, FUN)
#Arguments:
 # -X: A vector or an object
 #-FUN: Function applied to each element of x
require(stats); require(graphics)

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# compute the list mean for each list element
lapply(x, mean)
#################################################################################
#Sample function: x is a vector/list, n is a positive number of items to choose from and replace=FALSE means without replacement
sample(x, size, replace = FALSE, prob = NULL)
#To the vector 'id_spe' from data frame 'df_spe', pick elements equal to 'NumSpe4EachPhy=5' and pick unique elements
flag_picked = sample(df_spe$id_spe, NumSpe4EachPhy, replace = FALSE)
flag_picked

###################################################################################
###Extraction using big square brackets

#creates a matrix with number 1 to 30, and with 5 columns,
#and gives dimension names: rows are named by small letters  from 1:6 
#and columns are named by big letters from 1:5
mymat <- matrix(1:30, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5])) 
mymat

#Extract element that is at the intersection of 2nd row and 3rd column
mymat[2, 3]

## Extract all row elements from only second column
mymat[, 2]

#Extract all column elements from 3rd row
mymat[3, ]

#Extract all elements just ignore first row
mymat[-1, ] 