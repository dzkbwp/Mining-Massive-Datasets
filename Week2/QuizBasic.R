# Q 1

#       he, she, his, hers
#he     0   1   3   2
#she        0   4   3
#his            0   3
#hers               0

# -> 3 pairs with distance 3

# Q2

# The minhash value for C4 is R3, this is basically the first 1 that we encountered
# going thru the permutation order list

# Q3

# C1 and C4 are a candidate pair, since they agree on rows 1-2, which is the 1st band
# C1 and C6 are a candidate pair, since they agree on rows 3-4, which is the 2nd band

# Q4

#Find the set of 2-shingles for the "document":
#    
#    ABRACADABRA
#and also for the "document":
#    
#    
#    BRICABRAC
#Answer the following questions:
#    
#    How many 2-shingles does ABRACADABRA have?
#How many 2-shingles does BRICABRAC have?
#How many 2-shingles do they have in common?
#What is the Jaccard similarity between the two documents"?

# 2-shingles in ABRACADABRA: AB, BR, RA, AC, CA, AD, BA
# 2-shingles in BRICABRAC: BR, RI, IC, CA, AB, RA, AC
# ABRACADABRA has 7 2-shingles, BRICABRAC has 7 2-shingles
# 5 common 2-shingles: AB, BR, RA, CA
# 5/9 Jaccard similarity (#intersection / #union)


# Q6

L1norm <- function(vector) {
    sum(abs(vector))
}
L2norm <- function(vector) {
    sqrt(sum(vector^2))
}

# tests whether L1 and L2 norm both agree on whether (0,0) or (100,40) is the closest
# point to a given vector
q6test <- function(vector) {
    origo <- c(0,0)
    otherEnd <- c(100,40)
    
    L1 <- L1norm(vector - origo) > L1norm(vector - otherEnd)
    L2 <- L2norm(vector - origo) > L2norm(vector - otherEnd)
    L1 == L2
}

q6test(c(63,8))
q6test(c(51,18))  # False, so this is the correct answer
q6test(c(53,18))
q6test(c(55,5))
q6test(c(61,8))  # False
q6test(c(55,5))
q6test(c(66,5))
q6test(c(61,10))





