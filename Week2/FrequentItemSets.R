# Basic quiz
# Q1

# support 10,000
# One million items
# N frequent items
# One million frequent pairs
# 2M pairs that occur exactly once. M of these consists of 1 freq 1 unfreq items, and other half M from 2x freq items
# No other pairs occur at all
# Integers are always represented by 4 bytes

# memory needed triangular/hashtable ?

# For Triangular table we need (N^2/2) - N = choose(N,2) integers, 4 bytes each
# thus choose(N,2)*4 bytes.

# Hash table uses item-item-count triples, so 12 bytes per each hash table "item"
# We do not need to save the pairs where one item is unfrequent so we can discard those M pairs, and only store the M pairs that both items are frequent
# Thus we will need M * 12 bytes of memory

# -> Total memory needed is min(choose(N,2)*4, 2M*12) since we will choose the better algorithm always

# Calculates how much memory is left, where S is the memory, use S = 0 to check how much total memory needed
q1memory <- function(N,M, S = 0) {
    min(choose(N,2)*4, M*12) - S
}

# pick the one that is closest to 0
q1memory(2e+04, 6e+07, 1e+09)
q1memory(1e+05, 5e+07, 5e+09)
q1memory(1e+04, 4e+07, 2e+08)
q1memory(2e+04, 8e+07, 1.1e+09)


# Q2

# This question comes down to finding common denominators
# clearly set {3,5} has common denominator of 1 so this is the right answer

# Q3

#Suppose ABC is a frequent itemset and BCDE is NOT a frequent itemset. Given this information, we can be sure that certain other itemsets are frequent and sure that certain itemsets are NOT frequent. Other itemsets may be either frequent or not. Which of the following is a correct classification of an itemset?

# AB, AC, BC are frequent. And any set which has BCDE as a subset is not frequent

# ABCD can be either frequent or not frequent.     <-- right answer


##### Advanced quiz:

Suppose we perform the PCY algorithm to find frequent pairs, with market-basket data meeting the following specifications:

s, the support threshold, is 10,000.
There are one million items, which are represented by the integers 0,1,...,999999.
There are 250,000 frequent items, that is, items that occur 10,000 times or more.
There are one million pairs that occur 10,000 times or more.
There are P pairs that occur exactly once and consist of 2 frequent items.
No other pairs occur at all.
Integers are always represented by 4 bytes.

When we hash pairs, they distribute among buckets randomly, but as evenly as possible; i.e., you may assume that each bucket gets exactly its fair share of the P pairs that occur once.
Suppose there are S bytes of main memory. In order to run the PCY algorithm successfully, the number of buckets must be sufficiently large that most buckets are not large. In addition, on the second pass, there must be enough room to count all the candidate pairs. As a function of S, what is the largest value of P for which we can successfully run the PCY algorithm on this data? Demonstrate that you have the correct formula by indicating which of the following is a value for S and a value for P that is approximately (i.e., to within 10%) the largest possible value of P for that S.
