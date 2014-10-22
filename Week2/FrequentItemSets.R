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

# Q1

We will ignore the memory needed for frequent items since this s only 250k * 4 bytes = 1M bytes
buckets 4 bytes per bucket
S is main memory thus S/4 is the number of buckets

there are 1M (one million) frequent buckets, if infrequent pair hashes into these it will have to be counted

# main memory equals to number of stuff hashed into frequent basket plus bitmap size
# 12 comes from the fact that 12 bytes needed per hash, since it is (item,item, count) 3 integers 4 bytes each
# bitmap size is one bit per bucket. since there are s/4 buckets size will be s/4/8 bytes
b = s/4

s = [1M*p/b]*12 + b/8
s = [1M * p * 4 / s]*12 + s/32
s = [48M * p]/s + s/32
31/32*s = 48M * p / s
31/32*s^2 = 48M * p
s^2 = 32/31*48M * p
s^2 = 49548387 * p
p = s^2 / 49548387

minimem <- function(s, p) {
    print(p - s^2/49548387)
    p < s^2/49548387
}

minimem(1e+09, 3.5e+10)
minimem(2e+08, 4e+08)
minimem(5e+08, 5e+09) # closest
minimem(5e+08, 3.2e+09)

# Q2 Toivonen's algorithm

During a run of Toivonen's Algorithm with set of items {A,B,C,D,E,F,G,H} a sample is found to have the following maximal frequent itemsets: {A,B}, {A,C}, {A,D}, {B,C}, {E}, {F}. Compute the negative border. Then, identify in the list below the set that is NOT in the negative border.'
{B,D}
{E,F}
{G}
{A,B,D}
######
{A,B,C,D,E,F,G,H}
maximal frequent itemsets: {A,B}, {A,C}, {A,D}, {B,C}, {E}, {F} 
-> Also frequent {A}, {B}, {C}, {D}

Negative border: {A,B,C}, since all 2 item subsets of this set are frequent, but the set itself isn't.'
Negative border also {E,F} since all 1 item subsets are frequent, but the set itself isn't.'
same logic: {A,F}, {A,E}, {B,D}, {B,E}, {B,F}, {C,E}, {C,F}
Also in the negative border {G} and {H}, since all their smaller subsets (empty set) are frequent, 
but these sets themselves arent.

-> Thus the anser is {A,B,D}


