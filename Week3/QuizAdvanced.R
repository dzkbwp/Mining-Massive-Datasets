# Q1

For the following graph:
    C -- D -- E
/ |    |    | \
A  |    |    |  B
\ |    |    | /
    F -- G -- H
Write the adjacency matrix A, the degree matrix D, and the Laplacian matrix L. For each, find the sum of all entries and the number of nonzero entries. Then identify the true statement from the list below.


### answer:
Adjacency matrix will be 8x8 symmetric matrix

q1_A <- matrix(nrow = 8, ncol = 8)
           #c(A,B,C,D,E,F,G,H)
q1_A[1,] <- c(0,0,1,0,0,1,0,0)#A
q1_A[2,] <- c(0,0,0,0,1,0,0,1)#B
q1_A[3,] <- c(1,0,0,1,0,1,0,0)#C
q1_A[4,] <- c(0,0,1,0,1,0,1,0)#D
q1_A[5,] <- c(0,1,0,1,0,0,0,1)#E
q1_A[6,] <- c(1,0,1,0,0,0,1,0)#F
q1_A[7,] <- c(0,0,0,1,0,1,0,1)#G
q1_A[8,] <- c(0,1,0,0,1,0,1,0)#H

q1_A
q1_D <- matrix(data = 0, nrow = 8, ncol = 8)
diag(q1_D) <- rowSums(q1_A)
q1_D

q1_L <- q1_D - q1_A
colSums(q1_L); rowSums(q1_L)

#### questions:
The sum of the entries of A is 22.
sum(q1_A) # 22 <- right answer
The sum of the entries of A is 8.
sum(q1_A) # 22
A has 16 nonzero entries.
sum(q1_A != 0) # 22
D has 16 nonzero entries.
sum(q1_D != 0) # 8

# Q2

You are given the following graph.
2 ----6
/  \    |
    1    4   |
    \  /  \ |
    3      5 
The goal is to find two clusters in this graph using Spectral Clustering on the Laplacian matrix. Compute the Laplacian of this graph. Then compute the second eigen vector of the Laplacian (the one corresponding to the second smallest eigenvalue).

To cluster the points, we decide to split at the mean value. We say that a node is a tie if its value in the eigen-vector is exactly equal to the mean value. Let's assume that if a point is a tie, we choose its cluster at random. Identify the true statement from the list below.'

q2_A <- matrix(0, 6, 6)
q2_A[1,c(2,3)] <- 1
q2_A[2,c(1,4,6)] <- 1
q2_A[3,c(1,4)] <- 1
q2_A[4,c(2,3,5)] <- 1
q2_A[5,c(4,6)] <- 1
q2_A[6,c(2,5)] <- 1

q2_D <- matrix(0,6,6)
diag(q2_D) <- rowSums(q2_A)
q2_L <- q2_D - q2_A

x_1 <- rep(1,dim(q2_D)[1])

q2_L %*% x_1

q2eigen <- eigen(q2_L) # look at the values, lambda_1 is the smallest, and lambda_2 second smallest
# second smallest is vector [,5]
x_2 <- round(q2eigen$vectors[,5],4)

# clustering is based on being close to another in x_2 vector.
# since x_2 is only 0.5, -0.5 or 0's in it. All the 0.5 are in one cluster,
# -0.5 in another cluster, and 0's are randomized as per exercise statement

1 and 3 in the same cluster # true
4 and 5 are in the same cluster # false
3 is a tie # false
2 and 5 are in different clusters # false


# Q3

We wish to estimate the surprise number (2nd moment) of a data stream, using the method of AMS. It happens that our stream consists of ten different values, which we'll call 1, 2,..., 10, that cycle repeatedly. That is, at timestamps 1 through 10, the element of the stream equals the timestamp, at timestamps 11 through 20, the element is the timestamp minus 10, and so on. It is now timestamp 75, and a 5 has just been read from the stream. As a start, you should calculate the surprise number for this time.
For our estimate of the surprise number, we shall choose three timestamps at random, and estimate the surprise number from each, using the AMS approach (length of the stream times 2m-1, where m is the number of occurrences of the element of the stream at that timestamp, considering all times from that timestamp on, to the current time). Then, our estimate will be the median of the three resulting values.

You should discover the simple rules that determine the estimate derived from any given timestamp and from any set of three timestamps. Then, identify from the list below the set of three "random" timestamps that give the closest estimate.'


q3_surprise <- function(n = 75) {
    count_vector <- rep(0,10)
    for (i in 1:n) {
        item <- i%%10 #i modulo 10
        # 10 modulo 10 = 0, but we want this to show as 10:
        if(item == 0)
            item <- 10
        count_vector[item] <- count_vector[item] +1
        
    }
    return(sum(count_vector^2))
}

ams <- function(a, n = 75) {
    count_vector <- rep(0,10)
    for (i in a:n) {
        item <- i%%10 #i modulo 10
        # 10 modulo 10 = 0, but we want this to show as 10:
        if(item == 0)
            item <- 10
        count_vector[item] <- count_vector[item] +1        
    }
    a <- a%%10
    if(a == 0)
        a <- 10    
    X <- n * ((2* count_vector[a])-1)
    return(X)
}


{9, 50, 68}
{14, 35, 42}
{31, 48, 50}
{3, 45, 72}
# apply ams function to each "random" number, take the median of those
# and check which one is the closest
q3_distance <- function(q_vector) {
    abs(median(sapply(q_vector,ams)) - q3_surprise())
}

q3_distance(c(9,50,68))
q3_distance(c(14,35,42))
q3_distance(c(31,48,50))
q3_distance(c(3,45,72)) # closest one

q3_distance(c(25,34,47))
q3_distance(c(22,42,62))
q3_distance(c(5,33,67))
q3_distance(c(31,48,50))




# Q4

We wish to use the Flagolet-Martin lgorithm of Section 4.4 to count the number of distinct elements in a stream. Suppose that there ten possible elements, 1, 2,..., 10, that could appear in the stream, but only four of them have actually appeared. To make our estimate of the count of distinct elements, we hash each element to a 4-bit binary number. The element x is hashed to 3x + 7 (modulo 11). For example, element 8 hashes to 3*8+7 = 31, which is 9 modulo 11 (i.e., the remainder of 31/11 is 9). Thus, the 4-bit string for element 8 is 1001.
A set of four of the elements 1 through 10 could give an estimate that is exact (if the estimate is 4), or too high, or too low. You should figure out under what circumstances a set of four elements falls into each of those categories. Then, identify in the list below the set of four elements that gives the exactly correct estimate.

{2, 6, 8, 9}
{1, 4, 7, 9}
{2, 4, 6, 10}
{ 2, 6, 8, 10}

#hashes the number, then returns how many 0's int he tail of the binary representation of the number
q4_hash <- function(x){
    hashvalue <- ((3*x+7)%%11)
    if(hashvalue%%2 != 0)
        return(0)
    if(hashvalue%%4 != 0)
        return(1)
    if(hashvalue%%8 != 0)
        return(2)
    return(3)
}

q4_1 <- c(2,6,8,9)
q4_2 <- c(1,4,7,9)
q4_3 <- c(2,4,6,10)
q4_4 <- c(2,6,8,10)

q4_1 <- c(2,3,6,9)
q4_2 <- c(1,6,7,10)
q4_3 <- c(2,5,7,10)
q4_4 <- c(4,5,6,7)


sapply(q4_1, q4_hash)
sapply(q4_2, q4_hash)
sapply(q4_3, q4_hash)
sapply(q4_4, q4_hash) # correct one, since max = 2 thus esimate 2^2 = 4

# estimate is 2^R, where R is the maximum of these sapplys.

# Q5





