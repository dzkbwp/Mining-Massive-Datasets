# Q1

Here is a table of 1-5 star ratings for five movies (M, N, P. Q. R) by three raters (A, B, C).
M    N	P	Q	R
A	1	2	3	4	5
B	2	3	2	5	3
C	5	5	5	3	2
Normalize the ratings by subtracting the average for each row and then subtracting the average for each column in the resulting table. Then, identify the true statement about the normalized table.

The entry (A,Q) is -3. # it is +3
The smallest element is (C,R). # smallest is (A,N) and (C,R)
The smallest element is (B,P). 
There are more -1's than 0's. # 3 of both, so wrong

# data:
q1_U <- matrix(c(1,2,3,4,5,2,3,2,5,3,5,5,5,3,2), nrow = 3, ncol = 5, byrow = T)
# normalize:
q1_norm <- q1_U - rowMeans(q1_U)
q1_norm


# Q2

Below is a table giving the profile of three items.
A    1	0	1	0	1	2
B	1	1	0	0	1	6
C	0	1	0	1	0	2
The first five attributes are Boolean, and the last is an integer "rating." Assume that the scale factor for the rating is alpha. Compute, as a function of alpha, the cosine distances between each pair of profiles. For each of alpha = 0, 0.5, 1, and 2, determine the cosine of the angle between each pair of 
vectors. Which of the following is FALSE?


#data
q2_U <- matrix(c(1,0,1,0,1,2,1,1,0,0,1,6,0,1,0,1,0,2), nrow = 3, ncol = 6, byrow = T)

dist <- function(row1, row2, alpha) {
    #multiply last column with alpha
    row1[length(row1)] <- alpha * row1[length(row1)]    
    row2[length(row2)] <- alpha * row2[length(row2)]    
    dotproduct <- row1 %*% row2
    sum_of_lengths <- sqrt(sum((row1)^2)) * sqrt(sum((row2)^2))
    return(dotproduct/sum_of_lengths)
}

#rows
rA <- q2_U[1,]
rB <- q2_U[2,]
rC <- q2_U[3,]

#answers
For alpha = 1, C is closer to B than A is.
dist(rC, rB, 1) ; dist(rA, rB, 1)

For alpha = 2, C is closer to B than A is.
dist(rC, rB, 2) ; dist(rA, rB, 2)

For alpha = 1, A is closer to B than C is.
dist(rA, rB, 1) ; dist(rC, rB, 1)

For alpha = 0.5, A is closer to B than C is.
dist(rA, rB, 0.5) ; dist(rC, rB, 0.5)

## try2

For alpha = 1, B is closer to A than C is.
dist(rB, rA, 1) ; dist(rC, rA, 1)

For alpha = 1, A is closer to B than C is.
dist(rA, rB, 1) ; dist(rC, rB, 1)

For alpha = 0, A is closer to B than C is.
dist(rA, rB, 0) ; dist(rC, rB, 0)

For alpha = 1, B is closer to C than A is.
dist(rB, rC, 1) ; dist(rA, rB, 1)


