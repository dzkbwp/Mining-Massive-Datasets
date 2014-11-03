# Q1

Question 1
Note: In this question, all columns will be written in their transposed form, as rows, to make the typography simpler. Matrix M has three rows and two columns, and the columns form an orthonormal basis. One of the columns is [2/7,3/7,6/7]. There are many options for the second column [x,y,z]. Write down those constraints on x, y, and z. Then, identi fy in the list below the one column that could be [x,y,z]. All components are computed to three decimal places, so the constraints may be satisfied only to a close approximation.
[-.937, .312, .156]
[1.125, .500, -.625]
[-.857, .286, .429]
[.890, -.346, -.297]

q1_vec1 <- c(2/7,3/7,6/7)

q1_1 <- c(-.937, .312, .156)
q1_2 <- c(1.125, .500, -.625)
q1_3 <- c(-.857, .286, .429)
q1_4 <- c(.890, -.346, -.297)

# has to be 0 to be orthogonal
q1_vec1 %*% q1_1
q1_vec1 %*% q1_2
q1_vec1 %*% q1_3
q1_vec1 %*% q1_4

# has to be 1 to be unitvector
sqrt(sum(q1_vec1^2))
sqrt(sum(q1_1^2))
sqrt(sum(q1_2^2))
sqrt(sum(q1_3^2))
sqrt(sum(q1_4^2))



# Q4

Find, in the list below, the vector that is orthogonal to the vector [1,2,3]. Note: the interesting concept regarding eigenvectors is "orthonormal," that is unit vectors that are orthogonal. However, this question avoids using unit vectors to make the calculations simpler.
[1, 0, 0]
[-3, 4, -2]
[-1, -2, -3]
[1, -2, 1]

q4_vec1 <- c(1,2,3)

q4_1 <- c(1,0,0)
q4_2 <- c(-3,4,-2)
q4_3 <- c(-1,-2,-3)
q4_4 <- c(1,-2,1)

q4_vec1 %*% q4_1
q4_vec1 %*% q4_2
q4_vec1 %*% q4_3
q4_vec1 %*% q4_4 # right answer
