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


# Q2

Note: in the question, all columns will be written in their transposed form, as rows, to make the typography simpler. Matrix M has three rows and three columns, and the columns form an orthonormal basis. One of the columns is [2/7, 3/7, 6/7], and another is [6/7, 3/7, -3/7]. Let the third column be [x,y,z]. Since the length of the vector [x,y,z] must be 1, there is a constraint that x^2 + y^2 + z^2 = 1. However, there are other constraints, and these other constraints can be used to deduce facts about the ratios among x, y, and z. Compute these ratios, and then identify one of them in the list below.

q2_col1 <- c(2/7, 3/7, 6/7)
q2_col2 <- c(6/7, 2/7, -3/7)

# has to be 1 to be unitvector
sqrt(sum(q2_col1^2))
sqrt(sum(q2_col2^2))

# has to be 0 to be orthogonal
q2_col1 %*% q2_col2
# thus
2/7*x + 3/7*y + 6/7*z = 0
6/7*x + 2/7*y - 3/7*z = 0
# multiply by 7
2x + 3y + 6z = 0
6x + 2y - 3z = 0
# multiply second by 2
2x + 3y + 6z = 0
12x + 4y - 6z = 0
# sum
14x + 7y = 0
# div by 7 and redistribute
2x = -y ### final
# insert into first one
-y + 3y + 6z = 0
2y = -6z
y = -3z ### final
# insert into second one
6x -4x -3z = 0
2x = 3z ### final


# Q3

Suppose we have three points in a two dimensional space: (1,1), (2,2) and (3,4). We want to perform PCA on these points, so we construct a 2-by-2 matrix whose eigenvectors are the directions that best represent these three points. Construct this matrix and identify, in the list below, one of its elements.


q3_1 <- c(1,1)
q3_2 <- c(2,2)
q3_3 <- c(3,4)

M <- rbind(q3_1, q3_2, q3_3)

squareMatrix <- t(M) %*% M


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
