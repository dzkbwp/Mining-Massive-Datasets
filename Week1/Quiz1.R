# Q1

#Consider three Web pages with the following links:

#    A -> B, A -> C
#    B -> C
#    c -> C
    
    
#Suppose we compute PageRank with a Beta of 0.7, and we introduce the additional constraint that the sum of the PageRanks of the three pages must be 3, to handle the problem that otherwise any multiple of a solution will also be a solution. Compute the PageRanks a, b, and c of the three pages A, B, and C, respectively. Then, identify from the list below, the true statement.

# Transition matrix without teleport
#      A   B   C
#M = [ 0   0   0] A
#    [ 0.5 0   0] B
#    [ 0.5 1   1] C

# Teleportmatrix
#t = [1/3 1/3 1/3]
#    [1/3 1/3 1/3]
#    [1/3 1/3 1/3]

## code:
Mdata <- c(0,0,0,0.5,0,0,0.5,1,1)
M <- matrix(Mdata, 3, 3, byrow = T)

tdata <- rep(1/3,9)
t <- matrix(tdata, 3, 3)

beta <- 0.7

# add teleport and non-teleport together 
combined <- beta*M + (1-beta) * t


PowerIteration <- function(matrixM, initialvalue = NULL, epsilon = 0.005, maxIterations = 1000) {
    matrixDim <- dim(matrixM)[1]
    # initialize start point to vector of 1/n's
    if (is.null(initialvalue)) {
        initialvalue <- rep(1/matrixDim, matrixDim)
    }

    # initialize
    current_r <- initialvalue
    old_r <- 99999999999
    n <- 1
    
    # loop until convergence happens or too many iterations
    while ( sum(abs(current_r - old_r)) > epsilon && n <= maxIterations) {
        old_r <- current_r
        current_r <- matrixM %*% current_r
        n <- n+1
    }
    
    return(current_r)    
}

converged_r <- PowerIteration(combined)
# finally have to scale by 3x due to scaling factor A, B, C
final_r <- converged_r*3


# Q2

#Consider three Web pages with the following links:
 
#    A -> B, A -> C
#    B -> C
#    C -> A
   
#Suppose we compute PageRank with Beta =0.85. Write the equations for the PageRanks a, b, and c of the three pages A, B, and C, respectively. Then, identify in the list below, one of the equations.

# without beta:
# r_a = r_c
# r_b = 0.5 * r_a
# r_c = r_b + 0.5 * r_a

# with beta:

#a = beta * (c)          + (1- beta) * 1/3
#b = beta * (0.5*a)      + (1- beta) * 1/3
#c = beta * (b + 0.5*a)  + (1- beta) * 1/3

# Matrix:
#     A   B  C
# A [ 0   0  1]
# B [ 0.5 0  0]
# C [ 0.5 1  0]

q2data <- c(0,0.5,0.5,0,0,1,1,0,0)
q2M <- matrix(q2data, 3, 3, byrow = F)
betaq2 <- 0.85

# same t-matrix as in Q3, t is 3x3 matrix with 1/3's in every entry

q2Combined <- q2M * betaq2 + (1- betaq2)* t 

q2converged <- PowerIteration(q2Combined)

a <- q2converged[1]; b <- q2converged[2]; c <- q2converged[3]

# which is correct?
#a = .9c + .05b
a; .9*c + .05*b
#b = .575a + .15c
b; .575*a + .15*c
#.95b = .475a + .05c
.95*b; .475 * a + .05*c # <--- correct answer
#.85b = .575a + .15c
.85*b; .575*a + .15*c


# Q3

# Consider three Web pages with the following links:
    
#   A -> B, A -> C
#   B -> C
#   c -> A
    
# Assuming no "taxation," compute the PageRanks a, b, and c of the three pages A, B, and C, using iteration, starting with the "0th" iteration where all three pages have rank a = b = c = 1. Compute as far as the 5th iteration, and also determine what the PageRanks are in the limit. Then, identify the true statement from the list below.

q3data <- c(0,0.5,0.5,0,0,1,1,0,0)
q3M <- matrix(q3data, 3, 3, byrow = F)
q3initialvalue <- rep(1, 3)

q3test <- PowerIteration(q3M, initialvalue = q3initialvalue, maxIterations = 999999)
q3test

# in the limit b = 3/5


##### Q4

#Suppose our input data to a map-reduce operation consists of integer values (the keys are not important). The map function takes an integer i and produces the list of pairs (p,i) such that p is a prime divisor of i. For example, map(12) = [(2,12), (3,12)].
#The reduce function is addition. That is, reduce(p, [i1, i2, ...,ik]) is (p,i1+i2+...+ik).

#Compute the output, if the input is the set of integers 15, 21, 24, 30, 49. Then, identify, in the list below, one of the pairs in the output.

# stolen from rosettacode:
findfactors <- function(n) {
    d <- c()
    div <- 2; nxt <- 3; rest <- n
    while( rest != 1 ) {
        while( rest%%div == 0 ) {
            d <- c(d, div)
            rest <- floor(rest / div)
        }
        div <- nxt
        nxt <- nxt + 2
    }
    d
}
require(plyr)
q4map <- function(integer) {
    returnframe <- data.frame(NULL)    
    primedivisors <- unique(findfactors(integer))
    for (i in primedivisors) {
        returnframe <- rbind(c(i, integer), returnframe)    
    }
    names(returnframe) <- c("value", "key")
    return(returnframe)
}
q4allreduce <- function(inputframe, uniqvalues = NULL) {
    if (is.null(uniqvalues)) {
        uniqvalues <- unique(inputframe[,1])
    }
    
    returnresults <- data.frame(NULL)
    for (i in uniqvalues) {
        subsettedframe <- inputframe[inputframe$value == i,]
        addtoresults <- q4reduce(subsettedframe)
        returnresults <- rbind(returnresults, addtoresults)
    }
    names(returnresults) <- c("value", "key")
    return(returnresults)
}
q4reduce <- function(inputframe) {
    summa <- sum(inputframe[,2])
    returnme <- c(inputframe[1,1], summa)
    return(returnme)
}



inputs <- c(15, 21, 24, 30, 49)

allmapped <- data.frame(NULL)
for (i in inputs) {
    newresult <- q4map(i)
    allmapped <- rbind(allmapped, newresult)
}

sortedq4 <- arrange(allmapped, desc(value))

q4uniqvalues <- unique(sortedq4[,1]) 

# results
q4allreduce(sortedq4)

