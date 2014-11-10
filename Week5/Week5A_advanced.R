Question 1
Consider the diagonal matrix M =
    1    0	0
0	2	0
0	0	0
. Compute its Moore-Penrose pseudoinverse, and then identify, in the list below, the true statement about the elements of the pseudoinverse.
There is one element with value 1/4.
There is one element with value 0.
There are seven elements with value 0.
There is one element with value -2.

# Q1

M <- matrix(c(1,0,0,
              0,2,0,
              0,0,0), nrow = 3, ncol = 3, byrow = T)

# load package that has pseudo inverse function
require(MASS)
ginv(M)




Question 2
An ad publisher selects three ads to place on each page, in order from the top. Click-through rates (CTR's) at each position differ for each advertiser, and each advertiser has a different CTR for each position. Each advertiser bids for click-throughs, and each advertiser has a daily budget, which may not be exceeded. When a click-through occurs, the advertiser pays the amount they bid. In one day, there are 101 click-throughs to be auctioned.
                                                                                                     Here is a table of the bids, CTR's for positions 1, 2, and 3, and budget for each advertiser.
                                                                                                     
                                                                                                     Advertiser	Bid	CTR1	CTR2	CTR3	Budget
A	$.10	.015	.010	.005	$1
B	$.09	.016	.012	.006	$2
C	$.08	.017	.014	.007	$3
D	$.07	.018	.015	.008	$4
E	$.06	.019	.016	.010	$5

The publisher uses the following strategy to allocate the three ad slots:
                                                                                                         
                                                                                                         Any advertiser whose budget is spent is ignored in what follows.
                                                                                                     The first slot goes to the advertiser whose expected yield for the first slot (product of the bid and the CTR for the first slot) is the greatest. This advertiser is ignored in what follows.
                                                                                                     The second slot goes to the advertiser whose expected yield for the second slot (product of the bid and the CTR for the second slot) is the greatest. This advertiser is ignored in what follows.
                                                                                                     The third slot goes to the advertiser whose expected yield for the third slot (product of the bid and the CTR for the third slot) is the greatest.
                                                                                                     The same three advertisers get the three ad positions until one of two things happens:
                                                                                                         
                                                                                                         An advertiser runs out of budget, or
                                                                                                     All 101 click-throughs have been obtained.
                                                                                                     Either of these events ends one phase of the allocation. If a phase ends because an advertiser ran out of budget, then they are assumed to get all the clicks their budget buys. During the same phase, we calculate the number of click-throughs received by the other two advertisers by assuming that all three received click-throughs in proportion to their respective CTR's for their positions (round to the nearest integer). If click-throughs remain, the publisher reallocates all three slots and starts a new phase.
                                                                                                     
                                                                                                     If the phase ends because all click-throughs have been allocated, assume that the three advertisers received click-throughs in proportion to their respective CTR's (again, rounding if necessary).
                                                                                                     
                                                                                                     Your task is to simulate the allocation of slots and to determine how many click-throughs each of the five advertisers get.
                                                                                                     
E gets 35 click-throughs.
D gets 7 click-throughs.
C gets 37 click-throughs.
E gets 19 click-throughs.
                                                                                                     
#

Advertiser    Bid	CTR1	CTR2	CTR3	Budget
A	$.10	.015	.010	.005	$1
B	$.09	.016	.012	.006	$2
C	$.08	.017	.014	.007	$3
D	$.07	.018	.015	.008	$4
E	$.06	.019	.016	.010	$5

q2_matrix <- matrix(c(.10, .015, .010, .005, 1,
                      .09, .015, .012, .006, 2,
                      .08, .017, .014, .007, 3,
                      .07, .018, .015, .008, 4,
                      .06, .019, .016, .010, 5),
                    nrow = 5, ncol = 5, byrow = T)
colnames(q2_matrix) <- c("Bid", "CTR1", "CTR2", "CTR3", "Budget")
rownames(q2_matrix) <- c("A", "B", "C", "D", "E")
q2_df <- data.frame(q2_matrix)

ctr_df <- q2_df[,2:4]
bid_vector <- q2_df[,1]
budget_vector <- q2_df[,5]
ev_df <- ctr_df * bid_vector
# check which advertisers have highest ev for CTR1-3
wins_bid  <- apply(ev_df, 2, which.max)

# time to run out of money
1/(ctr_df*bid_vector*(1/budget_vector))

# A runs out of money first after 667 pageviews
667*q2_df["A","CTR1"]*q2_df["A","Bid"] #should equal to A's budget






Question 3
                                                                                                     In certain clustering algorithms, such as CURE, we need to pick a representative set of points in a supposed cluster, and these points should be as far away from each other as possible. That is, begin with the two furthest points, and at each step add the point whose minimum distance to any of the previously selected points is maximum.
                                                                                                     Suppose you are given the following points in two-dimensional Euclidean space: x = (0,0); y = (10,10), a = (1,6); b = (3,7); c = (4,3); d = (7,7), e = (8,2); f = (9,5). Obviously, x and y are furthest apart, so start with these. You must add five more points, which we shall refer to as the first, second,..., fifth points in what follows. The distance measure is the normal Euclidean L2-norm. Which of the following is true about the order in which the five points are added?
                                                                                                     
                                                                                                     d is added third
                                                                                                     d is added fourth
                                                                                                     f is added second
                                                                                                     e is added third