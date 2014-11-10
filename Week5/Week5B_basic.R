Question 1
We wish to cluster the following set of points:
    
    
    into 10 clusters. We initially choose each of the green points as a centroid. Assign each of the gold points to their nearest centroid. (Note: the scales of the horizontal and vertical axes differ, so you really need to apply the formula for distance of points; you can't just "eyeball" it.) Then, recompute the centroids of each of the clusters. Do any of the points then get reassigned to a new cluster on the next round? Identify the true statement in the list below. Each statement refers either to a centroid AFTER recomputation of centroids (precise to one decimal place) or to a point that gets reclassified.'
                                                                                                                                             
                                                                                                                                             There is a centroid after recomputation at (52.5,109.3)
# true                                                                                                                                             

There is a centroid after recomputation at (46,58.5)
# false

There is a centroid after recomputation at (29,97)
# false                                                                                                                                             

There is a centroid after recomputation at (50.3,116.3)
#false
                                                                                                                                             
                                                                                                                                             
                                                                                                                                             #k-means, where k=10, and the green points are original cluster centroids.
                                                                                                                                            # perform one round of updates
                                                                                                                                             
yellows <- c(28,145, 65,140, 50,130, 38,115, 55,118, 50,90, 63,88, 43,83, 50,60, 50,30)
greens <- c(25,125, 44, 105, 29,97, 35,63, 55,63, 42,57, 23,40, 64,37, 33,22, 55, 20)

yelmatrix <- matrix(yellows, nrow = 10, ncol = 2, byrow = T)
greenmatrix <- matrix(greens, nrow = 10, ncol = 2, byrow = T)

#distancefunction, calculates euclidian distance between 2 points
q1_calc_distance <- function(point1, point2) {
    delta_x <- point1[1] - point2[1]
    delta_y <- point1[2] - point2[2]
    euclidian_distance <- sqrt(delta_x^2 + delta_y^2)
    return(euclidian_distance)
}

q1_get_closest_centroid <- function(centroids, points) {
    number_of_points <- nrow(points)
    number_of_centroids <- nrow(centroids)
    new_centroids <- rep(NA, number_of_points)
    for(i in 1:number_of_points) {
        closest_centroid <- NULL
        distance_to_closest <- Inf
        for(j in 1:number_of_centroids) {
            distance_to_j <- q1_calc_distance(centroids[j,], points[i,])
            # update closest centroid & distance to clsoest, if this is the new closest one
            if(distance_to_j < distance_to_closest) {
                distance_to_closest <- distance_to_j
                closest_centroid <- j
            }                
        }
        new_centroids[i] <- closest_centroid
    }
    return(new_centroids)
}
# calculates which cluster the yellow points belong to
clusters_for_yellows <- q1_get_closest_centroid(greenmatrix, yelmatrix)
# combine all the points into one matrix
allpoints <- rbind(greenmatrix, yelmatrix)

# green points are the chosen as centroids, so each one will be in different cluster
greencentroids <- 1:10
# make a vector that links allpoints matrix into corresponding clustering
all_centroids <- c(greencentroids, clusters_for_yellows)

# function to calculate new centroid point locations given a matrix of 
# points and a vector that links those points to corresponding clusters
calculate_new_centroids <- function(points, centroid_numbers) {
    number_of_clusters <- max(centroid_numbers)
    number_of_points <- nrow(points)
    new_centroids <- NULL
    for(i in 1:number_of_clusters) {
        centroid_points <- NULL
        for(j in 1:number_of_points) {
            # is chosen point assigned to this cluster? if yes, add to centroid_points
            if(centroid_numbers[j] == i) {
                centroid_points <- rbind(centroid_points, points[j,])
            }
        }
        # now we have centroid_points that has all the points for this cluster
        # calculate the centroid simply by averaging the coordinate points
        new_center <- colMeans(centroid_points)
        new_centroids <- rbind(new_centroids, new_center)
    2}
    return(new_centroids)
}

new_clustering <- calculate_new_centroids(allpoints, all_centroids)

###

Question 2
                                                                                                                                             When performing a k-means clustering, success depends very much on the initially chosen points. Suppose that we choose two centroids (a,b) = (5,10) and (c,d) = (20,5), and the data truly belongs to two rectangular clusters, as suggested by the following diagram:
                                                                                                                                             
                                                                                                                                             
                                                                                                                                             Under what circumstances will the initial clustering be successful? That is, under what conditions will all the yellow points be assigned to the centroid (5,10), while all of the blue points are assigned to cluster (20,5))? Identify in the list below, a pair of rectangles (described by their upper left corner, UL, and their lower-right corner LR) that are successfully clustered.
                                                                                                                                             
# make strings from answers
q2_1 <- "Yellow: UL=(6,15) and LR=(13,7); Blue: UL=(16,16) and LR=(18,5)"                                                                                                                                           
q2_2 <- "Yellow: UL=(7,12) and LR=(12,8); Blue: UL=(16,19) and LR=(25,12)"                                                                                                                                             
q2_3 <- "Yellow: UL=(6,7) and LR=(11,4); Blue: UL=(11,5) and LR=(17,2)"                                                                                                                                             
q2_4 <- "Yellow: UL=(3,3) and LR=(10,1); Blue: UL=(15,14) and LR=(20,10)"
   
require(ggplot2)

# use regexp to get coordinates
getcoords <- function(x) {
    valitulos <- gregexpr("[0-9]+", x)
    keskitulos <- regmatches(x, valitulos)
    lopputulos <- as.numeric(keskitulos[[1]])
    #finalres <- data.frame(nrow = 4, ncol = 2, data = lopputulos, byrow = T)
    #endpoint <- length(lopputulos)
    #midpoint <- endpoint/2    
    #startofsecond <- midpoint+1

    # first row will be xmin,xmax for yellow cluster, 2nd row is ymin, ymax for yellow
    # 3rd and 4th rows are same but for blue cluster
    finalres <- data.frame(x = lopputulos[c(1,4,5,8)], y = lopputulos[c(3,2,7,6)])
    #ulx = upper left x-coordinate, lry = lower right y-coordinate
    #finalres <- data.frame( = lopputulos[c(1,5)], uly = lopputulos[c(2,6)],
    #                       lrx = lopputulos[c(3,7)], lry = lopputulos[c(4,8)])
    return(finalres)
}
# conver strings to coordinates
coords_1 <- getcoords(q2_1)
coords_2 <- getcoords(q2_2)
coords_3 <- getcoords(q2_3)
coords_4 <- getcoords(q2_4)

generate_points_and_choose_clustering <- function(coord_matrix, yellow_centroid = c(5,10),
                                                  blue_centroid = c(20,5), ss = 10000) {   
    # ss = sample size
    centroids <- rbind(yellow_centroid, blue_centroid)
    yellow_xcoord <- runif(ss, coord_matrix[1,1], coord_matrix[1,2])
    yellow_ycoord <- runif(ss, coord_matrix[2,1], coord_matrix[2,2])
    blue_xcoord <- runif(ss, coord_matrix[3,1], coord_matrix[3,2])
    blue_ycoord <- runif(ss, coord_matrix[4,1], coord_matrix[4,2])

    yellow_points <- cbind(yellow_xcoord, yellow_ycoord)
    blue_points <- cbind(blue_xcoord, blue_ycoord)

    yellow_closest <- q1_get_closest_centroid(centroids, yellow_points)
    blue_closest <- q1_get_closest_centroid(centroids, blue_points)
    cat("yellow points assigned to blue cluster:",sum(yellow_closest != 1),"\n")
    cat("blue points assigned to yellow cluster:",sum(blue_closest != 2),"\n")
}

# use the function to calculate how many points assigned to wrong cluster:
generate_points_and_choose_clustering(coords_1) # wrong
generate_points_and_choose_clustering(coords_2) # wrong
generate_points_and_choose_clustering(coords_3) # wrong
generate_points_and_choose_clustering(coords_4) # correct


#plotcoords <- function(coordmatrix) {
#    qplot(coordmatrix[,1], coordmatrix[,2], xlim = c(0,30), ylim = c(0,30))
#
#}
#ggplot(coords_1) + geom_point(aes(x,y))

plot_rectangle_and_centroids <- function(coord_df) {
    ggplot(coord_df) + geom_rect(aes(xmin = ulx, xmax = lrx,   ymin = lry, ymax = uly),   fill = "blue") +
        geom_point(aes(c(5,20),c(10,5)))    
}



plot_rectangle_and_centroids(coords_1)
plot_rectangle_and_centroids(coords_2)
plot_rectangle_and_centroids(coords_3)
plot_rectangle_and_centroids(coords_4)

Question 3
                                                                                                                                             Suppose we apply the BALANCE algorithm with bids of 0 or 1 only, to a situation where advertiser A bids on query words x and y, while advertiser B bids on query words x and z. Both have a budget of $2. Identify in the list below a sequence of four queries that will certainly be handled optimally by the algorithm.

# x for both, y for a only, z for b only

xyyz
# a,a,fail, optimal would be b,a,a,b
xzyz
# b,b,a,fail, optimal would be a,b,a,b
yzyy
# a,b,a,same as in optimal
xyyx
# a,a, fail, optimal would be b,a,a,b

Question 4
                                                                                                                                             The set cover problem is: given a list of sets, find a smallest collection of these sets such that every element in any of the sets is in at least one set of the collection. As we form a collection, we say an element is covered if it is in at least one set of the collection. Note: In this problem, we shall represent sets by concatenating their elements, without brackets or commas. For example, {A,B} will be represented simply as AB. There are many greedy algorithms that could be used to pick a collection of sets that is close to as small as possible. Here are some that you will consider in this problem. Dumb: Select sets for the collection in the order in which they appear on the list. Stop when all elements are covered. Simple: Consider sets in the order in which they appear on the list. When it is considered, select a set if it has at least one element that is not already covered. Stop when all elements are covered. Largest-First: Consider sets in order of their size. If there are ties, break the tie in favor of the one that appears first on the list. When it is considered, select a set if it has at least one element that is not already covered. Stop when all elements are covered. Most-Help: Consider sets in order of the number of elements they contain that are not already covered. If there are ties, break the tie in favor of the one that appears first on the list. Stop when all elements are covered. Here is a list of sets: AB, BC, CD, DE, EF, FG, GH, AH, ADG, ADF First, determine the optimum solution, that is, the fewest sets that can be selected for a collection that covers all eight elements A,B,...,H. Then, determine the sizes of the collections that will be constructed by each of the four algorithms mentioned above. Compute the ratio of the size returned by the algorithm to the optimum size, and identify one of these ratios in the list below, correct to two decimal places.
                                                                                                                                             
sets: AB, BC, CD, DE, EF, FG, GH, AH, ADG, ADF

one possible cover is AB, CD, EF, GH  = 4
this is indeed the optimal answer, because the sets of size 3 contain 2 common elements
#run
optimal <- 4

Dumb algo: AB, BC, CD, DE, EF, FG, GH = 7 
#run
dumb <- 7/optimal

Simple algo: AB, BC, CD, DE, EF, FG, GH = 7
#run
simple <- 7/optimal

Most-help algo: ADG, BC, EF, GH = 4
#run
most_help <- 4/optimal


The ratio for Most-Help is 1.00
most_help # correct
The ratio for Simple is 2.33
The ratio for Dumb is 1.50
The ratio for Dumb is 2.00
                                                                                                                                             Question 5
                                                                                                                                             This bipartite graph:
                                                                                                                                             
                                                                                                                                             
                                                                                                                                             Has several perfect matchings. Find all the perfect matchings and then identify, in the list below, a pair of edges that can appear together in a perfect matching.
               
pairs are:
    a0 -> b0, b1
    a1 -> b2, b3
    a2 -> b0, b4
    a3 -> b1, b2
    a4 -> b3, b4

q5_matrix <- matrix(c(1,1,0,0,0,
                      0,0,1,1,0, 
                      1,0,0,0,1,
                      0,1,1,0,0,
                      0,0,0,1,1), nrow = 5, ncol = 5, byrow = T)
colnames(q5_matrix) <- c("b0", "b1", "b2", "b3", "b4")
rownames(q5_matrix) <- c("a0", "a1", "a2", "a3", "a4")


a4-b3 and a2-b0
# cannot be right, because a4 and a2 are the only ones that map to b4
q5_1 <- q5_matrix
q5_1[c("a4","a2"),] <- 0
q5_1[,c("b3","b0")] <- 0
q5_1
###
a3-b2 and a4-b4
# would work, remaining are: a0-b1, a1-b3, a2-b0
q5_2 <- q5_matrix
q5_2[c("a3","a4"),] <- 0
q5_2[,c("b2","b4")] <- 0
q5_2
##
a2-b0 and a3-b1
# cannot be right, since a0 cannot map to anything after these


a3-b2 and a4-b3
# does not work, a1 cannot map to anythig
q5_4 <- q5_matrix
q5_4[c("a3","a4"),] <- 0
q5_4[,c("b2","b3")] <- 0
q5_4

