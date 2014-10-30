#Q1

# Suppose we hash the elements of a set S having 20 members, to a bit array of length 99. The array is initially all-0's, and we set a bit to 1 whenever a member of S hashes to it. The hash function is random and uniform in its distribution. What is the expected fraction of 0's in the array after hashing? What is the expected fraction of 1's? You may assume that 99 is large enough that asymptotic limits are reached.

#set size 20, hash to bit array of length 99.

# The fraction of 0's is e-20/99. <- right answer
# The fraction of 1's is e-20/99. 
# The fraction of 0's is 79/99.
# The fraction of 1's is 20/99.

# Q2


#A certain Web mail service (like gmail, e.g.) has 10^8 users, and wishes to create a sample of data about these users, occupying 10^10 bytes. Activity at the service can be viewed as a stream of elements, each of which is an email. The element contains the ID of the sender, which must be one of the 10^8 users of the service, and other information, e.g., the recipient(s), and contents of the message. The plan is to pick a subset of the users and collect in the 10^10 bytes records of length 100 bytes about every email sent by the users in the selected set (and nothing about other users).
#The method of Section 4.2.4 will be used. User ID's will be hashed to a bucket number, from 0 to 999,999. At all times, there will be a threshold t such that the 100-byte records for all the users whose ID's hash to t or less will be retained, and other users' records will not be retained. You may assume that each user generates emails at exactly the same rate as other users. As a function of n, the number of emails in the stream so far, what should the threshold t be in order that the selected records will not exceed the 10^10 bytes available to store records? From the list below, identify the true statement about a value of n and its value of t.


# n = 10^11, t = 1000
# n = 10^14, t = 0 <- correct I think
# n = 10^12, t = 999
# n = 10^9, t = 999


# 10^8 users, 10^6 buckets -> each bucket has 100 users, each "info" takes 100 bytes.

# n = 10^11, this would mean 10^11 emails in the system, each taking 100 bytes, so
# 10^13 bytes of information. We can store 10^10 bytes of memory, so we can only save
# 1/1000 of the emails -> t = 10^6*(1/1000) = 1000 <- this was incorrect answer

# n = 10^12, t = 999

# 10^12 emails. each takes 100 bytes so 10^14 bytes of memory total. Our memory capacity
# is 10^10

# this should return the proper result
tvalue <- function(n) {
    memory <- 10^10 #from assignment
    buckets <- 10^6 #from assignment
    users <- 10^8   #from assignment
    bytesneeded <- n*100    # each email takes 100 bytes
    compression_ratio <- memory/bytesneeded    
    compression_ratio*buckets -1 #gotta do -1 because buckets start from 0
}


