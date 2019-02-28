# market basket analysis

library(arules)

# goal of market basket analysis: identify item sets K of items that occur frequently together
#   - e.g., K ={peanut butter,jelly,bread}
# the support (or prevalence) of a set of items is the percentage of baskets that contain the set
#   - i.e., the probability of observing the set P(K).
# the size of K is the number of items in the set, i.e. |K|


# priori algorithm

# we wish to identify item sets K1,K2, . . . that have "large" support. 
# item sets with small support are more sensitive to sampling variation and less actionable
# there are 2p possible item sets and we cannot compute support for each unless p is small
# the a priori algorithm identifies high-support item sets for large p, P(A intersection B) <= P(A) and P(A intersection B) <= P(B) in a series of steps
# priori algorithm is a mad shit algorithm without which market basket analysis will be computationally difficult

# CODE NOT COMPLETE OR CHECKED

# create a basket for 100 items, numbered 1 to 100, and also 100 baskets,also numbered 1 to 100

basket <- matrix(0L, 100, 100)
for (i in 1:100){
  for (j in 1:100){
    if (i %% j == 0){
      basket[i,j] <- 1
    }
  }
}

basket_sets = apriori(basket, parameter=list(target="freq", support=.05, maxlen=1))
inspect(sort(basket_sets))

#Warning message:
#  In apriori(basket, parameter = list(target = "freq", support = 0.05,  :
#   Mining stopped (maxlen reached). Only patterns up to a length of 1 returned!

# - this warning is bad because the algorithm did'n run fully
# - also maxlen = 1 is not so useful

basket_sets2 = apriori(basket, parameter=list(target="freq", support=.1, minlen=2))
inspect(sort(basket_sets2))

# not important - navigating and using baskets

  sum(basket)
  which(rowSums(basket)==max(rowSums(basket)), arr.ind = TRUE)
  sets_df = inspect(apriori(basket, parameter=list(target="freq", support=0.00001)))
  sets_df[which(sets_df$items=='{5,7}'),]
  sets_df[which(sets_df$items=='{2,5,7}'),]
  0.01/0.02
  sets_df[which(sets_df$items=='{2,3,4}'),]
  sets_df[which(sets_df$items=='{2,3,4,5}'),]

# generate item sets. Note minlen and maxlen options
# Start with high support and low maxlen, then adjust!
# It is easy to freeze your computer!
  
  sets = apriori(basket, parameter=list(target="freq", support=.1, minlen=2))
  
# note the use of head and sort with the option n=50
  
  inspect(head(sort(sets), n=50))
  
# network graph, circles show joint probabilities
  
  plot(head(sort(sets), n=10), method="graph")
  plot(sets, method="graph")
  
# cluster data
  
  d <- dissimilarity(basket, method = "jaccard")
  plot(hclust(d, method="complete"), cex=.6)
  plot(hclust(d, method="average"), cex=.6)
  d <- dissimilarity(basket, method = "cosine")
  plot(hclust(d), cex=.6)
  
  
# association rules

# we seek a set of association rules for the baskets
#   - Let K be a high-support set of items
#   - Partition the items in K into two disjoint sets A, the "antecedent," and C, the "consequence," (A union C = K). 
#   - For example, if A = {peanut butter, jelly} and C = {bread} then we will study whether buying peanut butter and jelly "causes" people to buy bread also.
#   - An association Rule is written C <- A.

# the confidence of a rule is given by
#   - Conf(C <- A) = P(C|A) =   P(A intersection C)/P(A)
#   - For example, suppose that 5% of all baskets contain peanut butter and jelly, and that 4% have peanut butter, jelly, and bread. 
#   - Then, the rule {bread} <- {peanut butter, jelly} has support 5% and confidence 80%.
  
# the lift value is a measure of importance of a rule
# the lift value of an association rule is the ratio of the confidence of the rule and the expected confidence of the rule
# lift is the ratio of observed joint probability to the joint probabilty assuming independence
  
library(arules)
library(arulesViz)

# generate association rules

rules = apriori(basket, parameter=list(conf=.7, support=.1, minlen=2))
rules = apriori(basket, parameter=list(conf=.5, support=.05, minlen=2)) # add target = "freq" parameter to just find all baskets and support without the rules 
inspect(rules)

# understanding and plotting rules

subrule = head(sort(rules, by="lift"), n=200)
inspect(head(sort(rules, by="lift"), n=20))
plot(rules, method="grouped", control=list(k=20))
plot(subrule, method="grouped", control=list(k=50))
plot(subrule, method="grouped", interactive=T)
plot(rules)
rm(asc, sets, rules, d) # clean up



#####

# Solve this

The data set tv.dat has data on which of p = 139 TV programs n = 2366 households
watch \regularly." Every household has a \basket" of programs that they watch. One
must watch more than 2 hours of the show over a 9 month period to be considered
a viewer. I want to give you practice using the arules package in R. Read it into R
using read.transactions. Hint: see the ASC example in the course packet.
(a) Which TV program is most popular in that it has highest support? What is its
support?
  (b) Generate item sets with minlen=2 and maxlen=4. Select an appropriate minimum
support value. Which two-item set has the largest support? What is the support?
  (c) Which three-item set has the largest support? What is the support?
  (d) Create a method="graph" image showing the relationships between the top 100
item sets.
(e) Find the Jaccard distance between the shows using the dissimilarity function
and the which="items" option. Use hierarchical cluster with the average method.
Submit the dendrogram.
(f) What does watching Jimmy Kimmel go with? If you know I watch this show,
what would you recommend to me?