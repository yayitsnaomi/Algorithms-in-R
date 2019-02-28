# Market Basket Analysis
# Goal: identify item sets k that occur frequently together i.e. K = {peanut butter, jelly, bread}
# 
# 
# Each customer has basket of items, helpful to do the analysis at multiple levels of item aggregration 
#      Do not care about quantity, only if items is included in based or not (purchased or not)
#      Association rules account for quantity
#
# Variables:
#   |k| cardinality of k is the number of items in the set
#   P(k) is the probability of observing a set. Want high SUPPORT/prevelance of a set of items = % of baskets that contain the set.
#   n customers
#   p possible items
#   2^p possible item sets - can't compute support for all, unless p is small
# 
# Priori Algorithm: identifies high support item sets for large p. P(A & B) <= P(A). 
#                   Requires 1 pass through data for each value of |K|. Good computational speed even for large data sets.
#     1. Compute support of all single items sets; drop those with support < threshold
#     2. Compute support for 2 item sets, from surviving single set items, and drop those with support < threshold
#     3. Repete for 3, 4, etc item sets until maxlen is reached and there are no more remaining item sets
# 

library(apriori)

library(arules)
asc = read.transactions("teach/421/asc/asc2.dat")
summary(asc)

inspect(asc[1:2]) #items
itemFrequencyPlot(asc, support=.1)
sets = apriori(asc, parameter=list(target="freq"))
inspect(sets)


# Association rules are if-then statements that help to show the probability of relationships between data items within large data sets in various types of databases. Analyze patterns of cooccurance.
# Association Rules: C <- A (does buying A "cause" people to also buy C)
#   K - high support set of items. Partition k into 2 disjoint sets: antecedent (A) & consequence (C)
#   P(A) - Support comes from antecedent (A)
#   Conf(C <- A) = P(C|A) = P(A&C)/P(A) = confidence of a rule
# 

rules = apriori(asc, parameter=list(conf=.7, support=.1))
inspect(rules)

# LIFT = P(10 & 12)/ P(10) * P(12)
# LIFT= the ratio of observed joint probability / joint probability (assuming independence)
# LIFT = the ratio of the confidence of the rule and the expected confidence of the rule.
# The lift value is a measure of importance of a rule.
# By using rule filters, you can define the desired lift range in the settings. 
# 

# End to end example:

library(arules)
library(arulesViz)
asc = read.transactions("/Users/ecm/teach/421/asc/ascname10k.dat") summary(asc)
itemFrequencyPlot(asc, cex.names=.7)
# generate item sets. Note minlen and maxlen options
# Start with high support and low maxlen, then adjust!
# It is easy to freeze your computer!
sets = apriori(asc, parameter=list(target="freq", support=.1, minlen=2, maxlen=4)) # note the use of head and sort with the option n=50
inspect(head(sort(sets), n=50))

# network graph, circles show joint probabilities
plot(head(sort(sets), n=10), method="graph")
plot(sets, method="graph")

# cluster data
d <- dissimilarity(asc, method = "jaccard", which = "items") plot(hclust(d, method="complete"), cex=.6)
plot(hclust(d, method="average"), cex=.6)
d <- dissimilarity(asc, method = "cosine", which = "items") plot(hclust(d), cex=.6)

# generate association rules
rules = apriori(asc, parameter=list(conf=.7, support=.1, minlen=2, maxlen=4)) 
rules = apriori(asc, parameter=list(conf=.5, support=.05, minlen=2, maxlen=4)) 
rules
subrule = head(sort(rules, by="lift"), n=200)
inspect(head(sort(rules, by="lift"), n=20))
plot(rules, method="grouped", control=list(k=20))
plot(subrule, method="grouped", control=list(k=50))
plot(subrule, method="grouped", interactive=T)
plot(rules)
rm(asc, sets, rules, d) # clean up



