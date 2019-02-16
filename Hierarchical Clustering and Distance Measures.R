# Hierarchical Clustering
# Computes based on n x p distance matrix
#   - Agglomerative vs divisive
#       - Agglomerative: n samples, compute distnace, merge two closest clusters. Repeat n-1 times
#   - Sensitive to: outlies, scaling, and how distances are computed 
#   - Distances: (represented by the length of line in the dendogram - dissimilarity measure)
#       1. Average - distance between clusters is avg distance between all pairs 
#                    Biased toward clusters with same variance - joins clusters with small variance
#       2. Single - distance between clusters is the min distnace over all pairs. 
#                   Elongated / irregular clusters because of chaining
#       3. Complete - distance between 2 clusters is the max distance over all pairs
#                    Crates equal sized clusters but is sensitive to outliers

eur = as.dist(read.csv("europe.csv"))
fit = hclust(eur, method="average") plot(fit, main="Average Linkage")
fit$merge

#   - Distances:
#       1. Numerical: account for differences in magnitude . r value determines how much weight to assign to dimensions with larger differences.
#           - Chevbychev: d = max(5-1, 4-1) = 4 
#           - Euclidean: d = sqrt((5-1)^2 + (4-1)^2) = 5
#           - City Block: d = |5-1| + |4-1| = 7 ; r=1 and assigns the least weight to large differences, and supreme norm (r= infinity) equals the largest differences, ignoring smaller differences completely.


library(proxy)
x = matrix(c(5,1,4,1), nrow=2)
x

dist(x) #euclidean
dist(x, "euclidean") #euclidean
dist(x, method="maximum") #chebychev
dist(x, method="manhattan") #city block

pr_DB$get_entry_names() # lists all distance metrics

#       2. Numerical - Angular: Consider distance without looking at magnitude
#             - Cosine: angular separation - evaluates angle centered at origin. 
#                       Better to use with sparce data (many 0's). Similar to Jaccard with 0-1 data. Does not consider co absenses.
#             - Pearson correlation: evaluates angle centered at point of means.  When people have systematic biases that shift the magnitude of their measures, it helps to mean center (ipsatize) the rows.

# Example comparing cosine vs pearson distances
X = matrix(c(1,2,3,10, 11, 12), nrow=2) # cosine and pearson give different result
simil(X, "cosine") #cosine=0.9098763
simil(X) # Pearson=0.7857143

X = matrix(c(1,2,3,10, 20, 30), nrow=2) # multiples - cosine and pearson give same result(=0.9829169)
simil(X, "cosine") 
simil(X) # Pearson

X = matrix(c(1,2,3,10,15,20), nrow=2) # addition - cosine and pearson give same result (=0.9646925)
simil(X, "cosine")
simil(X) # Pearson


cor(t(X)) # use corr function
dist(X, method="correlation") #distance = 1-similarity


#       3. Categorical: counts of binary outcomes for individuals across p variables 
#         - Jaccard: intersection of common yes/union. Does NOT consider coabsenses: might be high value and make co occurances negligible. 
#                    Has same numerator as cosine, different denominator.
#         - Matching intersection of common yes + intersection of common no / total items. Use this when coabsense information is informative.
# 


x = matrix(c( 1,1,1,1,0,0,0,0,0,0,0,0, 0,0,0,1,1,0,1,0,0,0,0,0, 0,1,1,0,1,0,1,0,1,1,1,0), byrow=T, ncol=12, dimnames=list(LETTERS[1:3]))
cor(t(x))

simil(x, method="cosine")
1-dist(x,method="cosine") #same as similarity

simil(x, method="Jaccard")
simil(x, method="Pearson") # If 0 means unknown, pearson will be different
