# Distances

library(proxy)

pr_DB$get_entry_names() # lists all available distance metrics which can be used in method

# Numerical distances or Minkowski metrics of the vectors
    
# Minkowski metrics account for magnitude
    
#   - euclidean - sqrt((x1-x2)^2 + (y1-y2)^2)
#   - Chebychev or maximum distance - maximum distance between any two points in the same axis - max(x1-x2,y1-y2)
#   - city-block or manhattan distance - abs(x1-x2) + abs(y1-y2)

# With Minkowski metrics, the value r determines how much weight to assign to dimensions with larger differences
#   - city-block metric (r = 1) assigns the least weight to large differences 
#   - euclidean metric (r = 2) 
#   - the supremum norm which is Chebychev(r = infinity) equals the largest difference, ignoring smaller differences completely.

X <- matrix(c(5,1,4,1),nrow = 2)

dist(X) # euclidean defualt
dist(X,method = "euclidean") # euclidean - sqrt((x1-x2)^2 + (y1-y2)^2)
dist(X, method = "maximum") # or Chebychev - maximum distance between any two points in the same axis - max(x1-x2,y1-y2)
dist(X, method = "manhattan") # or city-block distance - abs(x1-x2) + abs(y1-y2)

# Angular metrics
  
  # Angular measures focus on the the angle between the row vectors and ignore the magnitudes of the vectors

  # note: distance = 1 - similarity

  # Pearson similarity - evaluates angle centered at point of means
  # Cosine similarity - evaluates angle centered at origin - for 0/1 categorical data cosine is similar to Jaccard

  # Pearson correlation and cosine similarity are invariant to scaling, i.e. multiplying all elements by a nonzero constant 
  # Pearson correlation is also invariant to adding any constant to all elements
  # For example, if you have two vectors X1 and X2, and your Pearson correlation function is called pearson(), pearson(X1, X2) == pearson(X1, 2 * X2 + 3)
  # This is a pretty important property because you often don't care that two vectors are similar in absolute terms, only that they vary in the same way.
  
  # The difference between Pearson Correlation Coefficient and Cosine Similarity can be seen from their formulas:
  
  # The reason Pearson Correlation Coefficient is invariant to adding any constant is that the means are subtracted out by construction. 
  # It is also easy to see that Pearson Correlation Coefficient and Cosine Similarity are equivalent when X and Y have means of 0, so we can think of Pearson Correlation Coefficient as demeaned version of Cosine Similarity.

  # Example illustrating pearson and cosine

    X <- matrix(c(1,10,2,15,3,20), nrow=2) # these two points differ w.r.t to the constants added to each point
    
    simil(X) # Pearson similarity
    simil(X, "cosine") # cosine similarity
    
    dist(X, method="correlation") # pearson distance # distance = 1 - similarity
    dist(X, "cosine") # cosine distance # distance = 1 - similarity
    
    X <- matrix(c(1,10,2,20,3,30), nrow=2) # these two points differ as they are scaled differently
    
    simil(X) # Pearson # Pearson similarity
    simil(X, "cosine") # cosine similarity

    dist(X, method="correlation") # pearson distance # distance = 1 - similarity
    dist(X, "cosine") # cosine distance # distance = 1 - similarity
    
# Jaccard and Matching Coefficient

# Consider counts of binary outcomes for two individuals across p variables(confusion matrix)

#       A
#       Yes No
# B Yes  a   b
#   No   c   d

# where a+b+c+d = p

# Jaccard between A,B = a/(a+b+c)
# Matching coefficient between A,B = (a+d)/(a+b+c+d)

X <- matrix(c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,0,0,0,1,1,0,1,0,1,0,1,1,1,0),byrow = T,ncol = 12,dimnames = list(LETTERS[1:3]))    

simil(X,method = "cosine")
simil(X,method = "Jaccard") # note Jaccard and cosine gives similar results. but in all cases???
simil(X,method = "simple matching")

# use Matching coefficient when co-absences are informative
# use Jaccard when co-absences are not informative

# Conclusion
# Angular measures focus on the the angle between the row vectors and ignore the magnitudes of the vectors. Minkowski metrics account for magnitude.
# With Minkowski metrics, the value r determines how much weight to assign to dimensions with larger differences, where the city-block metric (r = 1) assigns the least weight to large differences, and the supremum norm (r = ???) equals the largest difference, ignoring smaller differences completely.
# When people (rows) have systematic biases that shift the magnitude of their measures, it often helps to mean center (ipsatize) the rows, which is what the Pearson correlation does.
# For binary data, use the matching coefficient when co-absenses are informative, and Jaccard or cosine when they are not.
