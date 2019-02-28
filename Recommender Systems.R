# Recommender Systems
# 
# Relevant Definitions:
#   Personalization - firm decides what is suitable for individual
#   Customization - customer proactively specifices elements 
#   Recommendation - tools provide suggestions for items to be of use to a user. Often top k lists.
# 
# Recommendation Variables:
#   User - getting recommendation. User set is all users in system. Active user is those we are making recommendations for.
#   Item - entity being recommended. Item set is all the items.
#   Rating - expression of preference - explicit vs implicit (inferred/observed). Rm*n matrix of each users(row) rating by item(column)
#   Prediction - estimate of preference rhat ui - for that user for a specific item
#   Content - known metadata about items
#   Collaborative - using data from others users
# 
# Problems with recommendation algorithms:
#   1. Item set |I| can be large, as can User set |U| --> computational time is a challenge
#   2. The Rating matrix is mostly sparce
#   3. Explicit vs implicit rating
#   4. Cold start problem - how to make recommendations without data? i.e. new user or new item (or both)
#   5. Context - what is the current state of the user? who, where, when, why, with whom?
# 
# Evaluation of recommendation algorithms:
#   1. Domain - what is being recommended. Do we want to recommend only new items, or re recommend items (i.e. grocery list)?
#   2. Purpose - goal of recommendation 
#                Sales: cross sell compliments, upsell higher margin items, recommend substitutes/alternatives, bundling
#                Browsing, stay engaged (movies, music), education (exploit - issue with news bubble vs explore - more long term relationship with customer)
#   3. Context - what is the active user doing at the time of recommendation?
#   4. Whose opinion - other users, experts, sponsored?
#   5. Personalization level
#         1. Generic/non personalized - all receive the same recommendations i.e. "popular", best sellers, high margin items
#         2. Demographic/subsegments - matches target group
#         3. Ephemeral - matches current activity i.e. matches items in shopping cart using association rules
#         4. Persistent - matches long term interests 
#   6. Privacy and trustworthiness
#   7. Interfaces
#   8  Algorithms
# 
# Recommendation Approaches: 
#   1. Baseline methods - not based on a users's rating (good for cold start methods - suggest random items or most popular items)
#         User effect/Bias: bu·= 1/|I| SUM􏰎(rui−r ̄)=ru·−r
#         Item effect/popularity: b·i = 1/|U| SUM􏰎(rui −bu· −r ̄)= 1 􏰎(rui −ru·)
#       *Gives damping versions that shrink the means -> 0 where users or items with few ratings are shrunk towards the grand means

rest = expand.grid(user=factor(1:6), item=LETTERS[1:4])
rest$r = c(70,77,76,80,84,78,61,75,67,63,66,68,82,88,90,96,92,98,74,76,80,76,84,86)
lm(r~user+item, rest)$coef

#   2. Content based filering (CBF) -  makes predictions based on similarity in metadata of items i.e. horror movies. 
#           Problem: how much weight to give to each attribute
#           Requires: set of attributes, item profiles (from text mining or manual collection), and user profiles (explicit or implicit) -> which are good when the user is not looking for a specific item, otherwise let them search for a specific item
#           2 Approaches: 
#               1. similarity (more common) - recommend items similar to that user profile
#                       i.e. weighted average of the attribute values across all items with an expressed preference (sum attribute * preference/sum preferences)
#               2. classification - for each user "regress" known utilities on item attributes. unit of analysis = item
#                       - need to use this when there are interactions, because similarity will fail 
#                       - ex. fit a tree, can also fit regression when there are no interactions 
#                       A.) Conjoint analysis - looks like regression
#                           - Utility: o =erall utility from product i
#                           - Bj = the weight associated with attribute j
#  (estimated by inferring utility of individual attributes from overal evaluations of the product)                          - xij th =e amount of the attribute j posessed by product i
#
#   3. Collaborative Filtering (CF) - makes predictions and recommendations based on other users that are similar 
#             i.e. if they agree on quality or relevance of some items, they might be more likely to agree on other items
#   4. Decompositional methods - Assume every rating is a linear combo of a small # of unobserved factors. Use factors to estimate missing ratings. 
#   5. Hybrid systems - combine multiple algorithms i.e. use content based for new items, use baseline for new users, CF for established users
# 
#  
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 








