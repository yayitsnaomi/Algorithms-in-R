
library(stringr) #Regular expression leveraged in feature engineering
library(dplyr) #data transformation 
library(tidyr) #data transformation 

## Kmeans


#  Cluster analysis steps

  # variable constrction/feature engineering
  # consider initial splits
  # variable/feature selection - good clusters are actionalble
  # pick clustering method
      # kmeans - numerical variables only
      # hierarchical - does not work on giant data sets
      # mixture models and latent class: handles numerical and/or categorical variables
  # transform variables if necessary
      # symmetrize and standardize or use quantile method 
      # transform always if units are incommensurate or if distribution is skewed
  # Find various cluster solutions, profile them, and pick one


# fit kmeans model

fit = kmeans(cluster_data,centers = 5,iter.max = 1000, nstart = 100)

fit$cluster # which cluster group is an item in
fit$size # number of items in each cluster
fit$centers # means of each cluster
fit$withinss # sum of squares within each cluster
(fit$withinss)/(fit$size * ncol(cluster_data)) # variance within each cluster
sqrt((fit$withinss)/(fit$size * ncol(cluster_data))) # standard deviation within each cluster

#elbow plot - figure out optimal K to maximize SSB and minimize SSE

i = 1
diff_ncluster = c(3,4,5,6,7,8,9,10,11,12)
withinss <- vector(mode = 'numeric', length = length(diff_ncluster))
inbess <- vector(mode = 'numeric', length = length(diff_ncluster))
for (K in diff_ncluster){
  fit <- kmeans(ent2, centers = K, nstart=100, iter.max = 100)
  withinss[i] <- fit$tot.withinss
  inbess[i] <- fit$betweenss
  i = i+1
}
plot(diff_ncluster, withinss, type = 'b', xlab = '#clusters', ylab = '', ylim = c(0,max(inbess)), col = 'red')
lines(diff_ncluster, inbess, type = 'b', col = 'blue')
legend("center", legend=c("withinss", "betweenss"),
       col=c("red", "blue"), lty = 1, cex=1)


#Violin Plot - look more meaingful clusters, where multiple dots in each cluster are in different places across cluters
plot.kmeans = function(fit,boxplot=F) {
  require(lattice)
  p = ncol(fit$centers) 
  k = nrow(fit$centers) 
  plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
  )
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){ panel.dotplot(...) 
                  panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1), xlab="Cluster Mean"
  )) 
  invisible(plotdat) }


# kmeans summary function with SSE, R^2, and Pseudo F
summary.kmeans = function(fit) {
  # number of x features
  p = ncol(fit$centers)
  # number of clusters
  k = nrow(fit$centers)
  # number of observations need to be classified into clusters
  n = sum(fit$size)
  # sum of square within clusters 
  sse = sum(fit$withinss)
  # weighted mean --> grand mean of the dataset
  xbar = t(fit$centers)%*%fit$size/n
  # sum of square between clusters
  ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2)
  print(data.frame(
    n=c(fit$size, n),
    # percentage of obs in this cluster
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*(fit$size-1)), sse/(p*(n-k)))), 4)
  ))
  cat("SSE = ", sse, "; SSB = ", ssb, "\n")
  cat("R-Squared = ", ssb/(ssb+sse), "\n")
  cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
  invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}

summary.kmeans(fit)

  # Pseudo F - if we assume that "natural groupings" means homogeneous within and heterogeneous across, 
  # we can evaluate a clust solution with pseudo F
  # F = (SSB/[p(k-1)])/(SSE/[p(n-k)])
  
  # If a cluster solution "fits the data," the between-cluster variance will be large, 
      # the within-cluster variance will be small, and the pseudo F will spike.
  # Even if there are no spikes, the solution might still be interesting. Then use judgment to pick one. 
      # Caution: when there is no spike, the solution may be very sensitive to sampling variation, starting values, etc.
  # You may also look for where SSE or R2 flattens out.



plot.kmeans(fit)


## another cluster visualization - good for plotting and visualizing two variables ata time from a cluster
fviz_cluster(fit, geom = "point", data=cluster_data, xlab = "Desktop", ylab = "Mobile")

# standardize columns (often necessary before clustering using kmeans)
scale(cluster_data)


#Example:
#Read in new zaneville data
z1 <- read.csv("zanesville1.csv",stringsAsFactors = FALSE)
z2 <- read.csv("zanesville2.csv",stringsAsFactors = FALSE)
z3 <- read.csv("zanesville3.csv",stringsAsFactors = FALSE)
z4 <- read.csv("zanesville4.csv",stringsAsFactors = FALSE)
#View(z1)

#consolidate into single df with required columns
z <-  rbind(z1[,c("fire_fly_id", "section","content_type", "sub_section", "topic", "event_date")], 
            z2[,c("fire_fly_id", "section","content_type", "sub_section", "topic", "event_date")], 
            z3[,c("fire_fly_id", "section","content_type", "sub_section", "topic", "event_date")], 
            z4[,c("fire_fly_id", "section","content_type", "sub_section", "topic", "event_date")])

#Regular expression to parse out text

pattern <- "([^:]*)$"
z$sub_section_parsed <- str_extract(z$sub_section, regex(pattern))
z$topic_parsed <- str_extract(z$topic, regex(pattern))


#group data by fire_fly_id

z_features_sub_section <- z %>%
  group_by(fire_fly_id,sub_section_parsed) %>%
  summarize(count_sub_section =n()) %>%  
  spread(sub_section_parsed, count_sub_section) #create individual columns from values in a single column

z_features_topic <- z %>%
  group_by(fire_fly_id,topic_parsed) %>%
  summarize(count_topic =n()) %>% 
  spread(topic_parsed, count_topic) 

#replace zeros
z_features_sub_section[is.na(z_features_sub_section)] <- 0

#log all columns using apply function 
temp <- data.frame(apply(z_features_sub_section,2,function(x) log(x+1))) #transform from matrix to data frame 

#K means
temp1<- temp[,-1] #remove fire fly id as we do not want this attribute in the cluster, only the features 
View(temp1)

fit1 <- kmeans(temp1[,211:214], centers = 5, nstart=100, iter.max = 100)
summary(fit1)
fit1$size
plot(fit1)




#allthemoms, V1, blogs, baseball, arts, announcements, celebrations, business, bugpages, 
#crime, columnists, columnist, college, dining, deals, cycling, error, entertainment, 
#education, editorials, food, flights, fantasy, extras, extended, experience, golf, get.access, ftw,
#home, high.school, insider, humankind, life, lancaster.festival, local, mlb, nation, music, movies
#money, news, ncaaf, nation.now, people, outdoors, opinion, olympics, ohio.state, nhl, nfl,
#politics.extra, politics, personalfinance, reviewedcom, rentals, real.estate, readers, 
#sports, special.reports, search, tech, static, state, staff, usi, ufc, ue, tv, travel, traffic, 
#tennis, wellness, weather, ustoa, world, wnba, winter.olympics.2018


#business

bus1 <- temp1[,c( "business", "crime", "college", "education", "editorials", "golf", 
                  "insider", "opinion", "politics", "personalfinance", "real.estate", "traffic", "tech")]

fit1 <- kmeans(ent2, centers = 9, nstart=100, iter.max = 100)
summary(fit1)
fit1$size
plot(fit1)


#explorer

exp <- temp1[,c( "celebrations", "dining", "life","people", "nation.now", "outdoors" ,
                  "get.access")]


fit1 <- kmeans(exp, centers = 8, nstart=100, iter.max = 100)
summary(fit1)
fit1$size
plot(fit1)

#entertainment

ent <- temp1[,c("allthemoms", "V1", "blogs", "arts", "announcements", "celebrations", "bugpages",
                "columnists", "columnist", "dining", "deals", "entertainment")]

fit1 <- kmeans(ent, centers = 7, nstart=100, iter.max = 100)
summary(fit1)
fit1$size
plot(fit1)

#sports
sports <- temp1[, c("baseball", "college", "entertainment", "fantasy", "golf", "get.access", "ftw",
                    "local", "mlb", "nation", "olympics", "ohio.state", "nhl", "nfl", "state")]

fit1 <- kmeans(sports, centers = 6, nstart=100, iter.max = 100)
summary(fit1)
fit1$size
plot(fit1)



