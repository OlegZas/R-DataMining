### -------------------------------------------------------------------------------
#In this lab, we will first introduce the recommenderlab package to build recommender systems based on movie rating data. 
#We then perform social network analysis based on a small Facebook data
### ------------------------------------------------------------------------------

## Part 1. Recommender Systems
#We will use the MovieLense data set, which contains around 100,000 ratings (1-5) from 943 users on 1664 movies.
#In this MovieLense data set, each row corresponds to a user, and each column corresponds to a movie. 

#1. load recommender lab package
library(recommenderlab)

#2. load MovieLense dataset
data(MovieLense)
MovieLense
dim(MovieLense)

# Rating information
rating_vector = as.vector(MovieLense@data)
unique(rating_vector)
rating_table = table(rating_vector)
rating_table

# find the similarity among the first 5 users
similarity_users = similarity(MovieLense[1:5, ], method = "cosine", which = "users")
similarity_users
# find the similarity among the first 5 items
similarity_items = similarity(MovieLense[, 1:5], method ="cosine", which = "items")
similarity_items


#3. Build recommender systems
# The collaborative filtering algorithm relies on the users ratings to calculate the user-user or item-item similarity.
# We will remove movies that have been viewed only a few times, because their ratings might be biased due to the lack of data.
# We will also remove users who rated only a few movies because the small number of ratings cannot accurately reflect user's preference.

# We will build recommender system for users who rated at least 20 movies, and for movies have been rated over 20 times.
ratings_movies = MovieLense[rowCounts(MovieLense) > 20, colCounts(MovieLense) > 20] 
ratings_movies

# Define a evaluation scheme determining how to train and test a recommender system.
# Here we create an evaluation scheme to split the ratings_movies data into 80% training and 20% testing data.
# Therefore, we will randomly select 80% users' rating data as our training set and 20% users' ratings as the testing set. 
# For each user in the testing set, the recommender algorithm will predict user's ratings based on 15 given item ratings.
set.seed(1)
e_scheme = evaluationScheme(ratings_movies, method="split", train=0.8, given=15, goodRating=5)

# Train an user-based (user-user) collaborative filtering on the training data set
u_recommender = Recommender(getData(e_scheme, "train"), "UBCF")

# Predict on the testing data
u_prediction = predict(u_recommender, getData(e_scheme, "known"), type="ratings")

# Evaluate the recommendation performance
u_evaluation = calcPredictionAccuracy(u_prediction, getData(e_scheme, "unknown"))
u_evaluation

# Train an item-based (item-item) collaborative filtering on the training data set
i_recommender = Recommender(getData(e_scheme, "train"), "IBCF")

# Predict on the testing data
i_prediction = predict(i_recommender, getData(e_scheme, "known"), type="ratings")

# Evaluate the recommendation performance
i_evaluation = calcPredictionAccuracy(i_prediction, getData(e_scheme, "unknown"))
i_evaluation

# Select data to include users who rated at least 100 movies and movies having over 100 ratings


# Define a evaluation scheme determining how to train and test a recommender system
set.seed(1)
e_scheme_100 = evaluationScheme(ratings_movies_100, method="split", train=0.8, given=15, goodRating=5)

# Train and evaluate an user-based and item-based recommender system


# Which recommender system has better performance, user-based or item-based, and why?


## Part 2. Social Network Analysis
# We will use a small Facebook data from stanford network analysis project: https://snap.stanford.edu/data/ego-Facebook.html.
# The social network is stored by an edgelist.

#1. load the igraph package
library(igraph)
facebook_edges = read.table('facebook_edges.txt')
facebook_igraph = graph.data.frame(facebook_edges, directed=FALSE)

#2. number of nodes and edges

#3. Density


#4. Degree centrality

#5. Betweenness centrality

#6. Closeness centrality

#7. Transitivity

#8. Find cliques
 
# Get the size of each clique

# Plot the largest cliques
vcol = rep("grey", vcount(facebook_igraph))
vcol[unlist(largest_cliques(facebook_igraph))] = "red"
plot(as.undirected(facebook_igraph), vertex.label=V(facebook_igraph)$name, vertex.color=vcol)

#9. Community detection
# Communities are detected by rogressively removing edges from the social network. 
# Edges with high betweenness, which are most likely "between" communities, are removed sequentially to porduce communities.







