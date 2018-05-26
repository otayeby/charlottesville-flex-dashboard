# Start measuring time
start_time <- Sys.time()

#Load the libraries needed
library(tidyverse); library(stm)

# Load the input files: ctmFitNoHashtagsHotIssueLocal.RData (has the topic model) and allDocs1.RData (has the original tweets and other info)
#load(file = "ctmFitNoHashtagsHotIssueLocal.RData")
#load("allDocs1.RData")

loc <- getwd()
load(paste(loc, '/data/ctmFitNoHashtagsHotIssueLocal.RData', sep = ''))
#load(paste(loc, '/data/allDocs1.RData', sep = ''))

# Get number of topics
n <- dim(ctmFitNoHashtags40$theta)[1]
k <- dim(ctmFitNoHashtags40$theta)[2]

# Prepare the labels using the top 5 words
labels <- sageLabels(ctmFitNoHashtags40, n = 5)
# sageLabels: Displays Verbose Labels That Describe Topics And Topic-Covariate Groups In Depth
# change labels$marginal$prob for raw probabilities

# Get topic names
topicDF <- tibble(topicNum = paste0("V",1:k),
                  Prob = sapply(1:k, function(x) paste0(labels$marginal$prob[x,], collapse = " + ")),
                  Frex = sapply(1:k, function(x) paste0(labels$marginal$frex[x,], collapse = " + ")),
                  ExpProportions = colMeans(ctmFitNoHashtags40$theta))

# Construct a dataframe from the document-topic matrix Theta, topic names V1 ... Vk, with metadata
theta <- as.tibble(ctmFitNoHashtags40$theta) %>%
  mutate(id = as.character(out$meta$id),
         hotIssue = ifelse(out$meta$hotIssue == 1L, "Hot Issue", "Enduring Public"),
         local = ifelse(out$meta$local == 1L, "Local", "Non-Local"))

# Save the tweetIds and body from the allDocs in a separate variable
tweetInfo <- allDocs1[c("tweetId", "screenName", "body", "postedTime")]
tweetInfo$tweetId <- as.character(tweetInfo$tweetId)

# Rename the tweetId column to id, so it can match the model_theta id column when merging
names(tweetInfo) <- c("id", "screenName", "body", "postedTime")

# Merge the model_theta and tweet_id_info by id and save it to model_theta
df <- merge(tweetInfo, theta, by="id")

# Get network
library(igraph)
network <- topicCorr(ctmFitNoHashtags40, cutoff = .01)
# for network plot
links2 <- as.matrix(network$posadj)
net2 <- graph_from_adjacency_matrix(links2, mode = "undirected") %>%
  simplify(remove.multiple = F, remove.loops = T)

links <- as_data_frame(net2, what="edges")
nodes <- as_data_frame(net2, what="vertices")

#visNetwork edits
nodes$shape <- "dot"
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$label <- topicDF$Prob # Node label
nodes$title <- topicDF$Frex # Node label
nodes$size <- (topicDF$ExpProportions / max(topicDF$ExpProportions)) * 40 # Node size
nodes$borderWidth <- 2 # Node border width
nodes$id <- topicDF$topicNum

net <- list(nodes, links)

# Save the dataframe
save(df, file = "data/tweet_topic_df.RData")
save(topicDF, file = "data/topicDF.RData")
save(net, file = "data/network.RData")
save(dfmNoHashtags, file = "data/dfmNoHashtags.RData")

# End measuring time and print the time taken
end_time <- Sys.time()
print(end_time - start_time)
# K = 10
# 1/3: 7.662826 secs
# 1/2: 12.90461 secs
# 1/1: 25.20941 secs
# K = 40
# 1/1: 37.07303 secs