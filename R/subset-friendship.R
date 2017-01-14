# split up the friend data into 2.

library(RSiena)
friend.data.w1 <- as.matrix(read.table("data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("data/s50-network3.dat"))
drink <- as.matrix(read.table("data/s50-alcohol.dat"))


fdw1 <- as.data.frame(friend.data.w1)
fdw1$from <- 1:50
fdw2 <- as.data.frame(friend.data.w2)
fdw2$from <- 1:50
fdw3 <- as.data.frame(friend.data.w3)
fdw3$from <- 1:50
library(tidyr)
library(dplyr)
library(readr)
fdw1 %>%
  gather(variable, value, -from) %>%
  mutate(to = parse_number(variable)) %>% select(from, to, value) -> fdw1_long
fdw2 %>%
  gather(variable, value, -from) %>%
  mutate(to = parse_number(variable)) %>% select(from, to, value) -> fdw2_long
fdw3 %>%
  gather(variable, value, -from) %>%
  mutate(to = parse_number(variable)) %>% select(from, to, value) -> fdw3_long

fdw1_long$wave <- 1
fdw2_long$wave <- 2
fdw3_long$wave <- 3

fd_long <- rbind(fdw1_long, fdw2_long, fdw3_long)

library(ggplot2)

# ggplot(data = fd_long) +
#   geom_raster(aes(x = from, y = to, fill = as.factor(value))) +
#   scale_fill_manual(values = c('white', 'black')) +
#   theme(aspect.ratio = 1) +
#   facet_wrap(~wave)

# fdw2_long %>% group_by(from) %>% summarize(outdegree = sum(value))
# filter(fdw2_long, from == 21 & value == 1)
#
# plot(as.network.matrix(friend.data.w2, matrix.type = "adjacency"),
#      displaylabels=T, mode = 'kamadakawai')
# want to split up the 2nd wave into 2. Remove the tie from 21 to 19.

# only want

sub_ids <- sort(c(19,23,50,41,38,30,15,42,29,16,44,7,12,2,26,33,14,1,10,11,42,36))
friend.data.w1a <- friend.data.w1[sub_ids, sub_ids]
friend.data.w1b <- friend.data.w1[-sub_ids, -sub_ids]
friend.data.w2a <- friend.data.w2[sub_ids, sub_ids]
friend.data.w2b <- friend.data.w2[-sub_ids, -sub_ids]
friend.data.w3a <- friend.data.w3[sub_ids, sub_ids]
friend.data.w3b <- friend.data.w3[-sub_ids, -sub_ids]

drinka <- drink[sub_ids,]
drinkb <- drink[-sub_ids,]

friendshipDataa <- array(c(friend.data.w1a, friend.data.w2a, friend.data.w3a), dim = c(22, 22, 3))
friendshipDatab <- array(c(friend.data.w1b, friend.data.w2b, friend.data.w3b), dim = c(29, 29, 3))

friendshipa <- sienaDependent(friendshipDataa)
friendshipb <- sienaDependent(friendshipDatab)

alcohola <- varCovar(drinka)
alcoholb <- varCovar(drinkb)

mydataa <- sienaDataCreate(friendshipa, alcohola)
mydatab <- sienaDataCreate(friendshipb, alcoholb)
