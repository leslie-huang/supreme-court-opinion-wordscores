# Leslie Huang
# Text as Data
# Final Paper replication file

# Set up the workspace and libraries
rm(list=ls())
setwd("/Users/lesliehuang/Dropbox/final paper")

libraries <- c("foreign", "utils", "stargazer", "dplyr", "devtools", "quanteda", "quantedaData", "ggplot2", "stringr", "gdata")
lapply(libraries, require, character.only=TRUE)

##############################################################
# Read in vote data from http://supremecourtdatabase.org/
load("/Users/lesliehuang/Dropbox/final paper/SCDB_2015_03_justiceCentered_Docket.Rdata")

# subset to the time period of interest
SCDB <- filter(SCDB_2015_03_justiceCentered_Docket, term >= 2008)
rm(SCDB_2015_03_justiceCentered_Docket)

# read in the data scraped by python from Justia.com
scraps <- read.csv("supreme-court-scraper/opinions2003-2015.csv", stringsAsFactors = FALSE)

# clean up the opinion types, make into a categorical var
scraps$type <- str_replace(scraps$type, "[0-9]$", "")
scraps$type <- str_replace(scraps$type, "concurrence", "concur")
scraps$type <- str_replace(scraps$type, "opinion", "majority")
factor(scraps$type)
factor(scraps$justice)

# remove blank opinions
scraps <- scraps[-which(scraps$text == ""), ]

# frequency table for type
type_count <- count(scraps, type)
stargazer(type_count, title="Frequency of Opinion Types", summary = FALSE)

# get some info about the opinion distribution
num_unique_cases <- count(scraps, number)
num_maj_only <- filter(num_unique_cases, n == 1)

# extract metadata
opinion_metadata <- select(scraps, number, year, justice, type)

# make it into a corpus
opinions_corpus <- corpus(scraps$text, docvars = opinion_metadata)

# select anchor texts for wordscores:

# read in justice-term ideology ratings from Martin-Quinn (http://mqscores.berkeley.edu/measures.php). We will use these measures to select the most extreme liberal/conservative texts
mq <- read.csv("justices.csv", stringsAsFactors = FALSE)
mq <- filter(mq, term >= 2003)

# calculate mean score for each justice, then find the extremes
factor(mq$justiceName)
mean_mq_scores <- aggregate(mq$post_mn, list(mq$justiceName), mean)
colnames(mean_mq_scores) <- c("justice", "score")
arrange(mean_mq_scores, desc(score))

# most conservative = Thomas, Scalia, Rehnquist, Alito. most liberal: Stevens, Sotomayor, Ginsburg
# let's aim for 100 anchor texts for each extreme. We want a random mix of the justices to avoid justice fixed effects.

n <- length(scraps[,1])
scraps$id[1:n] <- 1:n

scraps$ideology <- NA

anchor_con <- c("ClarenceThomas", "AntoninScalia", "SamuelAAlitoJr")
anchor_lib <- c("JohnPaulStevens", "SoniaSotomayor", "RuthBaderGinsburg")

anchor_con_op <- sample_n(filter(scraps, justice %in% anchor_con), 100)
anchor_con_op$ideology <- 1 # 1 for conservative
anchor_lib_op <- sample_n(filter(scraps, justice %in% anchor_lib), 100)
anchor_lib_op$ideology <- -1 # -1 for liberal

anchors <- rbind(anchor_con_op, anchor_lib_op)
anchor_ids <- anchors$id

# get the non-anchors
not_anchors <- scraps[-anchor_ids, ]

# merge them back together
ws_labeled <- rbind(anchors, not_anchors)

# metadata
ws_metadata <- select(ws_labeled, number, year, justice, type, ideology, id)

# make the corpus
ws_corpus <- corpus(ws_labeled$text, docvars = ws_metadata)

# make the dfm
ws_dfm <- dfm(ws_corpus, stem = TRUE)

# wordscores textmodel. 100 liberal and 100 conservative anchor texts are labeled, the rest are NA
ws_model <- textmodel(ws_dfm, y = ws_metadata$ideology, model = "wordscores", smooth = 1)

# anchor corpus
anchor_metadata <- select(anchors, number, year, justice, type, ideology, id)
anchor_corpus <- corpus(anchors$text, docvars = anchor_metadata)
anchor_dfm <- dfm(anchor_corpus, stem = TRUE)

# not anchor corpus
not_anchor_metadata <- select(not_anchors, number, year, justice, type, ideology, id)
not_anchor_corpus <- corpus(not_anchors$text, docvars = not_anchor_metadata)
not_anchor_dfm <- dfm(not_anchor_corpus, stem = TRUE)

# use model to predict
model <- textmodel(anchor_dfm, y = anchor_metadata$ideology, model = "wordscores", scale = "linear", smooth = 0)

# save it
predicted_ws <- predict(model, newdata = not_anchor_dfm, rescaling = "lbg", level = 0.95, verbose = TRUE)

# put the predicted ideology scores back into our original df
not_anchors$ideology <- predicted_ws@textscores$textscore_lbg

# get all our data back in one df
scored_df <- rbind(not_anchors, anchors)
scored_df <- scored_df[order(scored_df$number), ]

# get some quick summary stats
concurs <- filter(scored_df, type == "concur")
mean(abs(concurs$ideology))
sd(abs(concurs$ideology))
cdparts <- filter(scored_df, type == "cdinpart")
dissents <- filter(scored_df, type == "dissent")

par(mfrow=c(2,2))
plot(concurs$ideology, main="Ideological Positions of Concurrences", ylab = "Wordscore Ideological Position", ylim = c(-5, 5))
plot(cdparts$ideology, main="Ideological Positions of Concurrences in Part", ylab = "Wordscore Ideological Position", ylim = c(-5, 5))
plot(dissents$ideology, main="Ideological Positions of Dissents", ylab = "Wordscore Ideological Position", ylim = c(-5, 5))
plot(cases_df$ideology, main="Ideological Positions of Majority Opinion", ylab = "Wordscore Ideological Position", ylim = c(-5, 5))
dev.copy(pdf, "scatter.pdf")
dev.off()


# get the data ready for OLS
cases_df <- filter(scored_df, type == "majority")
cases_df <- cases_df[ , -5]
cases_df$num_concur <- 0
cases_df$num_dissent <- 0
cases_df$num_cdpart <- 0
cases_df$max_concur <- NA
cases_df$max_dissent <- NA
cases_df$max_cdpart <- NA


# fill in concur/dissent counts, and the max ideology of concurring and dissenting opinions
for (i in 1:length(cases_df[,1])) {
  
  for (j in 1:length(scored_df[,1])) {
    
    if (cases_df$number[i] == scored_df$number[j]) {
      
      # fill in concurrence data
      if (scored_df$type[j] == "concur") {
        cases_df$num_concur[i] <- cases_df$num_concur[i] +1
        
        if (is.na(cases_df$max_concur[i])) {
          cases_df$max_concur[i] <- scored_df$ideology[j]
        }
        
        if (abs(cases_df$max_concur[i]) < abs(scored_df$ideology[j])) {
          cases_df$max_concur[i] <- scored_df$ideology[j]
        }
        
      }
      
      # fill in dissent data
      if (scored_df$type[j] == "dissent") {
        cases_df$num_dissent[i] <- cases_df$num_dissent[i] +1
        
        if (is.na(cases_df$max_dissent[i])) {
          cases_df$max_dissent[i] <- scored_df$ideology[j]
        }
        
        if (abs(cases_df$max_dissent[i]) < abs(scored_df$ideology[j])) {
          cases_df$max_dissent[i] <- scored_df$ideology[j]
        }
        
      }
      
      # fill in concurred in part data
      if (scored_df$type[j] == "cdinpart") {
        cases_df$num_cdpart[i] <- cases_df$num_cdpart[i] +1
        
        if (is.na(cases_df$max_cdpart[i])) {
          cases_df$max_cdpart[i] <- scored_df$ideology[j]
        }
        
        if (abs(cases_df$max_cdpart[i]) < abs(scored_df$ideology[j])) {
          cases_df$max_cdpart[i] <- scored_df$ideology[j]
        }
        
      }
    }
  }
  
}

# let's compare our results with the Martin Quinn positions
factor(scored_df$justice)
mean_ws_scores <- aggregate(scored_df$ideology, list(scored_df$justice), mean)
colnames(mean_ws_scores) <- c("justice", "score")
mean_ws_scores[,1] <- c("Kennedy", "Scalia", "Thomas", "Souter", "Kagan", "Roberts", "Stevens", "Ginsburg", "Alito", "Sotomayor", "Breyer", "Rehnquist")
arrange(mean_ws_scores, desc(score))

# plot it
mean_mq_scores <- mean_mq_scores[-6,]
mean_ws_scores <- arrange(mean_ws_scores, desc(justice))
mean_mq_scores <- arrange(mean_mq_scores, desc(Group.1))
ws_mq_scores <- cbind(mean_ws_scores, mean_mq_scores)
ws_mq_scores <- ws_mq_scores[, -3]
colnames(ws_mq_scores) <- c("Justice", "Wordscore position", "Martin-Quinn position")

pos_vector <- rep(3, length(ws_mq_scores[,1]))
pos_vector[12] <- 1
pos_vector[9] <- 1


plot(ws_mq_scores$`Wordscore position`, ws_mq_scores$`Martin-Quinn position`, main = "Scatter Plot of Martin-Quinn and Wordscore Ideological Scores", xlab = "Wordscore Ideological Score", ylab = "Martin-Quinn Ideological Score")
text(ws_mq_scores$`Wordscore position`, ws_mq_scores$`Martin-Quinn position`, ws_mq_scores$Justice, cex = 0.6, pos = pos_vector, col ="red")
dev.copy(pdf, "mq_ws.pdf")
dev.off()

############################################################################
# linear model
abs_cases <- cases_df
abs_cases$max_concur <- abs(abs_cases$max_concur)
abs_cases$max_dissent <- abs(abs_cases$max_dissent)
abs_cases$ideology <- abs(abs_cases$ideology)
abs_cases$max_cdpart <- abs(abs_cases$max_cdpart)
abs_cases$num_ccd <- abs_cases$num_concur + abs_cases$num_cdpart
abs_cases$total_opinions <- abs_cases$num_dissent + abs_cases$num_ccd

for (i in 1:length(abs_cases[,1])) {
  if (!is.na(abs_cases$max_concur[i]) & is.na(abs_cases$max_cdpart[i])) {
    abs_cases$max_ccd[i] <- abs_cases$max_concur[i]
  }
  
  if (!is.na(abs_cases$max_cdpart[i]) & is.na(abs_cases$max_concur[i])) {
    abs_cases$max_ccd[i] <- abs_cases$max_cdpart[i]
  }
  
  if (!is.na(abs_cases$max_cdpart[i]) & !is.na(abs_cases$max_concur[i])) {
    abs_cases$max_ccd[i] <- max(abs_cases$max_concur[i], abs_cases$max_cdpart[i])
  }
}


# ws_lm <- lm(ideology ~ num_concur + max_concur + num_cdpart + max_cdpart, data = cases_df)

ws_lm_abs <- lm(ideology ~ num_concur + max_concur + num_cdpart + max_cdpart, data = abs_cases)
summary(ws_lm_abs)

ws_lm_ccd <- lm(ideology ~ num_ccd + max_ccd, data = abs_cases)
summary(ws_lm_ccd)

ws_lm_all <- lm(ideology  ~ num_concur + max_concur + num_cdpart + max_cdpart + num_dissent + max_dissent, data = abs_cases)

stargazer(ws_lm_abs, ws_lm_ccd, title = "Regression Results", align = TRUE)
