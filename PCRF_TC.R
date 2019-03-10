
#####################################################################################
### Project     : ICT Industry 4.0s
### Script      : PCRF_Test-Case.R
### Description : PCRF Test Case
#####################################################################################


#####################################################################################
### TC ID: 01
#####################################################################################


# Load library 
pkgs <- c("dplyr", "readxl", "stringr", "quanteda", "scales", "h2o", "tm", 
          "ggplot2", "tidyr", "data.table", "RColorBrewer", "data.table")
sapply(pkgs, require, character.only = T)


#####################################################################################
### TC ID: 02
#####################################################################################


# Set working directory
setwd("C:/Users/rymyu/Dropbox/Public/project/hmd/2018/code/PCRF/PCRF_TC")

# Load data
dg <- read.csv("dg_final_V5.csv", stringsAsFactors = F)


#####################################################################################
### TC ID: 03
#####################################################################################


# Initialize h2o
h2o.init()


#####################################################################################
### TC ID: 04
#####################################################################################


# Tokenize sentences when using quanteda
eda_tokenize <- function(x, n_grams = 1:3) {
  # Extract and clean tokens
  tkns <- tokens(x, what = "word", 
                 remove_numbers = T, remove_punct = T, remove_symbols = T,
                 remove_hyphens = T, remove_url = T)
  tkns <- tokens_tolower(tkns)
  tkns <- tokens_select(tkns, stopwords(), selection = "remove")
  tkns <- tokens_wordstem(tkns, language = "english")
  # tkns <- tokens_ngrams(tkns, n = 1:3)
  tkns <- tokens_skipgrams(tkns, n = n_grams, skip = 0, concatenator = "_")
  
  # Remove single character tokens
  tkns <- as.tokens(lapply(tkns, function(x) x[nchar(x) > 1]))
  
  return(tkns)
}

# Tokenize sentences when using h2o for word2vec
h2o_tokenize <- function(sentences, stop.words = stopwords()) {
  tokenized <- h2o.tokenize(sentences, "\\\\W+")
  
  # convert to lower case
  tokenized.lower <- h2o.tolower(tokenized)
  
  # remove short words (less than 2 characters)
  tokenized.lengths  <- h2o.nchar(tokenized.lower)
  tokenized.filtered <- tokenized.lower[is.na(tokenized.lengths) || tokenized.lengths >= 2,]
  
  # remove words that contain numbers
  tokenized.words <- tokenized.lower[h2o.grep("[0-9]", tokenized.lower, invert = T, output.logical = T),]
  
  # remove stop words
  tokenized.words[is.na(tokenized.words) || (! tokenized.words %in% stop.words),]
}

#####################################################################################
### TC ID: 05
#####################################################################################


# Cosine similarity
cosine_sparse <- function(x, y = NULL, margin = 1) {
  if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
  if (margin == 1) x <- t(x)
  S <- rep(1, nrow(x))			
  #N <- Matrix::Diagonal( x = match.fun(norm)(x, S)^-1 )
  N <- Matrix::Diagonal( x = sqrt(colSums(x^2))^-1 )
  x <- x %*% N
  if (!is.null(y)) {
    if (margin == 1) y <- t(y)
    #N <- Matrix::Diagonal( x = match.fun(norm)(y, S)^-1 )
    N <- Matrix::Diagonal( x = sqrt(colSums(y^2))^-1 )
    y <- y %*% N
    return(as.matrix(Matrix::crossprod(x,y)))
  } else
    return(as.matrix(Matrix::crossprod(x)))
}

# Calculate Similarity : method w2v / cosine / jaccard
calculate_simil <- function(df_q, n_grams, method) {
  
  if (method == "w2v") {
    dg_ch_w2v <- data.frame(q = df_q, stringsAsFactors = F)
    
    h2o_dg <- as.h2o(dg_ch_w2v, destination_frame = "questions")
    q_tkns_h2o <- h2o_tokenize(h2o_dg$q)
    
    vec   <- 50 # Only 50 vectors to save time/memory
    q_w2v <- h2o.word2vec(q_tkns_h2o, model_id = "q_model", vec_size = vec, min_word_freq = 5, 
                          window_size = 5, init_learning_rate = 0.025, sent_sample_rate = 0, 
                          epochs = 20)
    
    q_vecs         <- as.matrix(h2o.transform(q_w2v, q_tkns_h2o, aggregate_method = "AVERAGE"))
    q_sm           <- cosine_sparse(q_vecs)
    #dimnames(q_sm) <- list(paste0("text", 1:nrow(q_sm)), paste0("text", 1:ncol(q_sm)))
    diag(q_sm)     <- NA
    
  } else {
    # Extract and clean tokens for questions
    tkns_eda <- eda_tokenize(df_q, n_grams = n_grams)
    
    # Create document term (feature) matrix for questions
    dfm_eda <- dfm(tkns_eda)
    
    # Create cosine similarity matrix (questions)
    q_sm       <- as.matrix(textstat_simil(dfm_eda, method = method, margin = "documents"))
    diag(q_sm) <- NA
  }
  
  return(q_sm)
}


#####################################################################################
### TC ID: 06
#####################################################################################


# Recommend Answer with Probability
recommend_a <- function(question, ch) {
  
  # Filter by chapter and Merge question with PCRF Data
  group <- dg %>% filter(chapter == ch) %>% select(chapter_group) %>% unique() %>% as.character()
  
  dg_ch <- dg %>% filter((chapter_group == group) & !is.na(a)) %>%
    mutate(text = paste0("text", index)) %>%
    bind_rows(data.frame(text = "question", q = question, stringsAsFactors = F))
  
  
  # Specify number for ngrams for quanteda methods
  n_grams <- 1:3
  
  
  # Create similarity matrix (questions)
  q_sm <- calculate_simil(dg_ch$q, n_grams = n_grams, method = "w2v")
  colnames(q_sm) <- dg_ch$text
  rownames(q_sm) <- dg_ch$text
  
  # Search Top 5 Similar Questions and Filter by threshold
  q_simil_df <- q_sm["question",] %>% as.data.frame() %>%
    rename(qsim = ".") %>% mutate(sq = dg_ch$text) %>%
    arrange(desc(qsim)) %>% select(sq, qsim) %>%
    filter(qsim >= 0.8) %>% top_n(5, qsim)
  
  if (nrow(q_simil_df) <= 1) {
    if (nrow(q_simil_df) == 0) print("Notice : There is no similar question.") # If there is no question more than threshold
    if (nrow(q_simil_df) == 1) print("Notice : There is only one similar question.") # If there is one question more than threshold
    
    # Not filtered by chapter
    dg_ch <- dg %>% filter(!is.na(a)) %>%
      mutate(text = paste0("text", index)) %>%
      bind_rows(data.frame(text = "question", q = question, stringsAsFactors = F))
    
    # Create similarity matrix (questions)
    q_sm <- calculate_simil(dg_ch$q, n_grams = n_grams, method = "w2v")
    colnames(q_sm) <- dg_ch$text
    rownames(q_sm) <- dg_ch$text
    
    # Search Top 5 Similar Questions and Filter by threshold
    q_simil_df <- q_sm["question",] %>% as.data.frame() %>%
      rename(qsim = ".") %>% mutate(sq = dg_ch$text) %>%
      arrange(desc(qsim)) %>% select(sq, qsim) %>% top_n(5, qsim)
  }
  
  if (nrow(q_simil_df) > 5) {
    q_simil_df <- q_simil_df[1:5,]
  }
  
  # Create similarity matrix(answers)
  a_sm <- calculate_simil(dg_ch$a, n_grams = n_grams, method = "w2v")
  colnames(a_sm) <- dg_ch$text
  rownames(a_sm) <- dg_ch$text
  answer_df <- a_sm[q_simil_df$sq, q_simil_df$sq]
  
  answer_df <- answer_df %>% data.frame() %>%
    mutate(sq = row.names(answer_df),
           q = dg_ch[dg_ch$text %in% q_simil_df$sq, "q"],
           a = dg_ch[dg_ch$text %in% q_simil_df$sq, "a"],
           mean_asim = colMeans(answer_df[,-c(6,7)], na.rm = T), 
           num_filtered_asim = ((answer_df[,-c(6,7,8)] > 0.6) %>% colSums(na.rm = T)) / nrow(answer_df), 
           qsim = q_simil_df$qsim) %>%
    select(sq, q, a, mean_asim, num_filtered_asim, qsim) %>%
    left_join(dg %>% mutate(sq = paste0("text", index)) %>% select(sq, sheet), by = "sq")
  answer_df$pr <- apply(answer_df[, c(4, 5, 6)], 1, function(x) {x * matrix(c(0.2,0.3,0.5), nrow=1)}) %>% t() %>% rowSums()
  
  # Order by probability
  answer_df <- answer_df %>% arrange(desc(pr)) %>% 
    mutate(pr = paste0(round(pr,4)*100, "%"),
           a = a %>% str_remove_all("\\n") %>% str_remove_all("\\r")) %>% select(a, pr, sheet)
  
  return(answer_df)
}


#####################################################################################
### TC ID: 07
#####################################################################################


# Case 1
q1 <- "Taking into consideration that this is a MINOR modification and further to our good collaboration till now and the fact that we have a big New-building program to your good shipyard, please comply with our minor request free of charge. Please re-consider subject issue"
ch1 <- "aux. central cool f.w. system"
test1 <- recommend_a(q1, ch1)

# Case 2
q2 <- "A supply duct to be provided near the HTC FW pumps and Fresh Water Generator;"
ch2 <- ""
test2 <- recommend_a(q2, ch2)

# Case 3
q3 <- "Inspection glass shall be provide after V/Vs 08V & 09V"
ch3 <- ""
test3 <- recommend_a(q3, ch3)

# Case 4
q4 <- "Please confirm that the telephone in the workshop and one of the telephones in the engine control room will be connected to the common circuit as same phone number."
ch4 <- ""
test4 <- recommend_a(q4, ch4)


#####################################################################################
### TC ID: 08
#####################################################################################


# Check performance
performance_check <- function(data, ch, idx, method) {
  
  # Filter by chapter and Merge question with PCRF Data
  group <- data %>% filter(chapter == ch) %>% select(chapter_group) %>% unique() %>% as.character()
  dg_temp <- data %>% mutate(text = paste0("text", index))
  dg_ch <- dg_temp %>% filter((chapter_group == group) & !is.na(a))
  
  if (method == "cosine") {
    q_sm <- cos_qsm
    a_sm <- cos_asm
  } else if (method == "w2v") {
    q_sm <- w2v_qsm
    a_sm <- w2v_asm
  }
  
  q_simil_df <- q_sm[paste0("text", idx), dg_ch$text] %>% as.data.frame() %>%
    rename(qsim = ".") %>% mutate(sq = dg_ch$text) %>%
    arrange(desc(qsim)) %>% select(sq, qsim) %>%
    filter(qsim >= 0.8) %>% top_n(5, qsim)
  
  if (nrow(q_simil_df) <= 1) {
    
    # Not filtered by chapter
    dg_no_ch <- dg_temp %>% filter(!is.na(a))
    
    # Search Top 5 Similar Questions and Filter by threshold
    q_simil_df <- q_sm[paste0("text", idx), dg_no_ch$text] %>% as.data.frame() %>%
      rename(qsim = ".") %>% mutate(sq = dg_no_ch$text) %>%
      arrange(desc(qsim)) %>% select(sq, qsim) %>% top_n(5, qsim)
    
    if (nrow(q_simil_df) > 5) {
      q_simil_df <- q_simil_df[1:5,]
    }
  }
  
  if (nrow(q_simil_df) == 0) {
    result <- data.frame(q_idx = NA,
                         q = NA,
                         a = NA,
                         mean_asim = NA,
                         numfiltered_asim = NA,
                         qsim = NA,
                         pr = NA,
                         actual_asim = NA)
  } else {
    # Create similarity matrix(answers)
    answer_df <- a_sm[q_simil_df$sq, q_simil_df$sq] %>% data.frame()
    actual_answer_df <- w2v_asm[paste0("text", idx), q_simil_df$sq]
    
    result <- answer_df %>%
      mutate(q_idx = row.names(answer_df),
             mean_asim = colMeans(answer_df, na.rm = T),
             num_filtered_asim = ((answer_df > 0.6) %>% colSums(na.rm = T)) / (nrow(answer_df) - 1), 
             qsim = q_simil_df$qsim) %>%
      left_join(dg_temp %>% 
                  filter(text %in% q_simil_df$sq) %>% 
                  select(text, q, a) %>%
                  rename(q_idx = text), by = "q_idx") %>%
      select(q_idx, q, a, mean_asim, num_filtered_asim, qsim)
    result$pr <- apply(result[, -c(1,2,3)], 1, function(x) {x * matrix(c(0.46,0.04,0.5), nrow=1)}) %>% t() %>% rowSums()
    result$actual_asim <- actual_answer_df
    
    # Order by probability
    result <- result %>% arrange(desc(pr)) %>% 
      mutate(q = q %>% str_remove_all("\\n") %>% str_remove_all("\\r"),
             a = a %>% str_remove_all("\\n") %>% str_remove_all("\\r"))
  }
  return(result)
}

compare_perf <- function(measure) {
  
  # Select performance measure
  if (measure == "cos") {
    asim <- cos_asm
  } else if (measure == "w2v") {
    asim <- w2v_asm
  } else if (measure == "dm") {
    asim <- dm_asm
  } else if (measure == "dbow") {
    asim <- dbow_asm
  }
  
  answer_all$actual_asim <- 0
  p <- progress_estimated(nrow(answer_all))
  for (i in 1:nrow(answer_all)) {
    if (is.na(answer_all$actual_idx[i]) || is.na(answer_all$answer_idx[i])) {
      answer_all$actual_asim[i] <- NA
    } else {
      answer_all$actual_asim[i] <- asim[answer_all$actual_idx[i], answer_all$answer_idx[i]]
    }
    p$tick()$print()
  }
  
  return(answer_all)
}


#####################################################################################
### TC ID: 09
#####################################################################################


n_grams <- 1:3

cos_qsm <- calculate_simil(dg$q, n_grams = n_grams, method = "cosine")
colnames(cos_qsm) <- paste0("text", dg$index)
rownames(cos_qsm) <- paste0("text", dg$index)

w2v_qsm <- calculate_simil(dg$q, n_grams = n_grams, method = "w2v")
colnames(w2v_qsm) <- paste0("text", dg$index)
rownames(w2v_qsm) <- paste0("text", dg$index)


cos_asm <- calculate_simil(dg$a, n_grams = n_grams, method = "cosine")
colnames(cos_asm) <- paste0("text", dg$index)
rownames(cos_asm) <- paste0("text", dg$index)

w2v_asm <- calculate_simil(dg$a, n_grams = n_grams, method = "w2v")
colnames(w2v_asm) <- paste0("text", dg$index)
rownames(w2v_asm) <- paste0("text", dg$index)


#####################################################################################
### TC ID: 10
#####################################################################################


dm_asm <- fread("dm_asim_mt.csv")
dm_asm <- dm_asm %>% select(-V1) %>% as.matrix()
rownames(dm_asm) <- paste0("text", dg$index)
colnames(dm_asm) <- paste0("text", dg$index)

dbow_asm <- fread("dbow_asim_mt.csv")
dbow_asm <- dbow_asm %>% select(-V1) %>% as.matrix()
rownames(dbow_asm) <- paste0("text", dg$index)
colnames(dbow_asm) <- paste0("text", dg$index)


#####################################################################################
### TC ID: 11
#####################################################################################


answer_result <- data.frame()
p <- progress_estimated(nrow(dg))
for (i in 1:nrow(dg)) {
  
  question <- dg[i, "q"] # Input question
  answer <- dg[i, "a"] # Actual answer
  ch <- dg[i, "chapter"] # Input question's chapter
  target_idx <- dg[i, "index"] # Input index
  
  dg_all <- dg %>% slice(-c(i))
  cos_a <- performance_check(dg_all, ch, target_idx, "cosine") # Recommend answers by cosine
  w2v_a <- performance_check(dg_all, ch, target_idx, "w2v") # Recommend answers by word2vec
  
  # Random Search
  while (T) {
    random_idx <- sample(1:nrow(dg), 1)
    
    if (random_idx != i) {
      break
    }
  }
  rs_a <- dg %>% slice(random_idx) %>% select(q, a, index)
  
  # Make result
  rs_df <- data.frame(method = "rs",
                      actual_idx = paste0("text", target_idx),
                      answer_idx = paste0("text", rs_a$index),
                      input_q = question,
                      sq = rs_a$q,
                      actual_a = answer,
                      recommended_a = rs_a$a,
                      actual_asim = w2v_asm[paste0("text", rs_a$index), paste0("text", target_idx)],
                      stringsAsFactors = F)
  
  cos_df <- data.frame(method = "cos",
                       actual_idx = paste0("text", target_idx),
                       answer_idx = cos_a$q_idx[1],
                       input_q = question,
                       sq = cos_a$q[1],
                       actual_a = answer,
                       recommended_a = cos_a$a[1],
                       actual_asim = cos_a$actual_asim[1],
                       stringsAsFactors = F)
  
  w2v_df <- data.frame(method = "w2v",
                       actual_idx = paste0("text", target_idx),
                       answer_idx = w2v_a$q_idx[1],
                       input_q = question,
                       sq = w2v_a$q[1],
                       actual_a = answer,
                       recommended_a = w2v_a$a[1],
                       actual_asim = w2v_a$actual_asim[1],
                       stringsAsFactors = F)
  
  # Merge it
  answer_result <- answer_result %>%
    bind_rows(rs_df) %>%
    bind_rows(cos_df) %>%
    bind_rows(w2v_df)
  
  p$tick()$print()
}


#####################################################################################
### TC ID: 12
#####################################################################################


# Load Doc2Vec result
doc2vec_DM <- read.csv("doc2vec_DM.csv", stringsAsFactors = F) %>%
  mutate(method = "DM") %>%
  rename(actual_idx = input_q_idx,
         answer_idx = q_idx,
         actual_asim = a_sim) %>%
  select(method, actual_idx, answer_idx, input_q, sq, actual_a, recommended_a, actual_asim)

doc2vec_DBOW <- read.csv("doc2vec_DBOW.csv", stringsAsFactors = F) %>%
  mutate(method = "DBOW") %>%
  rename(actual_idx = input_q_idx,
         answer_idx = q_idx,
         actual_asim = a_sim) %>%
  select(method, actual_idx, answer_idx, input_q, sq, actual_a, recommended_a, actual_asim)


# Merge it
answer_all <- answer_result %>% select(method, actual_idx, answer_idx) %>%
  bind_rows(doc2vec_DM %>% select(method, actual_idx, answer_idx)) %>%
  bind_rows(doc2vec_DBOW %>% select(method, actual_idx, answer_idx)) %>%
  arrange(actual_idx) %>% 
  mutate(method = factor(method, levels = c("rs", "cos", "w2v", "DM", "DBOW")))


#####################################################################################
### TC ID: 13
#####################################################################################


# Performance Check
cos_result <- compare_perf("cos")
w2v_result <- compare_perf("w2v")
dm_result <- compare_perf("dm")
dbow_result <- compare_perf("dbow")


# Merge result
result_all <- cos_result %>% mutate(measure = "cos") %>%
  bind_rows(w2v_result %>% mutate(measure = "w2v")) %>%
  bind_rows(dm_result %>% mutate(measure = "dm")) %>%
  bind_rows(dbow_result %>% mutate(measure = "dbow")) %>%
  mutate(measure = factor(measure, levels = c("cos", "w2v", "dm", "dbow")))


#####################################################################################
### TC ID: 14
#####################################################################################


# Result Plotting
result_all %>% filter(!is.na(actual_asim) & measure == "w2v") %>% 
  group_by(method) %>% summarize(mean_asim = mean(actual_asim, na.rm = T)) %>%
  ggplot(aes(method, mean_asim, fill = method)) + geom_bar(stat = "identity") +
  geom_text(aes(label = round(mean_asim, 2), x = method, y = mean_asim + 0.008), size = 3.5) +
  coord_cartesian(ylim=c(0.4, 0.7)) +
  scale_fill_manual(name = "Answering Method", 
                    values = brewer.pal(5, name = "Spectral"),
                    labels = c("Random Search", "Cosine", "Word2Vec", "Doc2Vec - DM", "Doc2Vec - DBOW")) +
  labs(y = "Mean Answer Similarity") + theme_bw()
