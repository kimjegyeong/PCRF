# -*- coding: utf-8 -*-
"""
Created on Sun Aug 12 16:12:36 2018

@author: Ro_Laptop
"""

import pandas as pd
import re
import string
import nltk
from gensim.models.doc2vec import Doc2Vec , TaggedDocument
from nltk.tokenize import word_tokenize


# Train Doc2Vec
def train_d2v(df, method) :
    # Tagg document
    tagged_q = [TaggedDocument(words = word_tokenize(df.iloc[i,]["q_new"].lower()), 
                               tags = ["text" + str(df.iloc[i,]["index"])]) for i in range(len(df))]
    
    tagged_a = [TaggedDocument(words = word_tokenize(df.iloc[i,]["a_new"].lower()), 
                               tags = ["text" + str(df.iloc[i,]["index"])]) for i in range(len(df))]
    
    
    
    # Set training parameter
    max_epochs = 100
    vec_size = 50
    alpha = 0.025
    
    # Create Doc2Vec model
    if method == "dm" :
        q_model = Doc2Vec(vector_size = vec_size, alpha = alpha, window = 5, min_count = 5, dm = 1)
        a_model = Doc2Vec(vector_size = vec_size, alpha = alpha, window = 5, min_count = 5, dm = 1)
    
    else :
        q_model = Doc2Vec(vector_size = vec_size, alpha = alpha, window = 5, min_count = 5, dm = 0)
        a_model = Doc2Vec(vector_size = vec_size, alpha = alpha, window = 5, min_count = 5, dm = 0)
    
    
    q_model.build_vocab(tagged_q)
    a_model.build_vocab(tagged_a)
    
    # Train
    for epoch in range(max_epochs) :
        if epoch % 10 == 0 :
            print("Iteration {}".format(epoch))
        q_model.train(tagged_q, total_examples = q_model.corpus_count, epochs = q_model.epochs)
        a_model.train(tagged_a, total_examples = a_model.corpus_count, epochs = a_model.epochs)
        
        q_model.alpha -= 0.002 # Decrease the learning rate
        q_model.min_alpha = q_model.alpha # Fix the learning rate, no decay
        a_model.alpha -= 0.002 # Decrease the learning rate
        a_model.min_alpha = q_model.alpha # Fix the learning rate, no decay
    
    return(q_model, a_model)
 
    
# Recommend Answer
def recommend_a(q_model, a_model) :
    for m in range(len(dg)) :
        
        tag = "text" + str(dg.iloc[m,]["index"])
        
        sq_idx = []
        input_q = []
        sq = []
        q_sim = []
        recommended_a = []
        actual_a = []
        a_sim = []
        for idx, sim in q_model.docvecs.most_similar(tag)[0:5] :
            sq_idx.append(idx)
            sq.append(dg[dg["index"] == int(idx.replace("text", ""))]["q"].values[0])
            q_sim.append(sim)
            input_q.append(dg[dg["index"] == int(tag.replace("text", ""))]["q"].values[0])
            
            recommended_a.append(dg[dg["index"] == int(idx.replace("text", ""))]["a"].values[0])
            actual_a.append(dg[dg["index"] == int(tag.replace("text", ""))]["a"].values[0])
            a_sim.append(a_model.docvecs.similarity(tag, idx))
        
        
        mean_asim = []
        num_filtered_asim = []
        for i in range(len(sq_idx)) :
            temp = list(range(5))
            temp.remove(i)
            
            a_sim_list = []
            for j in temp :
                a_sim_list.append(a_model.docvecs.similarity(sq_idx[i], sq_idx[j]))
                
            filtered_list = []
            for k in a_sim_list :
                filtered_list.append(k > 0.6)
                
            num_filtered_asim.append(sum(filtered_list)/len(filtered_list))
            mean_asim.append(sum(a_sim_list)/len(a_sim_list))
        
        
        
        result = pd.DataFrame(columns = ["sq_idx", "input_q", "sq", "actual_a", "recommended_a", 
                                         "mean_asim", "num_filtered_asim", "q_sim"])
        result["sq_idx"] = sq_idx
        result["input_q"] = input_q
        result["sq"] = sq
        result["actual_a"] = actual_a
        result["recommended_a"] = recommended_a
        result["mean_asim"] = mean_asim
        result["num_filtered_asim"] = num_filtered_asim
        result["q_sim"] = q_sim
        
        result["pr"] = 0.46*result["mean_asim"] + 0.04*result["num_filtered_asim"] + 0.5*result["q_sim"]
        result["a_sim"] = a_sim
        
        temp = result.loc[result["pr"].idxmax(),:].values.reshape(1,10)
        temp_df = pd.DataFrame(temp, columns = ["q_idx", "input_q", "sq", "actual_a", "recommended_a", 
                                         "mean_asim", "num_filtered_asim", "q_sim", "pr", "a_sim"])
        
        temp_df["input_q_idx"] = tag
        
        if m == 0 :
            final = temp_df.copy()
        else :
            final = pd.concat([final, temp_df], axis = 0)
            
        if m % 500 == 0 :
            print("{} of {} are done.".format(m, len(dg)))
    
    return(final)

 
def create_sim_mt(a_model) :
    
    # Create doc2vec similarity matrix
    for m in range(len(dg)) :
        if m == 0 :
            text = ["text" + str(index) for index in list(dg["index"])]
            sim_df = pd.DataFrame(index = text)
        
        tag = "text" + str(dg.iloc[m,]["index"])
        temp = a_model.docvecs.most_similar(tag, topn = dg.shape[0])
        
        idx_list = []
        sim_list = []
        for idx, sim in temp :
            idx_list.append(idx)
            sim_list.append(sim)
        idx_list.append(tag)
        sim_list.append(1)
        
        temp_df = pd.DataFrame(index = idx_list, columns = [tag])
        temp_df[tag] = sim_list
    
        sim_df = pd.merge(sim_df, temp_df, left_index = True, right_index = True)
        
        if m % 500 == 0 :
            print("{} of {} are done.".format(m, len(dg)))
            
    return(sim_df)



# First Time
nltk.download()


# Load data
file_path = "C:\\Users\\rymyu\\Dropbox\\Public\\project\\hmd\\2018\\code\\PCRF\\System\\dg_final_V5.csv"
dg = pd.read_csv(file_path, engine = "python")

# Preprocessing
pattern = re.compile('[{}]'.format(re.escape(string.punctuation)))
dg["q_new"] = dg["a"].map(lambda x : re.compile(r'[-.?!,":;()|0-9]').sub("", pattern.sub("", x)).lower())
dg["a_new"] = dg["a"].map(lambda x : re.compile(r'[-.?!,":;()|0-9]').sub("", pattern.sub("", x)).lower())


# Train model
q_dm_model, a_dm_model = train_d2v(dg, method = "dm")
q_dbow_model, a_dbow_model = train_d2v(dg, method = "dbow")


# Recommend answer
dm = recommend_a(q_dm_model, a_dm_model)
dbow = recommend_a(q_dbow_model, a_dbow_model)


dm.to_csv("doc2vec_DM.csv", encoding = "cp949", index = False)
dbow.to_csv("doc2vec_DBOW.csv", encoding = "cp949", index = False)


# Calculate similarity
dm_sim_mt = create_sim_mt(a_dm_model)
dbow_sim_mt = create_sim_mt(a_dbow_model)

dm_sim_mt.to_csv("dm_asim_mt.csv", encoding = "cp949", index = True)
dbow_sim_mt.to_csv("dbow_asim_mt.csv", encoding = "cp949", index = True)




