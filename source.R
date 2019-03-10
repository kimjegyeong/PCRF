
# Check the number of decks from MA text

ck_cadtxt<-function(cad_list){
  deck_list<-list()
  
  for(i in 1:length(cad_list)){
    
    a<-cad_list[[i]]
    for(j in 1:nrow(a)){
      ifelse(grepl("4th deck",a[j,1],ignore.case = T), deck_list[i]<-4, deck_list[i]<-3)
    }
  }
  return(unlist(deck_list))
}


#####################################################################################
### TC ID: 05
#####################################################################################


# Cleanse text
cleanse<-function(txt){
  txt<-gsub(" ","",txt)
  return(tolower(txt))
}


# Extract main engine table from Chapter 7 documents : table

main_engine_tb<-function(spec7_table_list, std) {
  engine_list<-list()
  idx<-0
  std<-cleanse(std)
  
  for(i in 1:length(spec7_table_list)) {
    t <- spec7_table_list[[i]]
    
    for(j in 1:length(t)) {
      p <- t[[j]]
      
      if(sum(sapply(p[,1],cleanse)%in% std) >= 9) {
        idx <- idx+1
        engine_list[[idx]]<-p
      }
    }
  }
  return(engine_list)
}


#####################################################################################
### TC ID: 06
#####################################################################################


# Check type & new system information from maine engine tables


ck_type_system<-function(engine_list) {
  type_list <- list()
  
  
  for(i in 1:length(engine_list)) {
    a <- engine_list[[i]]
    
    type <- a[grep("Type",a[,1]),2]
    ifelse(grepl("B[ ]{0,1}&[ ]{0,1}W", type), type_engine <- "B & W", type_engine <- "SULZER")
    num <- a[which(a[,1] == "Number"),2]
    scr <- grepl("SCR", type)
    
    ifelse(grepl("LGI",type), LGI<-"TRUE", LGI<-"FALSE")
    ifelse(grepl("Tier III",type), tier3<-"TRUE", tier3<-"FALSE")
    
    type_list[[i]] <- c(type_engine, num, scr, LGI, tier3)
  }
  
  df <- as.data.frame(type_list)
  colnames(df) = c("type_engine", "type_number", "system_SCR", "system_LGI", "system_Tier3")
  
  return(df)
}


#####################################################################################
### TC ID: 07
#####################################################################################


# Check thermal & scrubber information from Chapter 7 documents : text
ck_system<-function(spec7_text_list){
  system_list<-list()
  
  for(i in 1:length(spec7_text_list)){
    a<-spec7_text_list[[i]]
    
    ifelse(sum(grepl("STEAM GENERATING PLANT",a)) != 0, thermal<-"Conventional",thermal<-"Thermal")
    ifelse(sum(grepl("EXHAUST GAS CLEANING SYSTEM",a)) != 0, scrubber<-"TRUE",scrubber<-"FALSE")
    
    system_list[[i]] <- c(thermal, scrubber)
  }
  return(system_list)
}


#####################################################################################
### TC ID: 08
#####################################################################################


# Check the number of comments from Chapter 7 documnets : XML

get_bi_tbl = function(x){
  
  idx = 0
  x<-unlist(x)
  writeLines(x,"x1.txt")
  x2<-readLines("x1.txt")
  #define index for find tables
  
  idxs<-grep("<w:p ",x2)
  idxe<-grep("</w:p>",x2)
  
  
  
  if(length(idxs)!= length(idxe)){
    ck<-as.data.frame(rowr::cbind.fill(idxs,idxe))
    for(i in 2:nrow(ck)){
      if(ck[i,1]<ck[i-1,2]){
        idxs<-idxs[-(i-1)]
        ck<-as.data.frame(rowr::cbind.fill(idxs,idxe))
        if(length(idxs)== length(idxe)){
          break
        }
      }
    }
  }
  #get tbl.idx
  idx1<-grep("<w:tbl>",x2)
  idx2<-grep("</w:tbl>",x2)
  #############################
  t_list<-list()
  name_list<-list()
  
  
  for(a in 1:length(idx1)){
    t<-x2[idx1[a]:idx2[a]] #define 'a'th table
    
    #check paragraph in table
    t.idx1<-idxs[which(idxs>idx1[a]&idxs<idx2[a])]-idx1[a]+1
    t.idx2<-idxe[which(idxs>idx1[a]&idxs<idx2[a])]-idx1[a]+1
    
    
    if(length(t.idx1)!= length(t.idx2)){
      ck<-as.data.frame(rowr::cbind.fill(t.idx1,t.idx2))
      for(i in 2:nrow(ck)){
        if(ck[i,1]<ck[i-1,2]){
          t.idx1<-t.idx1[-(i-1)]
          ck<-as.data.frame(rowr::cbind.fill(t.idx1,t.idx2))
          if(length(t.idx1)== length(t.idx2)){
            break
          }
        }
      }
    }
    ####
    idvec<-idxs[which(idxs<idx1[a])]
    idvec2 = idvec[order(idvec,decreasing=T)]
    
    for(i in idvec2){
      k = which(idxs==i)
      tp = x2[idxs[k]:idxe[k]]
      tp2 = tp[grep("</w:t>",tp)]
      tp3<-paste(tp2[1:length(tp2)],collapse = "")
      if(length(tp3)>0){
        tp3<-unlist(strsplit(tp3,".>")) %>% gsub("<.*","",.) %>%
          Reduce(function(x,y)paste0(x,y),.) %>% gsub(" ","",.)
        if(grepl('7[.][ ]?([0-9][0-9]?[.]?[ ]?)+.+',tp3) & grepl("\\d", unlist(strsplit(tp3,""))[1]) 
           & grepl(")",unlist(strsplit(tp3,""))[2])==F){
          name_list[a] = tp3
          break
        }
      }
      
    }
    
    cnt=0
    for(i in 1:length(t.idx1)){
      if(TRUE %in% grepl('<w:b/>',t[t.idx1[i]:t.idx2[i]]) & TRUE %in% grepl('<w:i/>',t[t.idx1[i]:t.idx2[i]])){
        cnt = cnt + 1
      }
      
    }
    t_list[a]<-cnt
    ###i###
    
  }
  
  
  df<-data.frame(tname = unlist(name_list),count = unlist(t_list)) %>% aggregate(count~.,data = .,FUN = sum)
  res = t(df$count)
  colnames(res) = df$tname
  res = as.data.frame(res)
  return(res)
}

ck_comments2<-function(x){
  
  idx = 0
  x<-unlist(x)
  writeLines(x,"x1.txt")
  x2<-readLines("x1.txt")
  
  idx1<-grep("<w:p ",x2)
  idx2<-grep("</w:p>",x2)
  
  
  if(length(idx1)!= length(idx2)){
    ck<-as.data.frame(rowr::cbind.fill(idx1,idx2))
    for(i in 2:nrow(ck)){
      if(ck[i,1]<ck[i-1,2]){
        idx1<-idx1[-(i-1)]
        ck<-as.data.frame(rowr::cbind.fill(idx1,idx2))
        if(length(idx1)== length(idx2)){
          break
        }
      }
    }
  }
  
  
  cnt=0
  
  
  for(i in 1:length(idx1)){
    if((TRUE %in% grepl("<w:b/>",x2[idx1[i]:idx2[i]])) & (TRUE %in% grepl("<w:i/>",x2[idx1[i]:idx2[i]]))){
      cnt = cnt+1
    }
  }
  
  
  return(cnt)
}


#####################################################################################
### TC ID: 09
#####################################################################################


# Check authority information from Chapter 0 documents : text
ck_auth<-function(spec0_text_list){
  authority<-list()
  for(i in 1:length(spec0_text_list)){
    x<-spec0_text_list[[i]]
    ifelse(sum(grepl("ICE.CLASS.1A",x)) != 0, authority[i]<-"ICE CLASS 1A",
           ifelse(sum(grepl("ICE.CLASS.1B",x)) != 0, authority[i]<-"ICE CLASS 1B",
                  ifelse(sum(grepl("SBG",x)) != 0 | sum(grepl("SBSN",x)) != 0, authority[i]<-"SBG/SBSN", authority[i]<-"일반")))
  }
  return(unlist(authority))
}

