#####################################################################################
### Test Case V1
### 2018. 10. 20
#####################################################################################



#####################################################################################
### TC ID: 01 - LOAD DATA FOR TRAINING
#####################################################################################

#set seed
set.seed(1004)

#LOAD PKGS 
pkgs <- c("xgboost",'dplyr','docxtractr','ggplot2','data.table')
sapply(pkgs, require, character.only = T)

# Read dataframe for training and select variables
df = fread('D:/UNIST_Junmo/gov project/2018/TC-TS/df.csv',stringsAsFactors = T)
train.df = df %>% filter(Act.F != min(Act.F)) %>% as.data.frame #remove extraoridnary act.f(0.43)


#####################################################################################
### TC ID: 02 - TRAINING WITH XGBOOST CROSS-VALIDATION AND PLOTTING
#####################################################################################

#Training by cross-validation
cv.model = xgb.cv(data = select(train.df,-Act.F) %>% data.matrix,label = train.df$Act.F,
                  nfold = 5,verbose = F,nround = 50)

#Show training log and plotting
eval.log = cv.model$evaluation_log
data.frame(num_of_tree = c(1:nrow(eval.log),1:nrow(eval.log)),
           RMSE = c(eval.log$train_rmse_mean,eval.log$test_rmse_mean),
           Class = c(rep('train',nrow(eval.log)),rep('test',nrow(eval.log)))) %>%
  ggplot(aes(num_of_tree,RMSE,colour = Class))+
  geom_point(size = 0.9)+
  stat_smooth(geom = 'line', alpha = 0.3, size = 2, se = F) +
  theme(legend.title        = element_blank(),
        legend.position     = c(0.85,0.85),
        legend.background   = element_rect(fill = "transparent", 
                                           colour = "transparent"), 
        legend.text         = element_text(size = 10.5))+
  labs(x = 'The number of trees', y = 'Error')+
  ggtitle('XGBOOST Cross Validation Result',
          subtitle = eval.log$test_rmse_mean %>% last %>% paste('RMSE : ',.))

model = xgboost(data = select(train.df,-Act.F) %>% data.matrix,label = train.df$Act.F,
                verbose = F,nround = 50)

#####################################################################################
### TC ID: 03 - LOAD DATA FOR PREDICTION
#####################################################################################

#load functions from source script to read data
source('D:/UNIST_Junmo/gov project/2018/TC-TS/source.R',encoding = 'UTF-8')


#LOAD test data
path = 'D:/UNIST_Junmo/gov project/2018/TC-TS/test data' #enter your path 

fnames = list.files(path)
cad = fnames[grep('.txt',fnames)] %>% paste0(path,"/",.) %>% read.delim #cad
spec_0 = list((fnames[grep('.docx',fnames)][1] %>% paste0(path,"/",.) %>% read_docx)$docx %>% as.character) #spec_0 txt
spec_7 = list((fnames[grep('.docx',fnames)][2] %>% paste0(path,"/",.) %>% read_docx)$docx %>% as.character) #spec_7 txt
spec_7.tbl = list(fnames[grep('.docx',fnames)][2] %>% paste0(path,"/",.) %>% read_docx %>% docx_extract_all_tbls) #spec_7 tbl
capa = '50K' #enter capa of ship
type = 'CT'  #enter type of ship



#####################################################################################
### TC ID: 04 - READ NEW SPEC
#####################################################################################


#DECK NO.
deck=ifelse(cad %>% tolower %>% grepl('4th deck'),4,3)

#Engine std
std<- c("particular",                  "number",                      "type",                       
        "output",                      "poweroutput(kw)"  ,           "enginerevolution(rpm)"  ,    
        "cyl.no.xborexstroke" ,        "specificfueloilconsumption",  "fueloil"  ,                  
        "turbocharger" ,               "aircooler"     ,              "turninggear"   ,             
        "governor" ,                   "auxiliaryblower"  ,           "brakehorsepower(kw)" ,       
        "brakehorsepower(kw)kw)" ,     "*specificfueloilconsumption")


# Extract main engine tables
engine_list <- main_engine_tb(spec_7.tbl, std)


#type of system
type_system <- ck_type_system(engine_list) 


# Split extracted spec as type engine, type number, SCR, LGI and Tier 3
type_engine <- type_system$type_engine
type_number <- type_system$type_number
system_scr <- type_system$system_SCR
system_lgi <- type_system$system_LGI
system_tier3 <- type_system$system_Tier3


# Extract other system spec ----
system <- ck_system(spec_7)

# Split extracted spec as Thermal and Scrubber----
system_thermal<-list()
system_scrubber<-list()
for(i in 1:length(system)) {
  system_thermal[i] <- system[[i]][1]
  system_scrubber[i] <- system[[i]][2]
}

# Classify system category either new system or not
system_list <- ifelse(system_scr == "TRUE" | system_scrubber == "TRUE" | 
                        system_lgi == "TRUE" | system_tier3 == "TRUE", "New System",
                      ifelse(system_thermal == "Thermal", "Thermal", "Conventional"))

# Count the number of comments in table

bi_tbl=get_bi_tbl(spec_7)
bi_0 = ck_comments2(spec_0)
bi_7 = ck_comments2(spec_7)

# Extract authority spec
auth<-ck_auth(spec_0)





#####################################################################################
### TC ID: 05 - MERGE SPECS TO ONE AND PREDICT FACTOR
#####################################################################################

# Merge all specs as data frame
spec_df <- data.frame(Type = type, Capa = capa,Deck = deck,
                      System = system_list,
                      Type.1 = unlist(type_engine),
                      Engine = (type_number %>% as.character%>% tm::removePunctuation() %>% strsplit(' ') %>% unlist)[1],
                      Shaft = (type_number %>% as.character%>% tm::removePunctuation() %>% strsplit(' ') %>% unlist)[2],
                      B.I_0 = bi_0,B.I_7 = bi_7,Authority = auth) %>% cbind(bi_tbl) #Extracted SPEC

nms = model$feature_names[(model$feature_names %in% names(spec_df))==F]

mat = matrix(ncol = length(nms),nrow = 1,NA) %>% data.frame #replace unmatched column to NA
names(mat) = nms

spec_df = cbind(spec_df,mat)
spec_df = spec_df[,match(model$feature_names,names(spec_df))] #ordering colnames by feature name stored in model

predict(model,spec_df %>% data.matrix) #predicted factor of given spec
