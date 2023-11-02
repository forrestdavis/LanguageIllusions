# Depth-charge stat analysis

library(brms)
library(tidyverse)
library(lme4)
library(lmerTest)
library(testit)
library(emmeans)

# load the data
wd = ".."
file_name = "depth-charge_illusion_sentence_and_scores.tsv"
file_path = paste(wd, "/", file_name, sep="")

df = read_tsv(file_path)

# standardization
df_ModelMean=df %>% 
  group_by(model, measure) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating))

df = left_join(df,df_ModelMean,by=c("model","measure"))
df = df %>% 
  mutate(rating.z=(rating-mean)/sd)

models = c("bert-base-cased","gpt2","roberta-base","text-davinci-003")
measures = c("final", "ppl")

########
# GENERAL PRINCIPLE: 4 steps involved, separate the file; stat modeling; get all the results into a dataframe; add analyzable and interpretation column


############################
# PREDICTION Zero: Grammaticality
# four conditions: some..too..severe/trivial..to..ignore
############################

df_control = df %>% 
  filter(cond %in% c("posini-posadj","posini-negadj"))

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_control %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             condition=factor(cond,levels=c("posini-negadj","posini-posadj")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~condition + (1|item), data=df_iter)
    
    # store the model results into a list
    result = data.frame(summary(model_iter)$coefficients)
    result <- cbind(rownames(result), data.frame(result, row.names=NULL))
    result = result %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr)
    results = append(results, list(result))
  }
}

# get the model results into one dataframe
output_h0 = data.frame(results[1])
for (i in 2:length(results)){
  output_h0 = rbind(output_h0, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_h0 = output_h0 %>% 
  filter(rownames.result.!="(Intercept)") %>% 
  mutate(phenomenon="dc",
         hypothesis="grammatical_control",
         # need to refer to the literature
         color=case_when(measure=="LogProb"&Estimate>0&significant==1~"blue",
                         measure%in%c("ppl","final")&Estimate<0&significant==1~"blue",
                         measure=="LogProb"&Estimate<0&significant==1~"red",
                         measure%in%c("ppl","final")&Estimate>0&significant==1~"red",
                         significant==0~"white", TRUE~NA))



############################
# PREDICTION ONE: Canonical
# four conditions: depth-charge vs. grammatical/ungrammatical/ungrammatical
############################

df_Canonical = df %>% 
  filter(cond %in% c("de-implsb-qneg","negini-posadj","posini-posadj","posini-negadj")) %>% 
  mutate(condition=case_when(cond=="de-implsb-qneg"~"depth-charge",
                             cond=="negini-posadj"~"no.too.severe.ignore",
                             cond=="posini-posadj"~"some.too.severe.ignore",
                             cond=="posini-negadj"~"some.too.trivial.ignore"))

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_Canonical %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             condition=factor(condition,levels=c("depth-charge","no.too.severe.ignore","some.too.severe.ignore","some.too.trivial.ignore")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~condition + (1|item), data=df_iter)
    
    # store the model results into a list
    result = data.frame(summary(model_iter)$coefficients)
    result <- cbind(rownames(result), data.frame(result, row.names=NULL))
    result = result %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr)
    results = append(results, list(result))
  }
}

# get the model results into one dataframe
output_h1 = data.frame(results[1])
for (i in 2:length(results)){
  output_h1 = rbind(output_h1, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_h1 = output_h1 %>% 
  filter(rownames.result.!="(Intercept)") %>% 
  mutate(phenomenon="dc",
         hypothesis="canonical_depth-charge_base",
         # need to refer to the literature
         human_like=case_when(rownames.result.=="conditionsome.too.severe.ignore"&measure=="LogProb"&Estimate>0&significant==1~1,
                              rownames.result.=="conditionsome.too.severe.ignore"&measure%in%c("ppl","final")&Estimate<0&significant==1~1,
                              rownames.result.=="conditionsome.too.trivial.ignore"&measure=="LogProb"&Estimate<0&significant==1~1,
                              rownames.result.=="conditionsome.too.trivial.ignore"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                              rownames.result.=="conditionno.too.severe.ignore"&measure=="LogProb"&Estimate<0&significant==1~1,
                              rownames.result.=="conditionno.too.severe.ignore"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                              TRUE~0),
         literal_interpretation=NA)


############################
# PREDICTION TWO: Canonical
# four conditions: grammatical vs. depth-charge/ungrammatical/ungrammatical
############################

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_Canonical %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             condition=factor(condition,levels=c("some.too.severe.ignore","depth-charge","no.too.severe.ignore","some.too.trivial.ignore")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~condition + (1|item), data=df_iter)
    
    # store the model results into a list
    result = data.frame(summary(model_iter)$coefficients)
    result <- cbind(rownames(result), data.frame(result, row.names=NULL))
    result = result %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr)
    results = append(results, list(result))
  }
}

# get the model results into one dataframe
output_h2 = data.frame(results[1])
for (i in 2:length(results)){
  output_h2 = rbind(output_h2, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_h2 = output_h2 %>% 
  filter(rownames.result.!="(Intercept)") %>% 
  mutate(phenomenon="dc",
         hypothesis="canonical_grammatical_base",
         # need to refer to the literature
         human_like=NA,
         literal_interpretation=case_when(rownames.result.=="conditiondepth-charge"&measure=="LogProb"&Estimate<0&significant==1~1,
                                          rownames.result.=="conditiondepth-charge"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                                          rownames.result.=="conditionsome.too.trivial.ignore"&measure=="LogProb"&Estimate<0&significant==1~1,
                                          rownames.result.=="conditionsome.too.trivial.ignore"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                                          rownames.result.=="conditionno.too.severe.ignore"&measure=="LogProb"&Estimate<0&significant==1~1,
                                          rownames.result.=="conditionno.too.severe.ignore"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                                          TRUE~0))

#################################
# PREDICTION THREE: Pairwise plausibility
# three pairs, plausible ones should rated higher than implausible ones
#################################

df_three = df %>% 
  filter(recode %in% c("No..too..not..ignored", "No..too..not..treated",
                       "No..too..treated", "No..too..ignored",
                       "No..so..ignored", "No..so..treated"))

results = c()
for (mdl in models){
  for (msr in measures){
    ## ONE
    # subset data with model and metric
    df_1 = df_three %>%                              # plausible                # implausible
      filter(model==mdl & measure==msr & recode %in% c("No..too..not..ignored", "No..too..not..treated")) %>%
      mutate(item=factor(item),
             condition=factor(recode,levels=c("No..too..not..ignored", "No..too..not..treated")))
    
    # run the subset data with a model
    model_1 = lmer(rating.z~condition + (1|item), data=df_1)
    
    # store the model results into a list
    result_1 = data.frame(summary(model_1)$coefficients)
    result_1 <- cbind(rownames(result_1), data.frame(result_1, row.names=NULL))
    colnames(result_1)[1]="contrast"
    result_1 = result_1 %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr)
    
    ## TWO
    # subset data with model and metric
    df_2 = df_three %>%                              # plausible                # implausible
      filter(model==mdl & measure==msr & recode %in% c("No..too..treated", "No..too..ignored")) %>%
      mutate(item=factor(item),
             condition=factor(recode,levels=c("No..too..treated", "No..too..ignored")))
    
    # run the subset data with a model
    model_2 = lmer(rating.z~condition + (1|item), data=df_2)
    
    # store the model results into a list
    result_2 = data.frame(summary(model_2)$coefficients)
    result_2 <- cbind(rownames(result_2), data.frame(result_2, row.names=NULL))
    colnames(result_2)[1]="contrast"
    result_2 = result_2 %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr)
    
    ## THREE
    # subset data with model and metric
    df_3 = df_three %>%                              # plausible                # implausible
      filter(model==mdl & measure==msr & recode %in% c("No..so..ignored", "No..so..treated")) %>%
      mutate(item=factor(item),
             condition=factor(recode,levels=c("No..so..ignored", "No..so..treated")))
    
    # run the subset data with a model
    model_3 = lmer(rating.z~condition + (1|item), data=df_3)
    
    # store the model results into a list
    result_3 = data.frame(summary(model_3)$coefficients)
    result_3 <- cbind(rownames(result_3), data.frame(result_3, row.names=NULL))
    colnames(result_3)[1]="contrast"
    result_3 = result_3 %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr)
    
    results = append(results, list(result_1))
    results = append(results, list(result_2))
    results = append(results, list(result_3))
  }
}

# get the model results into one dataframe
output_h3 = data.frame(results[1])
for (i in 2:length(results)){
  output_h3 = rbind(output_h3, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_h3 = output_h3 %>% 
  filter(contrast!="(Intercept)") %>% 
  mutate(phenomenon="dc",
         hypothesis="three_pair_comparison",
         human_like=NA,
         literal_interpretation=case_when(contrast=="conditionNo..too..not..treated"&measure=="LogProb"&Estimate<0&significant==1~1,
                                          contrast=="conditionNo..too..not..treated"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                                          contrast=="conditionNo..too..ignored"&measure=="LogProb"&Estimate<0&significant==1~1,
                                          contrast=="conditionNo..too..ignored"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                                          contrast=="conditionNo..so..treated"&measure=="LogProb"&Estimate<0&significant==1~1,
                                          contrast=="conditionNo..so..treated"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                                          TRUE~0))

# combine the three dataframes togethers
colnames(output_h1)[1]="contrast"
colnames(output_h2)[1]="contrast"
final_result = rbind(output_h1, output_h2, output_h3)

#######################
# aggregate and save result files
#######################
path = ".."
file_name = "dc_modeling_result.csv"
file_path = paste(path, "/", file_name, sep="")
write_csv(final_result,file_path)



#######################
# VISUALIZE FOR EMNLP
#######################
df_dc_fig = output_h3 %>% 
  mutate(measure=case_when(measure=="ppl"~"Perplexity",measure=="final"~"Surprisal"),
         contrast=case_when(contrast=="conditionNo..too..not..treated"~"No..too..not..treated/ignored",
                            contrast=="conditionNo..too..ignored"~"No..too..ignored/treated",
                            contrast=="conditionNo..so..treated"~"No..so..treated/ignored"),
         significance=if_else(significant==1,"*",""),
         literal=if_else(literal_interpretation==1, "+",""),
         model=case_when(model=="bert-base-cased"~"BERT",model=="gpt2"~"GPT-2",
                         model=="roberta-base"~"RoBERTa",model=="text-davinci-003"~"GPT-3"),
         model=factor(model,levels=c("BERT","RoBERTa","GPT-2","GPT-3")))

fig = ggplot(data=df_dc_fig, aes(x=measure, y=Estimate, group=model)) +
  geom_bar(aes(fill=model),stat="identity", position=position_dodge(width=1)) +
  geom_text(aes(x=measure, y=Estimate, label=literal), position=position_dodge(width=1), vjust=0.8, size=5) +
  geom_text(aes(x=measure, y=Estimate, label=significance), position=position_dodge(width=1), vjust=0.4, size=5) +
  facet_wrap(vars(contrast)) +
  theme_bw()+
  theme(legend.position = "top") +
  scale_fill_manual(values=c("#66c2a5","#fc8d62","#8da0cb","#e78ac3"))

fig
