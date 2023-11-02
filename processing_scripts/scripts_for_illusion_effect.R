# CoNLL submission
# Aggregate: illusion condition

library(brms)
library(tidyverse)
library(lme4)
library(testit)
library(emmeans)
library(ggpattern)

#####################
# LOAD & PROCESS THE DATA
#####################
# COMPARATIVE ILLUSION
wd = "please set your own wd here"
file_name = "comparative_illusion_sentence_and_scores.tsv"
file_path = paste(wd, "/", file_name, sep="")

df_ci = read_tsv(file_path)

# standardization
df_ModelMean_ci=df_ci %>% 
  group_by(model, measure) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating))

df_ci = left_join(df_ci,df_ModelMean_ci,by=c("model","measure"))
df_ci = df_ci %>% 
  mutate(rating.z=(rating-mean)/sd)

# mutate and select
df_ci_il = df_ci %>% 
  filter(`subject number`!="plural"&subject=="pronoun") %>% 
  filter(!(`subject number`=="singular"&repeatability=="nonrepeatable")) %>% 
  mutate(condition=case_when(repeatability=="repeatable"&`subject number`=="singular"~"illusion",
                             repeatability=="repeatable"&`subject number`=="control"~"acceptable",
                             repeatability=="nonrepeatable"~"unacceptable")) %>% 
  select(item, sent, model, measure, rating.z, condition) %>% 
  mutate(phenomenon="Comparative Illusion")

# DEPTH-CHARGE CONDITION
wd = "please set your own wd here"
file_name = "depth-charge_illusion_sentence_and_scores.tsv"
file_path = paste(wd, "/", file_name, sep="")

df_dc = read_tsv(file_path)

# standardization
df_ModelMean_dc=df_dc %>% 
  group_by(model, measure) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating))

df_dc = left_join(df_dc,df_ModelMean_dc,by=c("model","measure"))
df_dc = df_dc %>% 
  mutate(rating.z=(rating-mean)/sd)

# mutate and select
df_dc_il = df_dc %>% 
  filter(recode%in%c("No..too..ignored","Some..too trivial..ignored","Some..too severe..ignored")) %>% 
  mutate(condition=case_when(recode=="No..too..ignored"~"illusion",
                             recode=="Some..too trivial..ignored"~"unacceptable",
                             recode=="Some..too severe..ignored"~"acceptable")) %>% 
  select(item, sent, model, measure, rating.z, condition) %>% 
  mutate(phenomenon="Depth-charge Illusion")

# NPI ILLUSION
wd = "please set your own wd here"
file_name = "npi_illusion_sentence_and_scores.tsv"
file_path = paste(wd, "/", file_name, sep="")

df_npi = read_tsv(file_path) %>% 
  mutate(measure=case_when(measure=="ever"~"final",measure=="LogProb"~"LogProb",measure=="ppl"~"ppl"))

# standardization
df_ModelMean_npi=df_npi %>% 
  group_by(model, measure) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating))

df_npi = left_join(df_npi,df_ModelMean_npi,by=c("model","measure"))
df_npi = df_npi %>% 
  mutate(rating.z=(rating-mean)/sd)

# mutate and select
df_npi_il = df_npi %>% 
  filter(expNum==1&condition!="Relative Didn't") %>% 
  mutate(condition=case_when(condition=="Relative No"~"illusion",
                             condition=="Licensor Absent"~"unacceptable",
                             condition=="Matrix No"~"acceptable"),
         item=itemNum) %>% 
  select(item, sent, model, measure, rating.z, condition) %>% 
  mutate(phenomenon="NPI Illusion")

df = rbind(df_ci_il, df_dc_il, df_npi_il) %>% 
  filter(measure!="LogProb")


#####################
# VISUALIZATION
#####################
# CREATE functions to bootstrap confidence intervals
resample_mean <- function(values){
  samples <- sample(values, replace = TRUE)
  return(mean(samples, na.rm=TRUE))
}

resample_means <- function(values, num_resamples){
  replicate(num_resamples, resample_mean(values))
}

ci_lower <- function(values) quantile(values, 0.025)
ci_upper <- function(values) quantile(values, 0.975)

# MAKE the summ file
df_summ = df %>% 
  group_by(phenomenon, condition, model, measure) %>% 
  summarise(rating_mean=mean(rating.z),
            lower_ci = ci_lower(rating.z),
            upper_ci = ci_upper(rating.z)) %>% 
  mutate(measure=case_when(measure=="ppl"~"Perplexity",measure=="final"~"Surprisal"),
         model=case_when(model=="bert-base-cased"~"BERT",model=="gpt2"~"GPT-2",
                         model=="roberta-base"~"RoBERTa",model=="text-davinci-003"~"GPT-3"),
         model=factor(model,levels=c("BERT","RoBERTa","GPT-2","GPT-3")))

# GRAPH
figure = ggplot(data=df_summ,aes(x=condition, y=rating_mean, group=model)) +
  geom_point(aes(color=model), size=2, position = position_dodge(width=0.6)) +
  geom_errorbar(aes(x=condition, ymin=lower_ci, ymax=upper_ci, color=model), width=0.3, position = position_dodge(width=0.6)) +
  geom_line(aes(color=model), size=0.5, alpha=0.5, linetype="dashed", position = position_dodge(width=0.6)) +
  facet_grid(measure~phenomenon) +
  xlab("Conditions") +
  ylab("Standardized scores") +
  theme_bw() +
  scale_color_manual(values=c("#66c2a5","#fc8d62","#8da0cb","#e78ac3"))

figure


#####################
# STAT MODELING
#####################
models = c("bert-base-cased","gpt2","roberta-base","text-davinci-003")
measures = c("final","ppl")
illusion = c("Comparative Illusion","Depth-charge Illusion","NPI Illusion")

# stat modeling
results = c()
for (ill in illusion){
  for (mdl in models){
    for (msr in measures){
      # subset data with model and metric
      df_iter = df %>% 
        filter(model==mdl & measure==msr & phenomenon==ill) %>%
        mutate(item=factor(item),
               condition=factor(condition,levels=c("illusion","acceptable", "unacceptable")))
      
      # run the subset data with a model
      model_iter = lmer(rating.z~condition + (1|item), data=df_iter)
      
      # store the model results into a list
      result = data.frame(summary(model_iter)$coefficients)
      result <- cbind(rownames(result), data.frame(result, row.names=NULL))
      result = result %>%
        mutate(significant=if_else(`Pr...t..`<0.05,1,0),
               model=mdl,
               measure=msr,
               phenomenon=ill)
      results = append(results, list(result))
  }
}}

# get the model results into one dataframe
output_h0 = data.frame(results[1])
for (i in 2:length(results)){
  output_h0 = rbind(output_h0, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_h0 = output_h0 %>% 
  filter(rownames.result.!="(Intercept)") %>% 
  mutate(case="all three phenomena",
         hypothesis="illusion_effects",
         # need to refer to the literature
         illusion_effect=case_when(rownames.result.=="conditionunacceptable"&Estimate>0&significant==1~1,TRUE~NA),
         literal_interpretation=case_when(rownames.result.=="conditionunacceptable"&Estimate<0&significant==1~1,
                                          rownames.result.=="conditionunacceptable"&significant==0~1,
                                          TRUE~NA),
         even_better_than_acceptable=case_when(rownames.result.=="conditionacceptable"&Estimate>0&significant==1~1,TRUE~NA))

#########################
# MODEL VISUALIZATION
#########################
df_illusion_stat = output_h0 %>% 
  filter(rownames.result.=="conditionunacceptable") %>% 
  mutate(measure=case_when(measure=="ppl"~"Perplexity",measure=="final"~"Surprisal"),
         star=case_when(illusion_effect==1~"+", significant==0~".",TRUE~"-"),
         model=case_when(model=="bert-base-cased"~"BERT",model=="gpt2"~"GPT-2",
                         model=="roberta-base"~"RoBERTa",model=="text-davinci-003"~"GPT-3"),
         model=factor(model,levels=c("BERT","RoBERTa","GPT-2","GPT-3")))

# There are better ways for this: use patterns for 
fig2 =ggplot(data=df_illusion_stat, aes(x=measure, y=Estimate)) +
  geom_bar(aes(fill=model),stat="identity", position=position_dodge(width=1)) +
  geom_text(aes(x=measure, label=star,group=model), position=position_dodge(width=1), size=8) +
  facet_wrap(vars(phenomenon)) +
  theme_bw()+
  scale_fill_manual(values=c("#66c2a5","#fc8d62","#8da0cb","#e78ac3"))

fig2
