# NPI illusion stat analysis

library(brms)
library(tidyverse)
library(lme4)
library(testit)
library(emmeans)

# load the data
wd = ".."
file_name = "npi_illusion_sentence_and_scores.tsv"
file_path = paste(wd, "/", file_name, sep="")

df = read_tsv(file_path)

# standardization
df_ModelMean=df %>% 
  group_by(model, measure) %>% 
  summarise(mean=mean(rating),
            sd=sd(rating))

df = left_join(df,df_ModelMean,by=c("model","measure"))
df = df %>% 
  mutate(rating.z=(rating-mean)/sd) %>% 
  mutate(measure=case_when(measure=="ever"~"final",measure=="LogProb"~"LogProb",measure=="ppl"~"ppl"))

models = c("bert-base-cased","gpt2","roberta-base","text-davinci-003")
measures = c("final", "ppl")


############################
# PREDICTION Zero: Grammaticality
# four conditions: Matrix No & Licensor Absent
############################

df_control = df %>% 
  filter(condition %in% c("Matrix No","Licensor Absent") & expNum==1)

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_control %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(itemNum=factor(itemNum),
             condition=factor(condition,levels=c("Licensor Absent","Matrix No")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~condition + (1|itemNum), data=df_iter)
    
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
  mutate(phenomenon="npi",
         hypothesis="grammatical_control",
         # need to refer to the literature
         color=case_when(measure=="LogProb"&Estimate>0&significant==1~"blue",
                         measure%in%c("ppl","final")&Estimate<0&significant==1~"blue",
                         measure=="LogProb"&Estimate<0&significant==1~"red",
                         measure%in%c("ppl","final")&Estimate>0&significant==1~"red",
                         significant==0~"white", TRUE~NA))

########################
# PREDICTION ONE: Canonical_1
# Human like prediction: Relative No is higher than Licensor Absent but lower than Matrix No
########################

df_Canonical = df %>% 
  filter(expNum==1&condition!="Relative Didn't")

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_Canonical %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(itemNum),
             condition=factor(condition, levels=c("Relative No","Matrix No","Licensor Absent")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~condition + (1|item), data=df_iter)
    
    # store the model results into a list
    result = data.frame(summary(model_iter)$coefficients)
    result <- cbind(rownames(result), data.frame(result, row.names=NULL))
    colnames(result)[1] = "contrast"
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
  filter(contrast!="(Intercept)") %>% 
  mutate(phenomenon="npi",
         hypothesis="canonical",
         # need to refer to the literature
         human_like=case_when(contrast=="conditionMatrix No"&measure=="LogProb"&Estimate>0&significant==1~1,
                              contrast=="conditionMatrix No"&measure%in%c("ppl","final")&Estimate<0&significant==1~1,
                              contrast=="conditionLicensor Absent"&measure=="LogProb"&Estimate<0&significant==1~1,
                              contrast=="conditionLicensor Absent"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                              TRUE~0),
         # if there isn't a difference between grammatical controls and the illusions
         # we call the literal_interpretation is 0.
         literal_interpretation=case_when(contrast=="conditionMatrix No"&measure=="LogProb"&Estimate>0&significant==1~1,
                                          contrast=="conditionMatrix No"&measure%in%c("ppl","final")&Estimate<0&significant==1~1,
                                          contrast=="conditionLicensor Absent"&measure=="LogProb"&(Estimate>0&significant==1|significant==0)~1,
                                          contrast=="conditionLicensor Absent"&measure%in%c("ppl","final")&(Estimate<0&significant==1|significant==0)~1,
                                          TRUE~0))


########################
# PREDICTION TWO: Grammatical controls
# Human like prediction: Matrix No should be better than Licensor Absent
########################

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_Canonical %>% 
      filter(model==mdl & measure==msr & condition %in% c("Matrix No","Licensor Absent")) %>%
      mutate(item=factor(itemNum),
             condition=factor(condition, levels=c("Matrix No","Licensor Absent")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~condition + (1|item), data=df_iter)
    
    # store the model results into a list
    result = data.frame(summary(model_iter)$coefficients)
    result <- cbind(rownames(result), data.frame(result, row.names=NULL))
    colnames(result)[1] = "contrast"
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
  filter(contrast!="(Intercept)") %>% 
  mutate(phenomenon="npi",
         hypothesis="grammatical_control",
         # need to refer to the literature
         human_like=case_when(contrast=="conditionLicensor Absent"&measure=="LogProb"&Estimate<0&significant==1~1,
                              contrast=="conditionLicensor Absent"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                              TRUE~0),
         # if there isn't a difference between grammatical controls and the illusions
         # we call the literal_interpretation is 0.
         literal_interpretation=case_when(contrast=="conditionLicensor Absent"&measure=="LogProb"&Estimate<0&significant==1~1,
                                          contrast=="conditionLicensor Absent"&measure%in%c("ppl","final")&Estimate>0&significant==1~1,
                                          TRUE~0))


########################
# PREDICTION THREE: More licensors
# Human like prediction: Like what is prediced from Orth et al. 2021
########################

df_more_licensors = df %>% 
  filter(expNum%in%c(1,2,3))

# there are multiple licensors
licensors = c("Relative Did Not","Relative Didn't","Relative Never")

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    for (lcnsr in licensors){
    # subset data with model and metric
    df_iter = df_more_licensors %>% 
      filter(model==mdl & measure==msr & condition %in% c(lcnsr, "Matrix No","Licensor Absent","Relative No")) %>%
      mutate(item=factor(itemNum),
             condition=factor(condition, levels=c(lcnsr, "Matrix No","Licensor Absent","Relative No")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~condition + (1|item), data=df_iter)
    
    # store the model results into a list
    result = data.frame(summary(model_iter)$coefficients)
    result <- cbind(rownames(result), data.frame(result, row.names=NULL))
    colnames(result)[1] = "contrast"
    result = result %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr,
             baseline=lcnsr)
    results = append(results, list(result))
    }
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
  mutate(phenomenon="npi",
         hypothesis="more_licensors",
         # need to refer to the literature
         human_like=case_when(contrast=="conditionMatrix No"&measure=="LogProb"&Estimate>0&significant==1~1,
                              contrast=="conditionMatrix No"&measure%in%c("ppl","final")&Estimate<0&significant==1~1,
                              # According to Orth et al. 2021, these licensors are rated worse than the Relative NO.
                              contrast=="conditionRelative No"&measure=="LogProb"&Estimate>0&significant==1~1,
                              contrast=="conditionRelative No"&measure%in%c("ppl","final")&Estimate<0&significant==1~1,
                              contrast=="conditionLicensor Absent"~NA,
                              TRUE~0),
         # if there isn't a difference between grammatical controls and the illusions
         # we call the literal_interpretation is 0.
         literal_interpretation=case_when(contrast=="conditionMatrix No"&measure=="LogProb"&Estimate>0&significant==1~1,
                                          contrast=="conditionMatrix No"&measure%in%c("ppl","final")&Estimate<0&significant==1~1,
                                          # According to Orth et al. 2021, these licensors are rated worse than the Relative NO.
                                          contrast=="conditionRelative No"~NA,
                                          contrast=="conditionLicensor Absent"&measure=="LogProb"&(Estimate>0&significant==1|significant==0)~1,
                                          contrast=="conditionLicensor Absent"&measure%in%c("ppl","final")&(Estimate<0&significant==1|significant==0)~1,
                                          TRUE~0))



########################
# PREDICTION Four: Hierarchical relation
# Human like prediction: The hierarchical position and not a single quantifier
########################

df_hierachy = df %>% 
  filter(expNum==6&condition%in%c("Embedded No","Embedded Not a Single","Relative Subject No","Relative Subject Not A Single")) %>% 
  mutate(position=case_when(condition=="Embedded No"|condition=="Embedded Not a Single"~"embed",
                            condition=="Relative Subject No"|condition=="Relative Subject Not A Single"~"subject"),
         determiner=case_when(condition=="Embedded No"|condition=="Relative Subject No"~"no",
                              condition=="Embedded Not a Single"|condition=="Relative Subject Not A Single"~"not a single"))

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_hierachy %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(itemNum),
             position=factor(position, levels=c("subject","embed")),
             determiner=factor(determiner, levels=c("no","not a single")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~position+determiner + (1|item), data=df_iter)
    
    
    # store the model results into a list
    result_pos = data.frame(emmeans(model_iter, specs=pairwise~position)$contrasts %>% summary(infer=T))
    result_det = data.frame(emmeans(model_iter, specs=pairwise~determiner)$contrasts %>% summary(infer=T))
    result_pos = result_pos %>%
      mutate(significant=if_else(p.value<0.05,1,0),
             model=mdl,
             measure=msr)
    result_det = result_det %>%
      mutate(significant=if_else(p.value<0.05,1,0),
             model=mdl,
             measure=msr)
    results = append(results, list(result_pos))
    results = append(results, list(result_det))
  }
}

# get the model results into one dataframe
output_h4 = data.frame(results[1])
for (i in 2:length(results)){
  output_h4 = rbind(output_h4, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_h4 = output_h4 %>% 
  mutate(phenomenon="npi",
         hypothesis="position_determiner",
         # need to refer to the literature
         human_like=case_when(contrast=="subject - embed"&significant==0~1,
                              contrast=="no - not a single"&measure=="LogProb"&estimate>0&significant==1~1,
                              contrast=="no - not a single"&measure%in%c("ppl","final")&estimate<0&significant==1~1,
                              TRUE~0),
         # we call the literal_interpretation is 0.
         literal_interpretation=NA)

############################
# SAVE the files
############################
final_result = bind_rows(output_h1, output_h2, output_h3, output_h4)
path = ".."
file_name = "npi_modeling_result.csv"
file_path = paste(path, "/", file_name, sep="")
write_csv(final_result,file_path)

############################
# VISUALIZATION
############################
df_npi_fig = output_h3 %>% 
  filter(contrast=="conditionLicensor Absent") %>% 
  mutate(measure=case_when(measure=="ppl"~"Perplexity",measure=="final"~"Surprisal"),
         illusion_effect=case_when(significant==1&Estimate>0~"*", significant==0~"-", significant==1&Estimate<0~"+"))

fig = ggplot(data=df_npi_fig, aes(x=measure, y=Estimate, group=model)) +
  geom_bar(aes(fill=model),stat="identity", position=position_dodge(width=1)) +
  geom_text(aes(x=measure, y=Estimate, label=illusion_effect), position=position_dodge(width=1), vjust=0.8, size=8) +
  facet_wrap(vars(baseline)) +
  theme_bw()+
  theme(legend.position = "top") +
  scale_fill_manual(values=c("#66c2a5","#fc8d62","#8da0cb","#e78ac3"))


