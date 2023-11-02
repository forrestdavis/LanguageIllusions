# Comparative illusion stat analysis

library(brms)
library(tidyverse)
library(lme4)
library(lmerTest)
library(testit)
library(emmeans)

# load the data
wd = ""
file_name = "comparative_illusion_sentence_and_scores.tsv"
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

########################
# PREDICTION ZERO
# Grammaticality
########################
models = c("bert-base-cased","gpt2","roberta-base","text-davinci-003")
measures = c("final", "ppl","LogProb")

df_controls = df %>% 
  filter(`subject number`=="control"&subject=="pronoun")

results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_controls %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             condition=factor(repeatability, levels=c("nonrepeatable","repeatable")))
    
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
  mutate(phenomenon="ci",
         hypothesis="grammatical_control",
         # need to refer to the literature
         color=case_when(measure=="LogProb"&Estimate>0&significant==1~"blue",
                         measure%in%c("ppl","final")&Estimate<0&significant==1~"blue",
                         measure=="LogProb"&Estimate<0&significant==1~"red",
                         measure%in%c("ppl","final")&Estimate>0&significant==1~"red",
                         significant==0~"white", TRUE~NA))


########################
# PREDICTION ONE
# H0: no diff for more..than I/many..more than I.
########################
df_Canonical = df %>% 
  filter(repeatability=="repeatable"&`subject number`!="plural"&subject=="pronoun")

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_Canonical %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             condition=factor(`subject number`))
    
    assert("Data file is not correct.", length(df_iter$sent)==64)
    
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
output_0 = data.frame(results[1])
for (i in 2:length(results)){
  output_0 = rbind(output_0, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_0 = output_0 %>% 
  filter(rownames.result.=="conditionsingular") %>% 
  mutate(phenomenon="ci",
         hypothesis="canonical",
         # need to refer to the literature
         human_like=if_else(significant==0,1,0),
         # if there isn't a difference between grammatical controls and the illusions
         # we call the literal_interpretation is 0.
         literal_interpretation=if_else(measure=="LogProb"&Estimate<0&significant==1,1,
                                        if_else(measure%in%c("ppl","final")&Estimate>0&significant==1,1,0)))

########################
# PREDICTION TWO
# H0: For pronouns, NO DIFF between Good controls and Bad controls.
# Human behaviors: There is A DIFF.
########################

# prepare for the datafile
df_pro_control = df %>% 
  filter(`subject number`=="control"&subject=="pronoun")

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_pro_control %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             condition=factor(repeatability))
    
    assert("Data file is not correct.", length(df_iter$sent)==64)
    
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

#TODO: Stop to look at the document before designing the following
# add other columns to indicate the illusion and the hypothesis
output_h2 = output_h2 %>% 
  filter(rownames.result.!="(Intercept)") %>% 
  mutate(phenomenon="ci",
         hypothesis="pronoun_control_comparison",
         #TODO Use human research to manually determine how to judge human_like, and literal_interpration
         ##### WATCH OUT FOR THE DIRECTION DEPENDING ON DIFF METRICS
         human_like=if_else(measure=="LogProb"&Estimate>0&significant==1,1,
                            if_else(measure%in%c("ppl","final")&Estimate<0&significant==1,1,0)),
         literal_interpretation=if_else(measure=="LogProb"&Estimate>0&significant==1,1,
                                        if_else(measure%in%c("ppl","final")&Estimate<0&significant==1,1,0))
         )

########################
# PREDICTION THREE: illusion?
# Prediction: For pronouns, all illusion sentences, are better at the bad controls 
#             but NO DIFF between illusion sentences and good controls.
########################
# prepare for the datafile
df_pro_illusion = df %>% 
  filter(subject=="pronoun")

# stat modeling
results_1 = c()
results_2 = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_pro_illusion %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item))
    
    df_1 = df_iter %>% 
      # illusion sentences with good controls
      filter(!(repeatability=="nonrepeatable"&`subject number`=="control")) %>% 
      mutate(condition=if_else(repeatability=="repeatable"&`subject number`=="control", "control","illusion"),
             condition=factor(condition,levels=c("control","illusion")))
    
    df_2 = df_iter %>% 
      # illusion sentences with bad controls
      filter(!(repeatability=="repeatable"&`subject number`=="control")) %>% 
      mutate(condition=if_else(repeatability=="nonrepeatable"&`subject number`=="control", "control","illusion"),
             condition=factor(condition,levels=c("control","illusion")))
    
    # run the subset data with a model
    model_iter_1 = lmer(rating.z~condition + (1|item), data=df_1)
    model_iter_2 = lmer(rating.z~condition + (1|item), data=df_2)
    
    # model_iter_1
    # store the model results into a list
    result = data.frame(summary(model_iter_1)$coefficients)
    result <- cbind(rownames(result), data.frame(result, row.names=NULL))
    result = result %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr)
    results_1 = append(results_1, list(result))
    
    
    # model_iter_2
    result = data.frame(summary(model_iter_2)$coefficients)
    result <- cbind(rownames(result), data.frame(result, row.names=NULL))
    result = result %>%
      mutate(significant=if_else(`Pr...t..`<0.05,1,0),
             model=mdl,
             measure=msr)
    results_2 = append(results_2, list(result))
  }
}

# get the model results into one dataframe
output_h3_1 = data.frame(results_1[1]) # illusion compared to good sentences
for (i in 2:length(results_1)){
  output_h3_1 = rbind(output_h3_1, data.frame(results_1[i]))
}

output_h3_2 = data.frame(results_2[1]) # illusion compared to bad sentences
for (i in 2:length(results_2)){
  output_h3_2 = rbind(output_h3_2, data.frame(results_2[i]))
}

#TODO: Stop to look at the document before designing the following
# add other columns to indicate the illusion and the hypothesis
output_h3_1 = output_h3_1 %>% 
  filter(rownames.result.!="(Intercept)") %>% 
  mutate(phenomenon="ci",
         hypothesis="pronoun_illusion_with_good_control",
         #TODO Use human research to manually determine how to judge human_like, and literal_interpration
         ##### WATCH OUT FOR THE DIRECTION DEPENDING ON DIFF METRICS
         human_like=case_when(significant==0~1,
                              measure=="LogProb"&Estimate>0&significant==1~1,
                              measure%in%c("ppl","final")&Estimate<0&significant==1~1,
                              TRUE~0), # should be NO DIFF
         
         # should be WORSE than good controls
         literal_interpretation=if_else(measure=="LogProb"&Estimate<0&significant==1,1,
                                        if_else(measure%in%c("ppl","final")&Estimate>0&significant==1,1,0))
         
  )

output_h3_2 = output_h3_2 %>% 
  filter(rownames.result.!="(Intercept)") %>% 
  mutate(phenomenon="ci",
         hypothesis="pronoun_illusion_with_bad_control",
         #TODO Use human research to manually determine how to judge human_like, and literal_interpration
         ##### WATCH OUT FOR THE DIRECTION DEPENDING ON DIFF METRICS
        
         # all illusion sentences should be better than bad control.
         human_like=if_else(measure=="LogProb"&Estimate>0&significant==1,1,
                            if_else(measure%in%c("ppl","final")&Estimate<0&significant==1,1,0)),
         
         # should either be the same, or worse
         literal_interpretation=case_when(significant==0~1,
                                          significant==1&measure=="LogProb"&Estimate<0~1,
                                          significant==1&measure%in%c("ppl","final")&Estimate>0~1,
                                          TRUE~0)
         )

########################
# PREDICTION FOUR: subject to REPEATABILITY/SUBJECT-NUMBER effect?
# Prediction: For pronouns, AN EFFECT for repeatability, NO EFFECT for number
########################
# separate files
df_pro_effect = df %>% 
  filter(subject=="pronoun"&`subject number`%in%c("singular","plural"))

# run models
results=c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_pro_effect %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             number=factor(`subject number`, levels=c("singular","plural")),
             repeatability=factor(repeatability, levels=c("nonrepeatable","repeatable")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~ number + repeatability + (1|item), data=df_iter)
    
    # store the model results into a list
    result_repeat = data.frame(emmeans(model_iter, specs=pairwise~repeatability)$contrasts %>% summary(infer=T))
    result_number = data.frame(emmeans(model_iter, specs=pairwise~number)$contrasts %>% summary(infer=T))
    result_repeat = result_repeat %>%
      mutate(significant=if_else(p.value<0.05,1,0),
             model=mdl,
             measure=msr)
    result_number = result_number %>%
      mutate(significant=if_else(p.value<0.05,1,0),
             model=mdl,
             measure=msr)
    results = append(results, list(result_repeat))
    results = append(results, list(result_number))
  }
}

# tidy up the results
# get the model results into one dataframe
output_h4 = data.frame(results[1])
for (i in 2:length(results)){
  output_h4 = rbind(output_h4, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_h4 = output_h4 %>% 
  mutate(phenomenon="ci",
         hypothesis="pronoun_repeatable_number",
         
         # need to refer to the literature
         # For pronouns, repeat matters, number doesn't.
         human_like=case_when(contrast=="nonrepeatable - repeatable"&significant==1&measure=="LogProb"&estimate<0~1,
                              contrast=="nonrepeatable - repeatable"&significant==1&measure%in%c("ppl","final")&estimate>0~1,
                              contrast=="singular - plural"&significant==0~1,
                              TRUE~0),
         # since all of these sentences should be illusion sentences, so we use NA to indicate there isn't a prediction.
         literal_interpretation=NA)


########################
# PREDICTION FIVE: NP
# Prediction: For NPs, all the illusion sentences should be worse than the good controls
########################

# separate the data
df_np_illusion = df %>% 
  filter(subject=="np")

# stat modeling
results = c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_np_illusion %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             condition=if_else(`subject number`=="control", "control","illusion"),
             condition=factor(condition,levels=c("control","illusion")))
    
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
output_h5 = data.frame(results[1]) # illusion compared to good sentences
for (i in 2:length(results)){
  output_h5 = rbind(output_h5, data.frame(results[i]))
}


#TODO: Stop to look at the document before designing the following
# add other columns to indicate the illusion and the hypothesis
output_h5 = output_h5 %>% 
  filter(rownames.result.!="(Intercept)") %>% 
  mutate(phenomenon="ci",
         hypothesis="np_illusion_with_good_control",
         #TODO Use human research to manually determine how to judge human_like, and literal_interpration
         ##### WATCH OUT FOR THE DIRECTION DEPENDING ON DIFF METRICS
         
         # illusion sentences worse than good controls
         human_like=if_else(measure=="LogProb"&Estimate<0&significant==1,1,
                            if_else(measure%in%c("ppl","final")&Estimate>0&significant==1,1,0)),
         
         # worse than good controls
         literal_interpretation=if_else(measure=="LogProb"&Estimate<0&significant==1,1,
                                        if_else(measure%in%c("ppl","final")&Estimate>0&significant==1,1,0))
  )

########################
# PREDICTION SIX: NP
# Prediction: For NPs, AN EFFECT for repeatability, AN EFFECT for number
########################
# separate files
df_np_effect = df %>% 
  filter(subject=="np"&`subject number`%in%c("singular","plural"))

# run models
results=c()
for (mdl in models){
  for (msr in measures){
    # subset data with model and metric
    df_iter = df_np_effect %>% 
      filter(model==mdl & measure==msr) %>%
      mutate(item=factor(item),
             number=factor(`subject number`, levels=c("singular","plural")),
             repeatability=factor(repeatability, levels=c("nonrepeatable","repeatable")))
    
    # run the subset data with a model
    model_iter = lmer(rating.z~ number + repeatability + (1|item), data=df_iter)
    
    # store the model results into a list
    result_repeat = data.frame(emmeans(model_iter, specs=pairwise~repeatability)$contrasts %>% summary(infer=T))
    result_number = data.frame(emmeans(model_iter, specs=pairwise~number)$contrasts %>% summary(infer=T))
    result_repeat = result_repeat %>%
      mutate(significant=if_else(p.value<0.05,1,0),
             model=mdl,
             measure=msr)
    result_number = result_number %>%
      mutate(significant=if_else(p.value<0.05,1,0),
             model=mdl,
             measure=msr)
    results = append(results, list(result_repeat))
    results = append(results, list(result_number))
  }
}

# tidy up the results
# get the model results into one dataframe
output_h6 = data.frame(results[1])
for (i in 2:length(results)){
  output_h6 = rbind(output_h6, data.frame(results[i]))
}

# add other columns to indicate the linguistic phenomenon under investigation, the hypothesis, and whether the result is human like or not.
output_h6 = output_h6 %>% 
  mutate(phenomenon="ci",
         hypothesis="np_repeatable_number",
         
         # need to refer to the literature
         # plural better than singular
         # nonrepeatable worse than repeatable
         human_like=case_when(contrast=="nonrepeatable - repeatable"&significant==1&estimate<0&measure=="LogProb"~1,
                              contrast=="nonrepeatable - repeatable"&significant==1&estimate>0&measure%in%c("ppl","final")~1,
                              contrast=="singular - plural"&significant==1&estimate<0&measure=="LogProb"~1,
                              contrast=="singular - plural"&significant==1&estimate>0&measure%in%c("ppl","final")~1,
                              TRUE~0),
         # since all of these sentences should be illusion sentences, so we use NA to indicate there isn't a prediction.
         literal_interpretation=NA)

#######################
# aggregate and save result files
#######################
final_result = bind_rows(output_0, output_h2, output_h3_1, output_h3_2, output_h4, output_h5, output_h6)
path = ".."
file_name = "comparative_modeling_result.csv"
file_path = paste(path, "/", file_name, sep="")
write_csv(final_result,file_path)

#########################
# VISUALIZE FOR EMNLP
#########################
df_ci_fig=bind_rows(output_h4, output_h6) %>% 
  filter(measure!="LogProb") %>% 
  mutate(measure=case_when(measure=="ppl"~"Perplexity",measure=="final"~"Surprisal"),
         human_mark=if_else(human_like==1,"+",""),
         significance=if_else(significant==1,"*",""),
         hypothesis=if_else(hypothesis=="pronoun_repeatable_number","Pronoun","NP"),
         hypothesis=factor(hypothesis, levels=c("Pronoun","NP")),
         contrast=if_else(contrast=="nonrepeatable - repeatable","non/repeatable","singular/plural"),
         contrast=factor(contrast, levels=c("singular/plural","non/repeatable")),
         model=case_when(model=="bert-base-cased"~"BERT",model=="gpt2"~"GPT-2",
                         model=="roberta-base"~"RoBERTa",model=="text-davinci-003"~"GPT-3"),
         model=factor(model,levels=c("BERT","RoBERTa","GPT-2","GPT-3")))

fig = ggplot(data=df_ci_fig, aes(x=measure, y=estimate, group=model)) +
  geom_bar(aes(fill=model),stat="identity", position=position_dodge(width=1)) +
  geom_text(aes(x=measure, y=estimate, label=human_mark), position=position_dodge(width=1), vjust=0.8, size=5) +
  geom_text(aes(x=measure, y=estimate, label=significance), position=position_dodge(width=1), vjust=0.4, size=5) +
  facet_grid(hypothesis~contrast) +
  theme_bw()+
  scale_fill_manual(values=c("#66c2a5","#fc8d62","#8da0cb","#e78ac3"))
fig

