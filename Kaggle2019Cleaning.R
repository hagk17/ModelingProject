library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(esquisse)

#### Cleaning Question Data ####

# read in the questions data 
questions <- read_csv("questions_only.csv")

#tidying the weird format 
questions <- pivot_longer(questions, 1:35, names_to = "Number", values_to = "Question")


#### Schema Cleaning ####


#read in survey schema 
schema <- read_csv("survey_schema.csv")
#View(schema)

#Key: Number 
schemaNumber <- colnames(schema)[-1] #35 

#getting column names from that first column 
schemaColNames <- unname((schema[,1])) %>% as_vector() 
schemaColName <- c("Number", schemaColNames) #11

#for loop to make the rows columns 
fullD <- NA
for(i in 2:36){
  fullD <- rbind(fullD, schema[,i] %>% unname() %>% as_vector())
}

fullD <- fullD[-1,] #removes that initial NA row
schema <- cbind(schemaNumber, fullD) #adds the question number key 
colnames(schema) <- schemaColName #renames the columns 
rownames(schema) <- NULL #removes weird row names 
schema <- as.data.frame(schema) #sets as data.frame because it didn't earlier??


#### Combining Schema and Question Tables ####

scQuest <- full_join(questions, schema) %>% 
  select(-c("Question:", "Who was excluded? (0 = not excluded; 1 = excluded)"))
#View(scQuest)

#### Other Data Set Cleaning ####
other <- read_csv("other_text_responses.csv")
other <- other[-1,]

#remove the joke ones from the data set and make separate table 
realGenders <- c("Other", "Male to female transgender", "non-binary", 
                 "Transman", "transbian", "Genderfluid", "Non-binary male-leaning", 
                 "Trans female", "Nonbinary", "Genderqueer", "queer", "agender", "non-binary af")

jokeGenders <- c("Attack Helicopter", "genderfluid helicopter", "battle helicopter", "bionicle", "ufo",
                 "State of mind", "T-rex shaped meteor made out of cheese", "alien", "_mexicani02", "a",
                 "Pharoah", "robot", "Indescribable", "chakka", "GREEN GUAVA TENSILE", "Puppy", 
                 "Supermacho", "Mr.", "Unicorn", "Dragonlord", "king", "Lvl 129 Dust Devil", 
                 "half femal-half male whole animal", "I'm a funky potato")

jokeOthers <- other %>% filter(Q2_OTHER_TEXT %in% jokeGenders)
otherclean <-  other %>% filter(Q2_OTHER_TEXT %in% realGenders)

#rename some of the actual genders for consistency 
otherclean$Q2_OTHER_TEXT[otherclean$Q2_OTHER_TEXT == "Transman" ] <- "Trans Male"
transF <- c("Male to female transgender", "transbian", "Trans female")
otherclean$Q2_OTHER_TEXT[otherclean$Q2_OTHER_TEXT %in% transF ] <- "Trans Female"
nb <- c("non-binary", "Non-binary male-leaning", "non-binary af")
otherclean$Q2_OTHER_TEXT[otherclean$Q2_OTHER_TEXT %in% nb ] <- "Non-Binary"
queer <- c("Genderqueer", "queer")
otherclean$Q2_OTHER_TEXT[otherclean$Q2_OTHER_TEXT %in% queer ] <- "Queer"
## remove the completely NA columns from otherclean
otherclean <- otherclean %>% select_if(~sum(!is.na(.)) > 0)


#### Multiple Choice Responses ####

multiple <- read_csv("multiple_choice_responses.csv")
multiple <- multiple[-1,] #get rid of the row with questions 
#View(colnames(multiple))

### making the right columns integers 
rightColNamesOrder = colnames(multiple)

intCols = c(1,4,8,20,35,48,50:55,69,82,95,97,110,116,
            131,140,148,155,168,181,194,207,220,233,246)
intColNames = colnames(multiple[,intCols])
holding <- select(multiple,intColNames) %>%
  sapply(as.integer)%>%as.data.frame()

isolate <- multiple %>% select(-intColNames)
multiple <- cbind(holding,isolate) %>% select(rightColNamesOrder)

#rename selected answer columns so that it makes sense 
#this was probably unnecessary but hey I can understand it better

multiple <- multiple %>% rename(Q9_influence_decisions = Q9_Part_1,
                                Q9_run_store_analyze_data = Q9_Part_2,
                                Q9_prototype_ml = Q9_Part_3,
                                Q9_ml_improves_workflow = Q9_Part_4,
                                Q9_improve_existing_ml = Q9_Part_5,
                                Q9_research_advancing_ml = Q9_Part_6,
                                Q9_none_of_these = Q9_Part_7,
                                Q9_other = Q9_Part_8)

multiple <- multiple %>% rename(Q12_twitter_ds_influencers = Q12_Part_1,
                                Q12_hacker_news = Q12_Part_2,
                                Q12_reddit = Q12_Part_3,
                                Q12_kaggle = Q12_Part_4,
                                Q12_course_forms = Q12_Part_5,
                                Q12_youtube = Q12_Part_6,
                                Q12_podcasts = Q12_Part_7,
                                Q12_blogs = Q12_Part_8,
                                Q12_journal_publications = Q12_Part_9,
                                Q12_slack_communitires = Q12_Part_10,
                                Q12_none = Q12_Part_11,
                                Q12_other = Q12_Part_12)

multiple <- multiple %>% rename(Q13_udacity = Q13_Part_1,
                                Q13_coursera = Q13_Part_2,
                                Q13_edx = Q13_Part_3,
                                Q13_datacamp = Q13_Part_4,
                                Q13_dataquest = Q13_Part_5,
                                Q13_kaggle_courses = Q13_Part_6,
                                Q13_fastai = Q13_Part_7,
                                Q13_udemy = Q13_Part_8,
                                Q13_linkedin_learning = Q13_Part_9,
                                Q13_university_courses = Q13_Part_10,
                                Q13_none = Q13_Part_11,
                                Q13_other = Q13_Part_12)

multiple <- multiple %>% rename(Q16_jupyter = Q16_Part_1,
                                Q16_rstudio = Q16_Part_2,
                                Q16_pycharm = Q16_Part_3,
                                Q16_atom = Q16_Part_4,
                                Q16_matlab = Q16_Part_5,
                                Q16_visual_studio = Q16_Part_6,
                                Q16_spyder = Q16_Part_7,
                                Q16_vim_emacs = Q16_Part_8,
                                Q16_notepad_pp = Q16_Part_9,
                                Q16_sublime_text = Q16_Part_10,
                                Q16_none = Q16_Part_11,
                                Q16_other = Q16_Part_12)

multiple <- multiple %>% rename(Q17_jkaggle_notebooks = Q17_Part_1,
                                Q17_google_colab = Q17_Part_2,
                                Q17_microsoft_azure_notebooks = Q17_Part_3,
                                Q17_google_cloud_notebook = Q17_Part_4,
                                Q17_paperspace_gradient = Q17_Part_5,
                                Q17_floydhub = Q17_Part_6,
                                Q17_binder_jupyterhub = Q17_Part_7,
                                Q17_ibm_watson = Q17_Part_8,
                                Q17_code_ocean = Q17_Part_9,
                                Q17_aws_notebook = Q17_Part_10,
                                Q17_none = Q17_Part_11,
                                Q17_other = Q17_Part_12)

multiple <- multiple %>% rename(Q18_python = Q18_Part_1,
                                Q18_r = Q18_Part_2,
                                Q18_sql = Q18_Part_3,
                                Q18_c = Q18_Part_4,
                                Q18_c_pp = Q18_Part_5,
                                Q18_java = Q18_Part_6,
                                Q18_javascript = Q18_Part_7,
                                Q18_typescript = Q18_Part_8,
                                Q18_code_bash = Q18_Part_9,
                                Q18_aws_matlab = Q18_Part_10,
                                Q18_none = Q18_Part_11,
                                Q18_other = Q18_Part_12)

multiple <- multiple %>% rename(Q20_ggplot = Q20_Part_1,
                                Q20_matplotlib = Q20_Part_2,
                                Q20_altair = Q20_Part_3,
                                Q20_shiny = Q20_Part_4,
                                Q20_d3.js = Q20_Part_5,
                                Q20_plotly = Q20_Part_6,
                                Q20_bokeh = Q20_Part_7,
                                Q20_seaborn= Q20_Part_8,
                                Q20_geoplotlib = Q20_Part_9,
                                Q20_leaflet_folium = Q20_Part_10,
                                Q20_none = Q20_Part_11,
                                Q20_other = Q20_Part_12)

multiple <- multiple %>% rename(Q21_cpu = Q21_Part_1,
                                Q21_gpu = Q21_Part_2,
                                Q21_tpu = Q21_Part_3,
                                Q21_none_idk = Q21_Part_4,
                                Q21_other = Q21_Part_5)

multiple <- multiple %>% rename(Q24_linear_log_regression = Q24_Part_1,
                                Q24_decision_trees_random_forest = Q24_Part_2,
                                Q24_gradient_boosting_machines = Q24_Part_3,
                                Q24_bayesian = Q24_Part_4,
                                Q24_evolutionary = Q24_Part_5,
                                Q24_dense_neural_networks = Q24_Part_6,
                                Q24_convolutional_neural_networks = Q24_Part_7,
                                Q24_generative_adversarial_networks = Q24_Part_8,
                                Q24_recurrent_neural_networks = Q24_Part_9,
                                Q24_transformer_networks = Q24_Part_10,
                                Q24_none = Q24_Part_11,
                                Q24_other = Q24_Part_12)

multiple <- multiple %>% rename(Q25_data_augmentation = Q25_Part_1,
                                Q25_feature_engineering = Q25_Part_2,
                                Q25_model_selection = Q25_Part_3,
                                Q25_model_architecture_searches = Q25_Part_4,
                                Q25_hyperparam_tuning = Q25_Part_5,
                                Q25_ml_pipelines = Q25_Part_6,
                                Q25_none = Q25_Part_7,
                                Q25_other = Q25_Part_8)

multiple <- multiple %>% rename(Q26_general_image_video_tools = Q26_Part_1,
                                Q26_image_segmentation_methods = Q26_Part_2,
                                Q26_object_detection_methods = Q26_Part_3,
                                Q26_image_classification_other_general = Q26_Part_4,
                                Q26_generative_networks = Q26_Part_5,
                                Q26_none = Q26_Part_6,
                                Q26_other = Q26_Part_7)

multiple <- multiple %>% rename(Q27_word_embedding_vectors = Q27_Part_1,
                                Q27_encoder_decoder_models = Q27_Part_2,
                                Q27_contextualized_embeddings = Q27_Part_3,
                                Q27_transformer_language_models = Q27_Part_4,
                                Q27_none = Q27_Part_5,
                                Q27_other = Q27_Part_6)

multiple <- multiple %>% rename(Q28_scikit_learn = Q28_Part_1,
                                Q28_tensorflow = Q28_Part_2,
                                Q28_keras = Q28_Part_3,
                                Q28_random_forest = Q28_Part_4,
                                Q28_xgboost = Q28_Part_5,
                                Q28_pytorch = Q28_Part_6,
                                Q28_caret = Q28_Part_7,
                                Q28_lightgbm = Q28_Part_8,
                                Q28_spark_mlib = Q28_Part_9,
                                Q28_fasai = Q28_Part_10,
                                Q28_none = Q28_Part_11,
                                Q28_other = Q28_Part_12)

multiple <- multiple %>% rename(Q29_google_cloud_platform = Q29_Part_1,
                                Q29_aws = Q29_Part_2,
                                Q29_microsoft_azure = Q29_Part_3,
                                Q29_ibm_cloud = Q29_Part_4,
                                Q29_alibaba_cloud = Q29_Part_5,
                                Q29_salesforce_cloud = Q29_Part_6,
                                Q29_oracle_cloud = Q29_Part_7,
                                Q29_sap_cloud= Q29_Part_8,
                                Q29_vmware_cloud = Q29_Part_9,
                                Q29_red_hat_cloud = Q29_Part_10,
                                Q29_none = Q29_Part_11,
                                Q29_other = Q29_Part_12)

multiple <- multiple %>% rename(Q30_aws_ec2 = Q30_Part_1,
                                Q30_google_compute_engine = Q30_Part_2,
                                Q30_aws_lambda = Q30_Part_3,
                                Q30_azure_virtual_machine = Q30_Part_4,
                                Q30_google_app_engine = Q30_Part_5,
                                Q30_google_cloud_functions = Q30_Part_6,
                                Q30_aws_elastic_beanstalk = Q30_Part_7,
                                Q30_google_kubernetes_engine = Q30_Part_8,
                                Q30_aws_batch = Q30_Part_9,
                                Q30_azure_container_service = Q30_Part_10,
                                Q30_none = Q30_Part_11,
                                Q30_other = Q30_Part_12)

multiple <- multiple %>% rename(Q31_google_bigquery = Q31_Part_1,
                                Q31_aws_redshift = Q31_Part_2,
                                Q31_databricks = Q31_Part_3,
                                Q31_aws_elastic_mapreduce = Q31_Part_4,
                                Q31_teradata = Q31_Part_5,
                                Q31_microsoft_analysis_services = Q31_Part_6,
                                Q31_google_cloud_dataflow = Q31_Part_7,
                                Q31_aws_athena = Q31_Part_8,
                                Q31_aws_kinesis = Q31_Part_9,
                                Q31_google_cloud_pub_sub = Q31_Part_10,
                                Q31_none = Q31_Part_11,
                                Q31_other = Q31_Part_12)

multiple <- multiple %>% rename(Q32_sas = Q32_Part_1,
                                Q32_cloudera = Q32_Part_2,
                                Q32_azure_ml_studio = Q32_Part_3,
                                Q32_google_cloud_ml_engine = Q32_Part_4,
                                Q32_google_cloud_vision = Q32_Part_5,
                                Q32_google_cloud_speech_to_text = Q32_Part_6,
                                Q32_google_cloud_natural_language = Q32_Part_7,
                                Q32_aws_rapidminer = Q32_Part_8,
                                Q32_aws_google_cloud_translation = Q32_Part_9,
                                Q32_amazon_sagemaker = Q32_Part_10,
                                Q32_none = Q32_Part_11,
                                Q32_other = Q32_Part_12)

multiple <- multiple %>% rename(Q33_google_automl = Q33_Part_1,
                                Q33_h20_driverless_ai = Q33_Part_2,
                                Q33_databricks_automl = Q33_Part_3,
                                Q33_datarobot_automl = Q33_Part_4,
                                Q33_tpot = Q33_Part_5,
                                Q33_auto_keras = Q33_Part_6,
                                Q33_auto_sklearn = Q33_Part_7,
                                Q33_auto_ml = Q33_Part_8,
                                Q33_xcessiv = Q33_Part_9,
                                Q33_mlbox = Q33_Part_10,
                                Q33_none = Q33_Part_11,
                                Q33_other = Q33_Part_12)

multiple <- multiple %>% rename(Q34_my_sql = Q34_Part_1,
                                Q34_postgress_sql = Q34_Part_2,
                                Q34_sqlite = Q34_Part_3,
                                Q34_microsoft_sql_server = Q34_Part_4,
                                Q34_oracle_database = Q34_Part_5,
                                Q34_microsoft_access = Q34_Part_6,
                                Q34_aws_relational_database_service = Q34_Part_7,
                                Q34_aws_dynamodb = Q34_Part_8,
                                Q34_azure_sql_database = Q34_Part_9,
                                Q34_google_cloud_sql = Q34_Part_10,
                                Q34_none = Q34_Part_11,
                                Q34_other = Q34_Part_12)


## turn NAs to 0 in appropriate columns 
need0Cols <- c(12:19,23:34,36:47,57:68,70:81,83:95,98:109,111:115,119:130,132:139,
               141:147, 149:154, 156:167, 169:180, 182:194, 195:206, 208:219,
               221:232, 234:245)
multiple <- multiple %>% mutate_at(need0Cols, ~replace(., is.na(.), 0))


## FACTOR EVERYTHING that's not already an integer

factorColNames <- setdiff(colnames(multiple),colnames(multiple[,c(5,21,22,intCols)]))
multiple[factorColNames] <- lapply(multiple[factorColNames], factor)


#### Exploring the Questions and Response ####

uniqueValsCols <- sapply(multiple, unique)
#View(uniqueValsCols)

## Cleaning Factors that Need to be ordered/other cleaning 

## [2] Q1: "What is your age (# years)?" 
ageLevels = c("18-21", "22-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-69", "70+")
multiple$Q1 <-  factor(multiple$Q1, levels = ageLevels)

## [4] Q3: "In which country do you currently reside?"
multiple$Q3[multiple$Q3 %in% c("South Korea", "Republic of Korea") ] <- "South Korea"
multiple$Q3[multiple$Q3 %in% c("Viet Nam") ] <- "Vietnam"
multiple$Q3[multiple$Q3 == "Iran, Islamic Republic of..." ] <- "Iran"
multiple$Q3[multiple$Q3 == "United Kingdom of Great Britain and Northern Ireland" ] <- "UK"
multiple$Q3 <-  factor(multiple$Q3)

#View(multiple)

## [5] Q4: "What is the highest level of formal education that you have attained 
#make factored and fix levels 
edu <- c("I prefer not to answer", "No formal education past high school", 
         "Some college/university study without earning a bachelor’s degree",
         "Bachelor’s degree", "Master’s degree", "Doctoral degree", "Professional degree")
multiple$Q4 <- factor(multiple$Q4, levels = edu)

#View(multiple)

## [7] Q6  "What is the size of the company where you are employed?" 
coSize <- c("0-49 employees", "50-249 employees", "250-999 employees",
            "1000-9,999 employees",  "> 10,000 employees")
multiple$Q6 <- factor(multiple$Q6, levels = coSize)

##  [8] Q7 "Approximately how many individuals are responsible for data science workloads 
## at your place of business?"
multiple$Q7 <- factor(multiple$Q7, levels = c("0", "1-2", "3-4", "5-9", "10-14", "15-19", "20+"))


## [9] Q8 "Does your current employer incorporate machine learning methods into their business?"  
mlUsage <- c("I do not know", "No (we do not use ML methods)",
             "We are exploring ML methods (and may one day put a model into production)",
             "We use ML methods for generating insights (but do not put working models into production)",
             "We recently started using ML methods (i.e., models in production for less than 2 years)",
             "We have well established ML methods (i.e., models in production for more than 2 years)" )
multiple$Q8 <- factor(multiple$Q8, levels = mlUsage)

#View(multiple)

## [11] Q10 "What is your current yearly compensation (approximate $USD)?"    
multiple$Q10[multiple$Q10 == "$0-999" ] <- "0-999"
multiple$Q10[multiple$Q10 == "> $500,000" ] <- "> 500,000"
money <- c("0-999", "1,000-1,999", "2,000-2,999", "3,000-3,999", 
           "4,000-4,999", "5,000-7,499", "7,500-9,999", "10,000-14,999",
           "15,000-19,999", "20,000-24,999", "25,000-29,999", "30,000-39,999",
           "40,000-49,999", "50,000-59,999", "60,000-69,999", "70,000-79,999",
           "80,000-89,999","90,000-99,999","100,000-124,999","125,000-149,999",
           "150,000-199,999", "200,000-249,999", "250,000-299,999", "300,000-500,000",
           "> 500,000")
multiple$Q10 <- factor(multiple$Q10, levels = money)


## [12] Q11 "Approximately how much money have you spent on machine learning 
#and/or cloud computing products at your work in the past 5 years?"       
multiple$Q11[multiple$Q11 == "$0 (USD)" ] <- "$0"
multiple$Q11[multiple$Q11 == "> $100,000 ($USD)" ] <- "> $100,000"
moneyToo <- c("$0", "$1-$99", "$100-$999", "$1000-$9,999", 
              "$10,000-$99,999","> $100,000")
multiple$Q11 <- factor(multiple$Q11, levels = moneyToo)

## [16] Q15 "How long have you been writing code to analyze data 
#(at work or at school)?"
coding <- c("I have never written code", "< 1 years", "1-2 years",
            "3-5 years", "5-10 years", "10-20 years", "20+ years")
multiple$Q15 <- factor(multiple$Q15, levels = coding)

## [23] Q22 "Have you ever used a TPU (tensor processing unit)?" 
elmo <- c("Never", "Once", "2-5 times", "6-24 times", "> 25 times")
multiple$Q22 <- factor(multiple$Q22, levels = elmo)

## [24] Q23 "For how many years have you used machine learning methods?"  
years <- c("< 1 years", "1-2 years", "2-3 years", "3-4 years",
           "4-5 years", "5-10 years", "10-15 years", "20+ years")
multiple$Q23 <- factor(multiple$Q23, levels = years)

#View(multiple)



#### Save the cleaned Data files ####

write.csv(multiple, file = "Kleaned_Kaggle_Multiple_2019.csv")
write.csv(otherclean, file = "Kleaned_Kaggle_Other_2019.csv")
write.csv(scQuest, file = "Kleaned_Kaggle_Schema_Questions_2019.csv")
write.csv(jokeGenders, file = "Kleaned_Kaggle_OtherJokeGenders_2019.csv")




















