#IDS 570 - Text as Data
#Peter de Guzman
#Week 5 - Text Representation (2) Tutorial

#packages
library(readr) 
library(tidyverse)
library(tidyr) 
library(tidytext) 
library(ggplot2) 
library(udpipe)
library(koRpus)
library(koRpus.lang.en)

theme_set(theme_minimal(base_size = 14))

#Load text files
circle <- read_file("texts/A07594__Circle_of_Commerce.txt")
mystery <- read_file("texts/A69858.txt")


#create a tidy data frame with both texts
texts_df <- tibble(
    document = c("Circle of Commerce", "A69858"),
    author =c("Misselden", "Unknown"),
    text=c(circle, mystery)
)

#Display basic information
texts_df %>% 
    select(document, author) %>%
    knitr::kable(caption="Our Two Texts")


#calculating the Type-Token Ratio (TTR)

#Tokenize the texts and do some basic cleaning
tokens <- texts_df %>%
    unnest_tokens(word, text) %>%
    mutate(word = str_to_lower(word))

# Calculate the Type Token Ratio for each text
ttr_results <- tokens %>%
    group_by(document, author) %>%
    summarise(
        tokens = n(), #total words
        types = n_distinct(word), #unique words
        ttr = types/tokens,
        .groups="drop"
    )

#display results
ttr_results %>%
    knitr::kable(
        digits = 3,
        caption = "Type-Token Ratio Results",
        col.names = c("Document", "Author", "Total Words", "Unique Words", "TTR")
    )


#create a bar plot comparing TTR

ggplot(ttr_results, aes(x = author, y = ttr, fill = author)) +
    geom_col(width = 0.6) + 
    geom_text(aes(label=round(ttr,3)),
                vjust=-0.5, size=5) +
    scale_fill_brewer(palette="Set2") +
    labs(
        title="Type-Token Ratio Comparison",
        subtitle = "Higher values = more diverse vocabulary",
        x = NULL,
        y = "Type-Token Ratio (TTR)"
    ) +
    theme(legend.position="none") +
    ylim(0, max(ttr_results$ttr)*1.15)

#Guiraud Index

#Calculate Guiraud Index for each text 
guiraud_results <- tokens %>%
    group_by(document, author) %>%
    summarise(
        tokens = n(),
        types = n_distinct(word),
        guiraud = types/sqrt(tokens),
        .groups="drop"
    )

#Display Results
guiraud_results %>%
    knitr::kable(
        digits=3,
        caption="Guiraud Index Results",
        col.names = c("Document", "Author", "Total Words", "Unique Words", "Guiraud Index")
    )

#Create a bar plot comparing Guiraud Index
ggplot(guiraud_results, aes(x=author, y=guiraud, fill=author)) +
    geom_col(width=0.6) +
    geom_text(aes(label=round(guiraud, 2)),
    vjust = -0.5, size = 5) +
    scale_fill_brewer(palette="Set1") +
    labs(
        title="Guiraud Index Comparison",
        subtitle="Length-corrected measure of lexican diversity",
        x=NULL,
        y="Guiraud Index"
    ) +
    theme(legend.position="none") +
    ylim(0, max(guiraud_results$guiraud) * 1.15)


    #Calculating MTLD 

    #Function to calculate MTLD for a text
    calculate_mtld <- function(text_string, doc_name) {
        #Create a temp file (koRpus requirement)
        temp_file <- tempfile(fileext = ".txt")
        writeLines(text_string, temp_file)

        #Tokenize with koRpus
        tokenized <- tokenize(temp_file, lang = "en")

        #Calculate MTLD
        mtld_result <- MTLD(tokenized)

        #Extract the MTLD value
        mtld_value <- mtld_result@MTLD$MTLD

        #Clean up
        unlink(temp_file)

        return(mtld_value)
    }

#Calculate MTLD for both texts
mtld_results <- texts_df %>%
    rowwise() %>%
    mutate(
        mtld = calculate_mtld(text, document)
    ) %>%
    ungroup() %>%
    select(document, author, mtld)


#Display Results
mtld_results %>%
    knitr::kable(
        digits = 2,
        caption = "MTLD Results",
        col.names = c("Document", "Author", "MTLD")
    )

#create a bar plot comparing MTLD
ggplot(mtld_results, aes(x = author, y = mtld, fill = author)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(mtld, 1)), 
            vjust = -0.5, size = 5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "MTLD Comparison",
    subtitle = "Mean Length of Sequential Word Strings (Higher = More Diverse)",
    x = NULL,
    y = "MTLD Score"
  ) +
  theme(legend.position = "none") +
  ylim(0, max(mtld_results$mtld) * 1.15)



#MEASURES OF SYNTACTIC COMPLEXITY

texts_df <- tibble(document = c("Circle of Commerce", "A69858"),
                   author = c("Misselden", "Unknown"),
                   text = c(circle, mystery)
                   )

texts_df

#Load an English Universal Dependancies model ONCE

model_info <- udpipe_download_model(language="english-ewt")
ud_model <- udpipe_load_model(model_info$file_model)

#annotating both texts using UDPipe
anno_df <- texts_df %>%
    mutate(
        #Parse each text with the UD parser; set doc_id to our document name
        anno = map2(text, document, ~ udpipe_annotate(ud_model, x = .x, doc_id = .y) %>%
        as.data.frame())
    ) %>%
    #Keep only parsed annotations, then unnest into rows
    select(anno) %>%
    unnest(anno) %>%
    #Use the UD doc_id as our document label (and drop any duplicates cleanly)
    rename(document=doc_id) %>%
    #Select columns for syntatic analysis
    select(
        document, 
        paragraph_id, 
        sentence_id,
        token_id,
        token,
        lemma,
        upos, #part of speech
        feats, #grammatical features (e.g., verb form)
        head_token_id, #head of dependency relation
        dep_rel #dependency relation type
    )

anno_df %>% glimpse()

#with head_token_id, we are crreating a dependency tree
# Create an example parsed sentence
example_sentence <- tibble(
  token = c("The", "big", "dog", "barks"),
  token_id = c(1, 2, 3, 4),
  head_token_id = c(3, 3, 4, 0),
  Relationship = c(
    '"The" depends on word #3 (dog)',
    '"big" depends on word #3 (dog)',
    '"dog" depends on word #4 (barks)',
    '"barks" is the ROOT (doesn\'t depend on anything)'
  )
)

example_sentence %>%
  knitr::kable(
    caption = 'Example: Dependency structure of "The big dog barks"',
    align = c("l", "c", "c", "l")
  )

#we can define some syntactic complexity features by using dependency relations

#we can create binary flags for different syntactic structures

syntax_df <- anno_df %>%
mutate(
is_word = upos != "PUNCT", #<--is it a word (and not punctuation?)


# Is this an independent clause? finite verbs are proxy for indipendent clauses
is_clause = (upos %in% c("VERB", "AUX")) &
            str_detect(coalesce(feats, ""), "VerbForm=Fin"),

# Dependent clause? 
is_dep_clause = dep_rel %in% c(
  "advcl", #adverbial clause 
  "ccomp", # clausal complement
  "xcomp", #open clausal complement
  "acl", #adnomial clause
  "acl:relcl" #relative clause
),

# Is this coordination? That is, does it use "and" "or" etc.?
is_coord = dep_rel %in% c("conj", "cc"),

# Nominal complexity: these relations make noun phrases more complex
is_complex_nominal = dep_rel %in% c(
  "amod", # adjective modifier ("big cup")
  "nmod", #nominal modifier ("cup of tea")
  "compound", # compound ("lemon tea")
  "appos" #apposition ("tea, my favorite!")
)

)

syntax_df %>% 
  select(document, token, upos, is_clause, is_dep_clause) %>%
  head(20)

#aggregating at the sentence level
#counting syntactic features for each individual sentence

sentence_df <- syntax_df %>%
    filter(is_word) %>% #only count words not punctuation)
    group_by(document, sentence_id) %>%
    summarise(
        words = n(), #number of words per sentence
        clauses = sum(is_clause), #number of clauses per sentence
        dep_clauses = sum(is_dep_clause), #number of dependent clauses per sentence
        .groups = "drop"
    )

sentence_df

#mean length of sentence 
mls_df <- sentence_df %>%
    group_by(document) %>%
    summarise(
        MLS = mean(words), #avg words per sentence
        .groups = "drop"
    )

#inspecting
mls_df

#Overall Sentence Complexity

#Calculate clauses per sentence
clausal_density_df <- sentence_df %>%
    group_by(document) %>%
    summarise(
        sentences = n(),
        clauses = sum(clauses),
        C_per_S = clauses / sentences,
        .groups = "drop"
    )

#Lets check and compare
clausal_density_df

#Subordination (DC/C and DC/S):
subordination_df <- sentence_df %>%
group_by(document) %>%
summarise(
clauses = sum(clauses),
dep_clauses = sum(dep_clauses),
sentences = n(),
DC_per_C = dep_clauses / pmax(clauses, 1), #avoid division by 0 (shouldn't happen, but if no clauses were detected in the sentence, we still want it to run)
DC_per_S = dep_clauses / sentences,
.groups = "drop"
)

subordination_df

#coordination
coordination_df <- syntax_df %>%
group_by(document) %>%
summarise(
coord_relations = sum(is_coord),
clauses         = sum(is_clause),
sentences       = n_distinct(sentence_id),
Coord_per_C     = coord_relations / pmax(clauses, 1),
Coord_per_S     = coord_relations / sentences,
.groups = "drop"
)

coordination_df

#phrasal complexity
nominal_df <- syntax_df %>%
group_by(document) %>%
summarise(
complex_nominals = sum(is_complex_nominal),
clauses          = sum(is_clause),
sentences        = n_distinct(sentence_id),
CN_per_C         = complex_nominals / pmax(clauses, 1),
CN_per_S         = complex_nominals / sentences,
.groups = "drop"
)

nominal_df


#bringing everything together
# Combine all measures
all_measures <- mls_df %>%  # ← Added mls_df %>%
  left_join(clausal_density_df %>% select(document, C_per_S), by = "document") %>%
  left_join(subordination_df %>% select(document, DC_per_C, DC_per_S), by = "document") %>%
  left_join(coordination_df %>% select(document, Coord_per_C, Coord_per_S), by = "document") %>%
  left_join(nominal_df %>% select(document, CN_per_C, CN_per_S), by = "document")

all_measures %>%
  knitr::kable(
    digits = 2,
    col.names = c("Document", "MLS", "C/S", "DC/C", "DC/S", 
                  "Coord/C", "Coord/S", "CN/C", "CN/S")
  )