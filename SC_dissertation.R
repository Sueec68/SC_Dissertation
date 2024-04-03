


# setup -------------------------------------------------------------------

# load libraries
library("here")
library("magrittr")
library("rstatix")
library("textdata")
library("tidytext")
library("tidyverse")
library("wordcloud")

# custom plot theme
theme_custom <- function() {
  theme_light() +
    theme(
      axis.text = element_text(size = 10),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size = 12, margin = margin(t = 15)),
      axis.title.y = element_text(size = 12, margin = margin(r = 15)),
      panel.grid.minor = element_blank(),
      plot.margin = margin(1, 1, 1, 1, unit = "cm"),
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10))
    )
}

# set plot theme
theme_set(theme_custom())


# read data ---------------------------------------------------------------

ffa_csv <- read_csv(file = here("C:/Users/susan/OneDrive/MSc Year 2/Dissertation/ffa.csv"), col_names = FALSE)



set.seed(1234)

ffa_cln <- 
  ffa_csv %>% 
  set_colnames(., c("feedback", "sex", "clinicalreview")) %>% 
  rownames_to_column(var = "entry_id") %>% 
  mutate(
    sex = sample(c("M", "F"), size =308, replace = TRUE), .before = feedback
  ) %>% 
  add_count(sex, name = "sex_n") %>% 
  relocate(sex_n, .after = sex)


# explore non-text variables in data --

# distribution of sex
ffa_cln %>%
  count(sex, name = "count", sort = TRUE) %>% 
  mutate(sex = fct_reorder(sex, count)) %>%
  ggplot(aes(x = count, y = sex, fill = sex)) +
  geom_col() +
  scale_fill_manual(values = c("skyblue", "pink"), guide = "none")

ggsave(
  filename = "Plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# distribution of outcome variable
ffa_cln %>% 
  count(clinicalreview, name = "count", sort = TRUE) %>% # ~50% more N than Y
  ggplot(aes(x = count, y = clinicalreview, fill = clinicalreview)) +
  geom_col() +
  scale_fill_manual(values = c("skyblue", "pink"), guide = "none")

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# distribution of outcome variable by sex
ffa_cln %>% 
  count(sex, sex_n, clinicalreview, name = "count", sort = TRUE) %>% 
  mutate(
    prop = divide_by(count,sex_n),
    clinicalreview = fct_rev(clinicalreview)
  ) %>% 
  ggplot(aes(x = sex, y = prop, fill = clinicalreview)) +
  geom_col(position = "dodge") +
  scale_y_continuous(breaks = seq(0, 1, 0.1))

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# is there a statistically significant difference in outcome variable between sex?
ffa_cln %>% 
  count(sex, clinicalreview, name = "count", sort = TRUE) %>% 
  pivot_wider(names_from = sex, values_from = count) %>% 
  column_to_rownames(var = "clinicalreview") %>% 
  chisq_test() 

# explore text variables --------------------------------------------------

# convert to tidy text format
ffa_cln_tt <- 
  ffa_cln %>% 
  unnest_tokens(input = feedback, output = "word") %>% # does str_to_lower by default
  anti_join(stop_words, by = "word")

# feedback length frequency distribution i.e., words per feedback item
words_per_obs <- 
  ffa_cln_tt %>% 
  count(entry_id, name = "total_words", sort = TRUE)

words_per_obs %>% 
  ggplot(aes(x = total_words)) +
  geom_histogram(fill = "midnightblue", alpha = 0.7, binwidth = 10) +
  geom_vline(xintercept = median(words_per_obs$total_words), lty = "dashed", colour = "hotpink") +
  scale_x_continuous(breaks = seq(0, 160, 20)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Total Words in Feedback",
    y = "Count"
  )
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# feedback length frequency distribution by sex
words_per_obs_sex <- 
  ffa_cln_tt %>% 
  count(sex, entry_id, name = "total_words", sort = TRUE)


# summary df here for `geom_vline`
words_median_sex <- 
  words_per_obs_sex %>% 
  group_by(sex) %>% 
  summarise(median = median(total_words))


words_per_obs_sex %>% 
  ggplot(aes(x = total_words, fill = sex)) +
  geom_histogram(alpha = 0.7, binwidth = 10, center = 5) +
  geom_vline(data = words_median_sex, aes(xintercept = median), lty = "dashed", colour = "gray50") +
  scale_x_continuous(breaks = seq(0, 160, 20)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("pink", "skyblue"), guide = "none") +
  facet_wrap(vars(sex), scales = "free_x") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Total Words in Feedback",
    y = "Count"
  )
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")


#box and whisker plots exploration for alternative visualisation
words_per_obs_sex %>% 
  ggplot(aes(x = total_words, fill = sex)) +
  geom_boxplot(alpha = 0.7,) +
  geom_vline(data = words_median_sex, aes(xintercept = median), lty = "dashed", colour = "gray50") +
  scale_x_continuous(breaks = seq(0, 160, 20)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("pink", "skyblue"), guide = "none") +
  facet_wrap(vars(sex), scales = "free_x") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Total Words in Feedback",
    y = "Count"
  ) + coord_flip()
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# word frequency (visualising only the top 10)
ffa_cln_tt %>% 
  count(word, name = "count", sort = TRUE) %>% 
  mutate(word = fct_reorder(word, count)) %>%
  slice_head(n = 10) %>% 
  ggplot(aes(x = count, y = word, fill = word)) +
  geom_col() +
  scale_colour_viridis_b() +
  theme(legend.position = "none")
  
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")


# word frequency by sex
ffa_cln_tt %>% 
  count(sex, word, name = "count", sort = TRUE) %>% 
  mutate(word = reorder_within(x = word, by = count, within = sex)) %>% 
  slice_head(n = 10, by = sex) %>% 
  ggplot(aes(x = count, y = word, fill = word)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(vars(sex), scales = "free_y") +
  scale_colour_viridis_b() +
  theme(legend.position = "none")


ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")


# word frequency by sex normalised to group size


ffa_cln_tt %>% 
  count(sex, sex_n, word, name = "count", sort = TRUE) %>% 
  mutate(prop = divide_by(count, sex_n))
 
ffa_cln_tt %>% 
  count(sex, sex_n, word, name = "count", sort = TRUE) %>% 
  mutate(
    prop = count %>% 
      divide_by(sex_n),
    word = reorder_within(x = word, by = prop, within = sex)
  ) %>% 
  slice_head(n = 10, by = sex) %>% 
  ggplot(aes(x = prop, y = word, fill = word)) +
  geom_col() +
  scale_colour_viridis_b() +
  theme(legend.position = "none") +
  scale_y_reordered() +
  facet_wrap(vars(sex), scales = "free_y") 
 
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")


# explore sentiment lexicons ----------------------------------------------

# ensure dictionaries are available 
# get_sentiments("afinn")
# get_sentiments("bing")
# get_sentiments("nrc")

# join each sentiment dictionary to data
data_afinn <- 
  ffa_cln_tt %>% 
  inner_join(get_sentiments("afinn"), by = "word")

data_bing <-
  ffa_cln_tt %>% 
  inner_join(get_sentiments("bing"), by = "word")

data_nrc <-
  ffa_cln_tt %>% 
  inner_join(get_sentiments("nrc"), by = "word")


data_afinn %>% 
  pull(entry_id) %>% 
  unique() %>% 
  length()

data_bing %>% 
  pull(entry_id) %>% 
  unique() %>% 
  length()

data_nrc %>% 
  pull(entry_id) %>% 
  unique() %>% 
  length()

# overall sentiment distribution in the data - afinn dictionary
data_afinn %>% 
  count(value, name = "count") %>% 
  mutate(
    value = value %>% 
      as_factor() %>% 
      fct_rev()
  ) %>% 
  ggplot(aes(x = count, y = value, fill = value)) +
  geom_col() +
  scale_colour_viridis_b() +
  theme(legend.position = "none")

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# overall sentiment distribution in the data - bing dictionary
data_bing %>%
  count(sentiment, name = "count") %>%
  mutate(sentiment = fct_reorder(sentiment, count)) %>%
  ggplot(aes(x = count, y = sentiment, fill = sentiment)) +
  geom_col() +
  scale_colour_viridis_b() +
  theme(legend.position = "none")

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# overall sentiment distribution in the data - nrc dictionary - included for completeness of analysis
data_nrc %>%
  count(sentiment, name = "count") %>%
  mutate(sentiment = fct_reorder(sentiment, count)) %>%
  ggplot(aes(x = count, y = sentiment, fill = sentiment)) +
  geom_col() +
  scale_colour_viridis_b() +
  theme(legend.position = "none")

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")


# sentiment distribution in the data by sex

data_afinn %>% 
  count(sex, value, name = "count") %>% 
  mutate(
    value = value %>%
      as_factor() %>%
      fct_rev()
  ) %>%
  ggplot(aes(x = count, y = value, fill = value)) +
  geom_col() +
  facet_wrap(vars(sex), scales = "free_x") +
  scale_colour_viridis_b() +
  theme(legend.position = "none")

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

data_bing %>%
  count(sex, sentiment, name = "count") %>%
  mutate(sentiment = fct_reorder(sentiment, count)) %>%
  ggplot(aes(x = count, y = sentiment, fill = sentiment)) +
  geom_col() +
  facet_wrap(vars(sex), scales = "free_x") +
  scale_colour_viridis_b() +
theme(legend.position = "none")

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# calculate overall sentiment per patient using afinn and bing lexicons
afinn_net <- 
  data_afinn %>% 
  group_by(entry_id, sex, clinicalreview) %>% 
  summarise(net_sentiment = sum(value)) %>% 
  ungroup()


bing_net <- 
  data_bing %>% 
  count(entry_id, sex, clinicalreview, sentiment, name = "n_type") %>% 
  arrange(entry_id, sentiment) %>% 
 
  mutate(n_type_weighted = ifelse(sentiment == "negative", yes = subtract(n_type), no = n_type)) %>% 
  group_by(entry_id, sex, clinicalreview) %>% 
  summarise(net_sentiment = sum(n_type_weighted)) %>% 
  ungroup()

# visualise distribution of net sentiments
afinn_net %>% 
  ggplot(aes(net_sentiment)) +
  geom_histogram(binwidth = 1, center = 0.5, fill = "purple")

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

bing_net %>% 
  ggplot(aes(net_sentiment)) +
  geom_histogram(binwidth = 1, center = 0.5, fill = "blue")

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# visualise distribution of net sentiments by sex 
afinn_net %>% 
  ggplot(aes(net_sentiment, fill = sex)) +
  geom_histogram(binwidth = 1, center = 0.5) +
  facet_wrap(vars(sex)) +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")
  
afinn_net %>% 
  ggplot(aes(x = sex, y = net_sentiment, fill = sex)) +
  geom_boxplot() +
  labs(title = "AFINN Lexicon") +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

bing_net %>% 
  ggplot(aes(net_sentiment, fill = sex)) +
  geom_histogram(binwidth = 1, center = 0.5) +
  facet_wrap(vars(sex)) +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

bing_net %>% 
  ggplot(aes(x = sex, y = net_sentiment, fill = sex)) +
  geom_boxplot() +
  labs(title = "Bing Lexicon") +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")



# 1) test for normality, to assume normality has to be > 0.05
shapiro.test(afinn_net$net_sentiment)
shapiro.test(bing_net$net_sentiment)

# 2) run statistical test - if p > 0.05 then there is no difference in net sentiment between sex groups
wilcox_test(data = afinn_net, net_sentiment ~ sex)
wilcox_test(data = bing_net, net_sentiment ~ sex)

t_test(data = afinn_net, net_sentiment ~ sex)
t_test(data = bing_net, net_sentiment ~ sex)

# correlation between two afinn and bing lexicons
cor_dat <- 
  afinn_net %>% 
  inner_join(bing_net, by = "entry_id") %>% 
  select(
    entry_id,
    afinn_net = net_sentiment.x,
    bing_net = net_sentiment.y
  )

# test for normality - data set specific test due to loss of observations, not common to both data frames
shapiro.test(cor_dat$afinn_net)
shapiro.test(cor_dat$bing_net)

# spearman if data not normally distributed, 
cor_res <- cor.test(cor_dat$afinn_net, cor_dat$bing_net, method = "spearman", exact = FALSE) 
cor_res

# pearson if data is normally distributed 
cor_res <- cor.test(cor_dat$afinn_net, cor_dat$bing_net, method = "pearson", exact = FALSE) 
cor_res

# significant and a corr coeff > 0.6 is a strong positive correlation
cor_dat %>% 
  ggplot(aes(x = afinn_net, y = bing_net)) +
  geom_smooth(method = "lm", lty = "dashed", colour = "hotpink") +
  geom_point(colour = "midnightblue", alpha = 0.5, size = 3) +
  scale_x_continuous(limits = c(-25, 5), breaks = seq(-25, 5, 5)) +
  scale_y_continuous(limits = c(-20, 5), breaks = seq(-20, 5, 5)) +
  labs(
    title = "Correlation Between Lexicon Net Sentiment",
    subtitle = str_c("Spearman Correlation Coefficient = ", round(cor_res$estimate, digits = 2)),
    x = "AFINN",
    y = "Bing"
  )

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

cor_dat %>% 
  ggplot(aes(x = afinn_net, y = bing_net)) +
  geom_smooth(method = "lm", lty = "dashed", colour = "hotpink") +
  geom_point(colour = "midnightblue", alpha = 0.5, size = 3) +
  scale_x_continuous(limits = c(-25, 5), breaks = seq(-25, 5, 5)) +
  scale_y_continuous(limits = c(-20, 5), breaks = seq(-20, 5, 5)) +
  labs(
    title = "Correlation Between Lexicon Net Sentiment",
    subtitle = str_c("Pearson Correlation Coefficient = ", round(cor_res$estimate, digits = 2)),
    x = "AFINN",
    y = "Bing"
  )

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")


# explore words contribution to sentiment
data_afinn %>% 
  count(word, value, sort = TRUE) %>% 
  group_by(value) %>% 
  slice_max(n, n = 10) %>% 
  ungroup() %>% 
  mutate(
    value = as_factor(value),
    word = fct_reorder(word, n)
  ) %>% 
  ggplot(aes(x = n, y = word, fill = value)) +
  geom_col(colour = "gray50") +
  scale_fill_brewer(palette = "Purples", guide = "none") +
  facet_wrap(vars(value), scales = "free_y", nrow = 1) +
  labs(
    title = "AFINN - Words That Contribute to Sentiment Scores",
    x = "Contribution to sentiment",
    y = NULL
  )
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 11.69, 
  height = 8.27,
  dpi = 300, 
  bg = "transparent")

data_bing %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(x = n, y = word, fill = sentiment)) +
  geom_col(colour = "gray50") +
  scale_fill_brewer(palette = "Purples", guide = "none") +
  facet_wrap(vars(sentiment), scales = "free") +
  labs(
    title = "Bing - Words That Contribute to Positive and Negative Sentiment",
    x = "Contribution to sentiment",
    y = NULL
  )

ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# word contribution to sentiment by sex
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

data_bing %>% 
  count(sex, word, sentiment, sort = TRUE) %>% 
  group_by(sex, sentiment) %>% 
  slice_head(n = 10) %>% 
  ungroup() %>% 
  mutate(
    grp_var = str_c("Sex: ", sex, " Sentiment: ", str_to_title(sentiment)) %>% 
      fct_inorder(),
    word = reorder_within(word, n, grp_var)
  ) %>%
  ggplot(aes(x = n, y = word, fill = sex)) +
  geom_col() +
  scale_y_reordered() +
  theme(legend.position = "none") +
  facet_wrap(vars(grp_var), scales = "free", labeller = labeller(.multi_line = FALSE)) +
  labs(
    title = "Bing - Words That Contribute to Positive and Negative Sentiment by Sex",
    x = "Contribution to sentiment",
    y = NULL
  )
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

data_afinn %>% 
  count(sex, word, value, sort = TRUE) %>% 
  group_by(sex, value) %>% 
  slice_head(n = 5) %>% 
  ungroup() %>% 
  mutate(
    grp_var = str_c("Sex: ", sex, " value: ", str_to_title(value)) %>% 
      fct_inorder(),
    word = reorder_within(word, n, grp_var)
  ) %>%
  ggplot(aes(x = n, y = word, fill = sex)) +
  geom_col() +
  scale_y_reordered() +
  theme(legend.position = "none") +
  facet_wrap(vars(grp_var), scales = "free", labeller = labeller(.multi_line = FALSE)) +
  labs(
    title = "afinn - Words That Contribute to Positive and Negative Sentiment by Sex",
    x = "Contribution to sentiment",
    y = NULL
  )
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# word cloud visualisation 

set.seed(2345)
data_bing %>% 
  filter(sentiment == "negative") %>% 
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, min.freq = 1, max.words = 100))


set.seed(2345)
data_bing %>% 
  filter(sentiment == "positive") %>% 
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, min.freq = 1, max.words = 100))

set.seed(2345)
data_afinn %>% 
     count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, min.freq = 1, max.words = 100))
#filter(value == "positive") %>%

# association between sentiment score and review indicator

# Does net sentiment correlate with clinical review indication?
afinn_net %>% 
  ggplot(aes(x = clinicalreview, y = net_sentiment, fill = clinicalreview)) +
  geom_boxplot()
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

bing_net %>% 
  ggplot(aes(x = clinicalreview, y = net_sentiment, fill = clinicalreview)) +
  geom_boxplot()
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

# 1) test for normality - to assume normality has to be > 0.05

shapiro.test(afinn_net$net_sentiment)
shapiro.test(bing_net$net_sentiment)

# 2) run statistical test - if p > 0.05 then there is no difference in net sentiment between sex groups
wilcox_test(data = afinn_net, net_sentiment ~ clinicalreview)
wilcox_test(data = bing_net, net_sentiment ~ clinicalreview)
# t_test(data = afinn_net, net_sentiment ~ clinical_review)
# t_test(data = bing_net, net_sentiment ~ clinical_review)


# is it possible to train a predictive model on these data to predict refer based on words? would sentiment metrics be valuable predictor?
# units beyond words e.g., ngrams

# network graph analysis for word associations - understand the most frequent co-occuring words
library(igraph)
library(ggraph)

set.seed(3456)
data_bing %>% 
  group_by(sentiment) %>% 
  widyr::pairwise_count(word, feature = entry_id, sort = TRUE) %>% 
  ungroup() %>% 
  filter(sentiment == "negative") %>% 
  select(!sentiment) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "stress") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(linewidth = 5) +
  geom_node_text(
    aes(label = name), repel = TRUE, 
    point.padding = unit(0.2, "lines")
  ) +
  theme_graph() +
  labs(
    edge_alpha = "Co-occurrences",
    edge_width = "Co-occurrences"
  )
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")

data_bing %>% 
  group_by(sentiment) %>% 
  widyr::pairwise_count(word, feature = entry_id, sort = TRUE) %>% 
  ungroup() %>% 
  filter(sentiment == "positive") %>% 
  select(!sentiment) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "stress") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(linewidth = 5) +
  geom_node_text(
    aes(label = name), repel = TRUE, 
    point.padding = unit(0.2, "lines")
  ) +
  theme_graph() +
  labs(
    edge_alpha = "Co-occurrences",
    edge_width = "Co-occurrences"
  )
ggsave(
  filename = "plots.png",
  device = "png",
  path = "path/to/output/directory",
  width = 8.27, 
  height = 11.69,
  dpi = 300, 
  bg = "transparent")
