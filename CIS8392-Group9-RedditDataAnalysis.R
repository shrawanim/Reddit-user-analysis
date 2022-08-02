library(tidytext)
library(stringr)
library(dplyr)
library(kableExtra)
library(RedditExtractoR)
library(tidyverse)


b = read.csv("piracy_comments_bots.csv")


reddit_comments <- unique(b)


kable(reddit_comments[1000, ], caption = "Raw Reddit Data") %>%
  kable_styling(font_size = 10) %>%
  scroll_box(width = "100%", height = "200px")

kable(summary(reddit_comments), caption = "Data Summary") %>%
  kable_styling(font_size = 10) %>%
  scroll_box(width ="100%", height = "200px")  


library(lubridate)

reddit_comment_df <- reddit_comments %>%
  mutate(thread_id = url) %>%
  mutate(comm_date = dmy(date)) %>%
  mutate(comment = str_replace_all(comment,  "<.+>", " ")) %>%
  mutate(comment = str_replace_all(comment,  "\\W", " ")) %>%
  mutate(comment = str_replace_all(comment,  
                                   "www|https|http", " ")) %>%
  mutate(comment_score = as.numeric(score)) %>%
  select(comm_date, comment_score, thread_id, author, upvotes, downvotes,golds,
         comment_id, comment, bots) %>%
  unique() %>%
  na.omit()

reddit_df <- rbind(reddit_comment_df)

kable(reddit_df[8, ], caption = "Selected Reddit Data") %>%
  kable_styling(font_size = 10) %>%
  scroll_box(width = "100%", height = "200px")



reddit_df %>%
  group_by(month = floor_date(comm_date, "month")) %>%
  summarize(comments = n()) %>%
  ggplot(aes(month, comments)) +
  geom_line() +
  labs(title = "Comments Timeline") +
  scale_x_date(date_labels = "%b/%y", date_breaks = "4 months") +
  theme(axis.text.x = element_text(size = 7, angle = 45))

#number of comments
n_comments <- nrow(reddit_df)



threads <-reddit_comment_df %>%
  count(thread_id) %>%
  unique()

summary_df <- tibble("number_threads"=n_distinct(threads),
                     "number_comments"=n_comments)

summary_df %>%
  kable() %>%
  kable_styling(c("striped", "bordered"),
                full_width = FALSE, position = "left") %>%
  row_spec(row = 1, align = "right")


reddit_df %>%
  group_by(comm_date) %>%
  summarize(comments = n()) %>%
  filter(comments > 100) %>%
  ggplot(aes(comm_date, comments, fill = comments)) +
  geom_col() +
  coord_flip() +
  labs(title = "Date Wise Highest Number of Comments (>500 comments)")


top5 <- top_n(threads, 5) %>%
  arrange(desc(n))


top5 %>%
  kable(caption = "Top Five Thread by Comment Count") %>%
  kable_styling(font_size = 8)

user_df <- reddit_comments %>%
  group_by(author) %>%
  summarise(n = n()) %>%
  filter(n<20)


freq_users <- reddit_comments %>%
  anti_join(user_df, by = "author") %>%
  mutate(author = str_replace_all(author,  "\\Q[deleted]\\E", " ")) %>%
  group_by(author) %>%
  summarise(n = n())



freq_users %>%
  filter(author != " ") %>%
  ggplot((aes(x = n, y = author, color = author))) +
  geom_point(size=5) +
  labs(x = "number of comments", y = "author", title = "Users With Highest Number of Comments (>20 comments)")


library(ggthemes)

a=read.csv("piracy_threads.csv")
reddit_threads2= unique(a)

reddit_thread_df2 <- reddit_threads2 %>% filter(author!="[deleted]") %>%
  mutate(date = ymd(date)) %>%
  select(title, author, upvotes, comments, total_awards_received) %>%
  arrange(desc(total_awards_received)) %>%
  na.omit()



reddit_thread_df2 %>% slice_max(total_awards_received, n=5) %>%
  ggplot(aes(author, total_awards_received)) +
  geom_point(color='goldenrod',aes(size=upvotes)) + theme_pander() +
  ggtitle("Top awarded threads") + theme(axis.text.x = element_text(size = 6, angle = 45))


library(h2o)


target <- "bots"
predictors <-colnames(select(reddit_df,-c('comment_id','thread_id','comm_date','bots')))

h2o.init(nthreads = -1) 

data_h2o <- as.h2o(bind_cols(reddit_df))
#h2o.ls()


splits <- h2o.splitFrame(data = data_h2o, ratios = c(0.7, 0.15), seed = 1234)
train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
test_h2o  <- splits[[3]]



automl_h2o_models <- h2o.automl(
  x = predictors, 
  y = target,
  training_frame    = train_h2o,  
  validation_frame  = valid_h2o,  
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 600
)



automl_h2o_models@leaderboard

automl_leader <- automl_h2o_models@leader
