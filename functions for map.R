data <- flights

numeric <- data %>% 
  select_if(~ is.numeric(.)) %>% 
  colnames()

my_histogram <- function(var){
    data %>% 
    ggplot(aes(.data[[var]])) +
    geom_histogram() +
    ggtitle(paste0("histogram for ", var))}

my_histogram("dep_time")

histograms <- map(numeric, my_histogram)

histograms %>% pluck(7)


flights %>% 
  sample_n(20) %>% 
  ggplot(aes(dest)) +
  geom_bar(aes(y = ..prop.., group = 1, fill = as_factor(..x..)))


flights %>% 
  count(dest, name = "count", sort = TRUE) %>%
  slice(1:30) %>% 
  mutate(prop = round(count/sum(count),2),
         dest = fct_reorder(dest, prop)) %>% 
  ggplot(aes(dest, prop, fill = dest)) +
  geom_col(show.legend = FALSE)

destination <- flights %>% 
  count(dest) %>% 
  sample_n(10) %>% 
  pull(dest)

flights %>%
  filter(dest %in% destination,
         arr_delay < quantile(arr_delay, 0.95, na.rm = TRUE)) %>% 
  sample_n(500) %>%
  ggplot(aes(arr_delay, color = dest)) +
  geom_freqpoly()

flights %>%
  filter(dest %in% destination,
         arr_delay < quantile(arr_delay, 0.99, na.rm = TRUE)) %>% 
  ggplot(aes(arr_delay, y = ..density..)) +
  geom_freqpoly(aes(color = dest))

flights %>% 
  filter(dest %in% destination) %>% 
  sample_n(400) %>% 
  count(dest, origin) %>% 
  ggplot(aes(x = dest, y = origin)) +
  geom_tile(aes(fill = n))

