library('fpp3')

# calculate lambda
lambda <- aus_arrivals %>%
  filter(Origin == "UK") %>%
  features(Arrivals, features = guerrero) %>%
  pull(lambda_guerrero)

# plot original data
aus_arrivals %>%
  filter(Origin == "UK") %>%
  autoplot()  +
  labs(y = "",
       title = "International Arrivals to Australia from UK", 
       subtitle = "by Quarter")

# plot data with best transformation according to lambda value
aus_arrivals %>%
  filter(Origin == "UK") %>%
           autoplot(box_cox(Arrivals, lambda)) +
           labs(y = "",
                title = "International Arrivals to Australia from UK", 
                subtitle = "by Quarter")

# apply box-cox outcome to original dataset (log)
aus_arrivals_transformed <- aus_arrivals %>%
  filter(Origin == "UK") %>%
  mutate(Arrivals = log(Arrivals))

# decompose via STL and graph
aus_arrivals_transformed %>%
  model(
    STL(Arrivals ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()
