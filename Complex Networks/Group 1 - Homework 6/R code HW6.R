tot_self_loops_k1 <- c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0)
tot_multiple_edges_k1 <-c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0)
tot_self_loops_k2 <- c(0, 0, 2, 2, 1, 0, 0, 1, 1, 2, 0, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1, 1, 0, 1, 2, 1, 1, 2, 1, 0, 1, 2, 0, 3, 2, 1, 2, 1, 0, 1, 2, 1, 0, 2, 2, 1, 1, 0, 1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 0)
tot_multiple_edges_k2 <- c(0, 2, 0, 0, 0, 1, 2, 1, 0, 0, 2, 2, 0, 0, 1, 0, 1, 1, 1, 2, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 1, 2, 0, 1, 1, 1, 0, 1, 0, 3, 1, 0, 1, 2, 0, 0, 1, 2, 2, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1)


# Test t for self
t_test_self <- t.test(tot_self_loops_k1,tot_self_loops_k2, paired = FALSE)
print(t_test_self)

# Test t for multiple edges
t_test_multiple <- t.test(tot_multiple_edges_k1, tot_multiple_edges_k2, paired = FALSE)
print(t_test_multiple)
R.version.string


attempts_k1 <- c(2, 2, 3, 1, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 3, 1, 1, 1, 2, 2, 1, 1, 2, 1, 4, 3, 2, 1, 1, 1, 2, 2, 2, 1, 4, 2, 2, 1, 3, 1, 7, 1, 4, 2, 1, 3, 1, 1, 2, 5, 1, 4, 1, 1)
attempts_k2 <- c(80, 23, 12, 35, 11, 52, 3, 11, 44, 46, 33, 7, 46, 24, 14, 1, 3, 17, 3, 72, 20, 8, 63, 112, 5, 17, 69, 8, 3, 106, 153, 35, 3, 42, 14, 16, 112, 50, 79, 36, 10, 35, 91, 12, 31, 39, 23, 125, 8, 1, 17, 75, 15, 10, 17, 143, 34, 72, 144, 54)

t_test_attempts <- t.test(attempts_k1, attempts_k2, paired = FALSE)
print(t_test_attempts)

t_test_greater <- t.test(attempts_k1, attempts_k2, paired = FALSE, alternative = "less")
print(t_test_greater)







library(ggplot2)
log_degree_sublinear <- c(2.5649493574615367, 2.8903717578961645, 3.1780538303479458, 3.091042453358316, 2.9444389791664403, 2.3978952727983707, 1.6094379124341003, 2.772588722239781, 1.3862943611198906, 2.0794415416798357, 2.70805020110221, 2.302585092994046, 2.833213344056216, 1.791759469228055, 2.4849066497880004, 1.9459101490553132, 2.1972245773362196, 1.0986122886681098, 0.6931471805599453)
log_freq_sublinear <- c(1.3862943611198906, 0.0, 0.6931471805599453, 0.0, 0.6931471805599453, 1.3862943611198906, 3.58351893845611, 0.0, 3.6888794541139363, 2.1972245773362196, 1.0986122886681098, 1.791759469228055, 0.0, 2.6390573296152584, 1.0986122886681098, 1.791759469228055, 1.6094379124341003, 4.110873864173311, 0.0)
data_sublinear <- data.frame(
  log_degree = log_degree_sublinear,
  log_freq = log_freq_sublinear
)

fit_linear <- lm(log_freq ~ log_degree, data = data_sublinear)
r_squared_linear <- summary(fit_linear)$r.squared

fit_poly5 <- lm(log_freq ~ poly(log_degree, 4), data = data_sublinear)
r_squared_poly4 <- summary(fit_poly4)$r.squared


x11()
ggplot(data_sublinear, aes(x = log_degree, y = log_freq)) +
  geom_point() +
  geom_smooth(aes(color = "Linear Fit"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(aes(color = "Polynomial Fit"), method = "lm", formula = y ~ poly(x, 4), se = FALSE) +
  labs(x = "Log Degree", y = "Log Frequency", color = "Fit Type") +
  ggtitle("Log(Degree) vs Log(Frequency) (Sublinear)") +
  annotate("text", x = 2, y = 4, label = paste("R^2 = ", round(r_squared_linear, 4)), color = "#ee82ee") +
  annotate("text", x = 2, y = 3.5, label = paste("R^2 = ", round(r_squared_poly5, 4)), color = "#32cd32") +
  scale_color_manual(values = c("Linear Fit" = "#ee82ee", "Polynomial Fit" = "#32cd32")) +
  theme_minimal()


log_degree_superlinear <- c(3.6109179126442243, 5.198497031265826, 5.187385805840755, 4.969813299576001, 2.3978952727983707, 1.0986122886681098, 1.6094379124341003, 1.791759469228055, 1.9459101490553132, 1.3862943611198906)
log_freq_superlinear <- c(0.0, 0.0, 0.0, 0.0, 0.0, 5.099866427824199, 2.1972245773362196, 1.6094379124341003, 0.0, 2.772588722239781)
data_superlinear <- data.frame(
  log_degree = log_degree_superlinear,
  log_freq = log_freq_superlinear
)

fit_linear <- lm(log_freq ~ log_degree, data = data_superlinear)
r_squared_linear <- summary(fit_linear)$r.squared

fit_poly4 <- lm(log_freq ~ poly(log_degree, 4), data = data_superlinear)
r_squared_poly4 <- summary(fit_poly5)$r.squared

x11()
ggplot(data_superlinear, aes(x = log_degree, y = log_freq)) +
  geom_point() +
  geom_smooth(aes(color = "Linear Fit"), method = "lm", formula = y ~ x, se = FALSE) +
  geom_smooth(aes(color = "Polynomial Fit"), method = "lm", formula = y ~ poly(x, 4), se = FALSE) +
  labs(x = "Log Degree", y = "Log Frequency", color = "Fit Type") +
  ggtitle("Log(Degree) vs Log(Frequency) (Superlinear)") +
  annotate("text", x = 2, y = 4, label = paste("R^2 = ", round(r_squared_linear, 4)), color = "#ee82ee") +
  annotate("text", x = 2, y = 3.5, label = paste("R^2 = ", round(r_squared_poly5, 4)), color = "#32cd32") +
  scale_color_manual(values = c("Linear Fit" = "#ee82ee", "Polynomial Fit" = "#32cd32")) +
  theme_minimal()


packageVersion("ggplot2")
