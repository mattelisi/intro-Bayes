
# Bayesian DDM example

rm(list=ls())
hablar::set_wd_to_script_path()

#
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores()) # indicate stan to use multiple cores if available
library(tidybayes)
library(rtdists)

# ----
# data
d <- read_csv("./data/ddm_data.csv")                      # load data

d %>%                                                 # make a plot
  mutate(resp = factor(resp)) %>%
  ggplot(aes(x=rt, color=resp, fill=resp))+
  geom_histogram(binwidth=0.05, color="white") +
  facet_grid(.~resp)

# ----
# format data adn fit model
data_stan <- list(
  N = nrow(d),
  rt = d$rt,
  resp = d$resp
)

ddm_fit <- stan(file = "../ddm4p.stan",  # ddm4p.stan is the file containing the model code
                data = data_stan, iter = 2000, chains = 4)

# results
print(ddm_fit)

# traceplot
traceplot(ddm_fit, pars=c("v","a","t0","z"))

# pairs plot
pairs(ddm_fit, pars=c("v","a","t0","z"))



# ----
# marginal posterior distribution on movdel parameters

# Extract samples and convert to long format for plotting
parameter_samples <- gather_draws(ddm_fit, v, a, t0, z)

# Create violin plot with 95% HDI intervals
ddm_fit %>%
  spread_draws(v, a, t0, z) %>%
  pivot_longer(cols = c(v, a, t0, z), names_to = "parameter", values_to = "value") %>%
  ggplot(aes(y = parameter, x = value)) +
  stat_halfeye(.width = .95,normalize="groups") +
  labs(x="value",y="parameter") 



# ----
# Visualize fit to the data

# Step 1: Extract samples
samples <- ddm_fit %>%
  spread_draws(v, a, t0, z) # Extracts the parameters

# Step 2: Compute average parameters
avg_params <- samples %>%
  summarise(across(c(v, a, t0, z), \(x) mean(x, na.rm = TRUE)))

# Use the average parameters for prediction
avg_v <- avg_params$v
avg_a <- avg_params$a
avg_t0 <- avg_params$t0
avg_z <- avg_params$z

# Step 3: Predict probability density for each data point and response type
rt_seq <- seq(min(d$rt), max(d$rt), length.out = 100)
predicted_densities <- expand.grid(rt = rt_seq, resp = c(0, 1))


predicted_densities$pd <- with(predicted_densities, ddiffusion(rt, response = ifelse(resp == 1, "upper", "lower"), 
                                                               a = avg_a, v = avg_v, t0 = avg_t0, 
                                                               z = avg_z * avg_a))
predicted_densities$resp <- factor(predicted_densities$resp)

# Step 4: Create a plot with facets for each response type
binwidth <- 0.05
d%>%
  mutate(resp = factor(resp)) %>%
  ggplot(aes(x = rt, fill=resp, color=resp, group=resp)) +
  geom_histogram(aes(y = ..count../sum(..count..)/binwidth), binwidth = 0.05, alpha = 0.75, color="white") +
  geom_line(data = predicted_densities, aes(x = rt, y = pd, group = resp),size = 1.5) +
  facet_wrap(~ resp) +
  labs(x = "Response Time", y = "Density")


