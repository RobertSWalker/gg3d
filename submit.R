setwd("C:/Users/walkerro/onedrive - university of missouri/Desktop/")

library(dplyr);library(stringr);library(purrr);library(rvest)
library(RSelenium);library(XML);library(readxl);library(ggplot2);library(cowplot);theme_set(theme_cowplot())

df <- read_excel("amazon_gg.xlsx")

#group form into 4 categories
df$Form <- ifelse(df$form == 'general_geoglyphs', "other", 
                  ifelse(df$form == 'hexagon', "quadrangles",
                  ifelse(df$form == 'mound_villages', "other",
                  ifelse(df$form == 'pentagon', "quadrangles", df$form))))
table(df$Form)
df$Form <- ordered(df$Form, levels = c("quadrangles", "circles", "ovals", "other"))
df$type <- ordered(df$type, levels = c("excavated", "current"))

df %>% group_by(type) %>% summarise(median = median(depth, na.rm = TRUE))
df %>% group_by(type) %>% summarise(median = median(width, na.rm = TRUE))

(t <- ggplot(df, aes(y=depth, x=width, color=type))  +
    geom_point(size=.5) + #aes(color=Form)
    geom_smooth(method = "lm", se=T, formula=y~0+x) +
    xlab("Ditch width (m)") + ylab("Ditch depth (m)") +
    scale_colour_discrete(na.translate = F) +
    theme_cowplot() +
    theme(legend.title=element_blank() ) )
ggsave("depth by width.pdf") #, units = "in", width = 7, height=5)

#run brms model
library(brms);library(rstan);options(mc.cores = parallel::detectCores());rstan_options(auto_write = TRUE)
priors = c(#set_prior("normal(0, 5)", class = "Intercept"),
  set_prior("normal(0, .2)", class = "b"))
priors

m2b <- brm(#family = poisson(link = "identity"),
  bf(depth ~ 0 + width:type + (1|type) + s(lat, lon),
     sigma ~ 1 + (1|type)),
  prior = priors,
  family = gaussian,
  data = df,
  iter =  1e4, chains = 4, cores = 4, save_all_pars = TRUE,
  control = list(adapt_delta = .999, max_treedepth = 20),
  seed = 14, sample_prior = T)
prior_summary(m2b)
m2b
pl <- plot(m2b, N = 4, ask = FALSE) #Trace and Density Plots for MCMC Samples
posterior_summary(m2b)
bayes_R2(m2b) #.65
conditional_effects(m2b, points=T)
saveRDS(m2b,"m2b.Rds")
m2b <- readRDS("m2b.Rds")
pp_check(m2b)

plot(hypothesis(m2b, "width:typeexcavated > 0"))
plot(hypothesis(m2b, "width:typecurrent > 0"))

#posterior plot https://cran.r-project.org/web/packages/bayesplot/vignettes/plotting-mcmc-draws.html
posterior <- as.array(m2b) #full model
dim(posterior)
dimnames(posterior)
library(bayesplot)
plt1 <- mcmc_areas(
  posterior, 
  pars = c("b_width:typeexcavated" ,"b_width:typecurrent"),
  prob = 0.95, # 80% intervals
  point_est = "mean"
)
plt1
plot1 <- plt1 + 
  annotate(geom="text", x=.2, y=2.5, label="Width effect\non excavated depth",color="black", size=4) +
  annotate(geom="text", x=.2, y=1.5, label="Width effect\non current depth",color="black", size=4) +
  ylab("Density") + xlab("Posterior estimates") +
  geom_vline(xintercept=0, linetype=3) +
  scale_x_continuous(limits = c(0,.4), breaks = round(seq(0, .3, by = .1),2), expand = c(0, 0)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())
plot1  

library(ggpubr)
figure <- ggarrange(t, plot1, labels = c("A", "B"), ncol = 2, nrow = 1, align="hv")
figure

df2 <- df
#choose either current or excavated
df2$type <- "excavated"

#predict excavated depth 
df2$depth_prediction <- predict(m2b, df2, probs = c(.25,.75))[,1] #1 is mean, 2 error, 3 lower q2.5, 4 upper

#predict current depth 
df2$type <- "current"
df2$depth_prediction_current <- predict(m2b, df2, probs = c(.25,.75))[,1] #1 is mean, 2 error, 3 lower q2.5, 4 upper

#make actual depth and width
df2$actual_depth <- df2$depth_prediction - df2$depth_prediction_current /2
df2$actual_width <- df2$width *2/3

#get perimeters
df2$circle_perimeter <- df2$side1 * pi
df2$oval_perimeter <- (df2$side1 + df2$side2)/2*pi
df2$quad_perimeter <- df2$side1 *2 + df2$side2*2
df2$square_perimeter <- df2$side1 *4
df2$perimeter <- ifelse( is.na(df2$perimeter) &
                           df2$Form == "quadrangles" &
                           is.na(df2$side2), df2$square_perimeter, df2$perimeter )
df2$perimeter <- ifelse( is.na(df2$perimeter) &
                           df2$Form == "quadrangles" &
                           !is.na(df2$side2), df2$quad_perimeter, df2$perimeter )
df2$perimeter <- ifelse( is.na(df2$perimeter) &
                           df2$Form == "circles" &
                           is.na(df2$side2), df2$circle_perimeter, df2$perimeter )
df2$perimeter <- ifelse( is.na(df2$perimeter) &
                           df2$Form == "ovals", df2$oval_perimeter, df2$perimeter )
df2$perimeter <- ifelse( is.na(df2$perimeter) &
                           df2$Form == "ovals" &
                           is.na(df2$side2), df2$circle_perimeter, df2$perimeter )
df2$perimeter <- ifelse( is.na(df2$perimeter) &
                           df2$Form == "other" &
                           is.na(df2$side2), df2$circle_perimeter, df2$perimeter )
df2$perimeter <- ifelse( is.na(df2$perimeter) &
                           df2$Form == "other" &
                           !is.na(df2$side2), df2$oval_perimeter, df2$perimeter )

#find multiples
df2$perimeter <- ifelse(grepl("Double", df2$place), df2$perimeter *2, df2$perimeter)
df2$perimeter <- ifelse(grepl("Triple", df2$place), df2$perimeter *3, df2$perimeter)

#get half cylinder volume
hcvolume <- df2$actual_width * df2$actual_depth * df2$perimeter * pi / 4 
median(hcvolume, na.rm=T) #5350
sum(hcvolume, na.rm=T) #8.5 million
sum(hcvolume, na.rm=T)*(1-.2337) #convert to sine method 6.5 million

#get sine volume
df2$area <- df2$actual_depth * 2 * df2$actual_width / pi
median(df2$area, na.rm=T) #8.2
df2$volume <- df2$area * df2$perimeter

median(df2$volume, na.rm=T) #4336, lower 3285  upper 5388
median(df2$perimeter, na.rm=T) #506
median(df2$actual_depth, na.rm=T) #2.15 
mean(df2$actual_width, na.rm=T) #6.3

hist(df2$volume, 50)
max(df2$volume, na.rm = T) 
df2[df2$volume == max(df2$volume, na.rm=T),]
sum(df2$volume, na.rm=T) #m2b: 6.92 million m3 , lower 5.40, upper 8.44

(p <- df2 %>%
  ggplot( aes(x=volume, fill=Form)) +
  geom_histogram( color="#e9ecef", alpha=0.2, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080", 'gray', "blue")) +
  labs(fill=""))

df2$Form2 <- df2$Form
library(forcats)
df2$Form2 <- fct_collapse(df2$Form2, ellipses = c("ovals","circles"))
df2$Form2 <- ordered(df2$Form2, levels = c("quadrangles", 'ellipses', 'other'))

library(forcats);library(viridis);library(scales)
dat_text <- data.frame(
  label = c("5.3k", "3.1k", '4.3k'),
  Form2   = c("quadrangles", "ellipses", 'other'),
  x     = c(8000,6000,7000),
  y     = c(60, 70, 70)
)
dat_text$Form2 <- ordered(dat_text$Form2, levels = c("quadrangles", 'ellipses', 'other'))

(p <- df2 %>%
    group_by(Form2) %>%
    mutate(med_vol = median(volume, na.rm=T) ) %>%
  ggplot( aes(x=volume, color=Form2, fill=Form2)) +
  geom_histogram(alpha=1, binwidth = 1000, size = .1) +
  geom_vline(aes(xintercept= med_vol), colour='red') +
  geom_text(
      data    = dat_text,
      mapping = aes(x = x, y = y, label = label)
    )
  +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  scale_x_continuous(limits = c(0,65000), breaks = round(seq(0, 140000, by = 10000),1), expand = c(0, 0), labels = label_number(suffix = "k", scale = 1e-3)) + #
  theme(
    legend.position="none",
    panel.spacing = unit(.3, "lines"),
  ) +
  xlab(expression(Volume~(m^{"3"}))) +
  ylab("Frequency") +
  facet_wrap(~Form2, ncol=1)) 
ggsave("volume.pdf", units = "in", width = 6, height=4)

df2 %>% group_by(Form2) %>% summarise(median = median(volume, na.rm = TRUE))

