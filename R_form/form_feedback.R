# analyse R questionnaire data

# load packages
library(googledrive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggimage)
library(palettetown)

# find file
x <- drive_find(n_max = 10, type = c("spreadsheet", "jpg"))

# download file of responses
d_use <- drive_download(as_id('1pXwZPANwn2RaWgJNXrEzSNIscEoaq1Zf60lZej8ohq4'), path = file.path('R_form', 'R_responses.csv'), type = 'csv', overwrite = TRUE)

# load in data
d_use <- read.csv(file.path('R_form', 'R_responses.csv'), stringsAsFactors = FALSE)

# change column names
colnames(d_use) <- c('Time', 'Learn_more', 'what_do_you_use', 'drop', 'R_exp', 'RStudio_exp', 'drop1', 'R_learn', 'drop2', 'format', 'present', 'name', 'email', 'R_use', 'other_tasks', 'hours_week', 'pkg_used', 'fave_pkgs')

# format data
d_use <- select(d_use, -starts_with('drop'))

# looking at range of programs used for analysis ####
# split columns and data
prog_use <- select(d_use, what_do_you_use, name) %>%
  mutate(., program = strsplit(.$what_do_you_use, ',')) %>%
  unnest(., program) %>%
  mutate(., program = trimws(program)) %>%
  group_by(., program) %>%
  summarise(n = n())

# plot
ggplot(prog_use, aes(forcats::fct_reorder(program, n, .desc = TRUE), n, fill = program)) +
  geom_bar(stat = 'identity') +
  xlab('') +
  ylab('Number of responses') +
  theme_bw(base_size = 12, base_family = 'Helvetica') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_blank()) +
  labs(title = 'Programs used for data analysis by the labgroup!') +
  scale_y_continuous(breaks = scales::pretty_breaks())

ggsave(file.path('R_form', 'prog_use.pdf'), last_plot(), width = 5.5, height = 5)
ggsave(file.path('R_form', 'prog_use.png'), last_plot(), width = 5.5, height = 5)

# looking at R and R studio proficiency ####
R_prof <- select(d_use, R_exp, RStudio_exp) %>%
  mutate_at(., c('R_exp', 'RStudio_exp'), as.numeric) %>%
  gather(., 'program', 'exp') %>%
  mutate(., image = ifelse(program == 'R_exp', "https://www.r-project.org/logo/Rlogo.png", 'https://www.rstudio.com/wp-content/uploads/2014/06/RStudio-Ball.png'))

# add pictures onto these
ggplot(R_prof, aes(program, exp, col = program, fill = program)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(geom = 'crossbar', position = position_dodge(width = 0.75), fatten = 0, color = 'white', width = 0.4, fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x)))}) +
  ylab('Experience') +
  xlab('') +
  geom_image(aes(image = image), position = position_jitter(width = 0.15, height = 0), size = 0.05) +
  ggtitle('Experience using R and RStudio') +
  labs(title = 'Experience using R and RStudio',
       subtitle = '1 = What is R/RStudio? \n10 = I am the R king/Hadley Wickham') +
  theme_bw(base_size = 12, base_family = 'Helvetica') +
  scale_y_continuous(breaks = 0:10, limits = c(0,10)) +
  theme(axis.text.x = element_blank(),
        legend.position = 'none') +
  scale_color_poke(pokemon = 'blastoise', spread = 2) +
  scale_fill_poke(pokemon = 'blastoise', spread = 2)

ggsave(file.path('R_form', 'Experience_using_R.pdf'), last_plot(), width = 5.25, height = 4.25)
ggsave(file.path('R_form', 'Experience_using_R.png'), last_plot(), width = 5.25, height = 4.25)

# look at what people want to learn ####


# Used packages ####

# Packages used ####

