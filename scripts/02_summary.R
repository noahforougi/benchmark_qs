library(tidyverse)
library(here)
source(file = "https://raw.githubusercontent.com/skhiggins/R_guide/master/scripts/programs/set_theme.R")

save_results <- read.csv(here("results", "save_results.csv"))
read_results <- read.csv(here("results", "read_results.csv"))

csv_objects <- list.files(here("proc"), full.names = T) %>% 
  tibble(file = .) %>%
  mutate(size = file.size(file)/1e6) %>%
  mutate(file = basename(file))  %>%
  filter(grepl(".csv",file)) %>%
  mutate(file = gsub("\\..*$", "", file)) %>%
  mutate(file = gsub("sample_", "", file)) %>% 
  filter(file != "read_results" & file != "save_results") %>% 
  arrange(size)

# Graph the time it takes to read each objects
read_df <- read_results %>% 
  data.frame() %>%
  transmute(expr, mean = mean/1000) %>% 
  mutate(func = gsub("\\(d.*$|\\(dat.*$|\\(.*", "", expr))  %>%
  mutate(object = str_extract(string = expr, "df[1-9]"))  %>%
  mutate(file = object) %>%
  left_join(csv_objects, by =  "file") %>%
  mutate(file = gsub("\\..*$", "", file)) %>%
  
  rename("csv_size_mb" = "size")%>%
  mutate("log_csv_size_mb" = log(csv_size_mb)) %>%
  mutate(log_mean = log(mean)) 

read_df %>%
  mutate(func = factor(func,levels = c("qread", "read_rds", "read.fst", "readRDS"))) %>% 
  ggplot(aes(x = csv_size_mb, y = mean, group = func, color = func)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  #scale_x_continuous(breaks = 1:9, labels = seq(20, 180, 20), expand = expansion(mult = c(0.05,0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) + 
  scale_x_continuous(trans = "log", 
                     labels = scales::label_comma(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.1))) + 
  set_theme() + 
  ylab("Average time to save (seconds)") +
  xlab("Original csv size (mb)") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank())

ggsave(plot = last_plot(), 
       filename = here("results", "figures", "time_to_read.png"), 
       width = 7, height = 7)

# Graph the time it takes to save each object
save_df <- save_results  %>% 
  data.frame() %>%
  transmute(expr, mean = mean/1000) %>% 
  mutate(func = gsub("\\(d.*$|\\(dat.*$|\\(.*", "", expr))  %>%
  mutate(object = str_extract(string = expr, "df[1-9]"))  %>%
  mutate(file = object) %>%
  left_join(csv_objects, by =  "file") %>%
  mutate(file = gsub("\\..*$", "", file)) %>%
  rename("csv_size_mb" = "size")%>%
  mutate("log_csv_size_mb" = log(csv_size_mb)) %>%
  mutate(log_mean = log(mean)) 

save_df %>%
  mutate(func = factor(func,levels = c("qsave", "write_rds", "write.fst", "saveRDS"))) %>% 
  ggplot(aes(x = csv_size_mb, y = mean, group = func, color = func)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(trans = "log", 
                     labels = scales::label_comma(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.1))) + 
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) + 
  set_theme() + 
  ylab("Average time to save (seconds)") +
  xlab("Original csv size (mb)") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank())

ggsave(plot = last_plot(), 
       filename = here("results", "figures", "time_to_save.png"), 
       width = 7, height = 7)


# Graph the size of each saved object
# Plot the size of each saved object
size_df <- list.files(here("proc"), full.names = T) %>% 
  tibble(file = .) %>%
  mutate(saved_size = file.size(file)/1e6) %>%
  mutate(file = basename(file)) %>% 
  filter(!grepl(".csv",file)) %>%
  mutate(file = gsub("\\..*$", "", file)) %>% 
  arrange(file) %>%
  mutate(func = gsub("df[1-9]_", "", file))  %>%
  mutate(file = gsub("_.*$", "", file)) %>%
  left_join(csv_objects, by = "file") %>%
  rename("csv_original_size" = "size") %>%
  mutate(log_csv_original_size = log(csv_original_size)) %>%
  mutate(log_saved_size = log(saved_size)) %>% 
  mutate(func = ifelse(func == "writefst", "write.fst", func))
size_df %>%
  mutate(func = factor(func,levels = c("qsave", "write_rds", "write.fst", "saveRDS"))) %>% 
  ggplot(aes(x = csv_original_size, y = saved_size, group = func, color = func)) + 
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(trans = "log", labels = scales::label_comma(accuracy = 1), expand = expansion(mult = c(0.01,0.1))) + 
  scale_y_continuous(trans = "log", labels = scales::label_comma(accuracy = 1)) + 
  #scale_x_continuous(breaks = 0:6, labels = round(seq(0, 800, 800/6)), expand = expansion(mult = c(0.01,0.05))) +
  #scale_y_continuous(breaks = 1:6, labels = seq(800/6, 800, 800/6), expand = expansion(mult = c(0.05,0.05))) +
  set_theme() + 
  xlab("Original csv size (mb)") +
  ylab("Saved size of object (mb)") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank()) 

ggsave(plot = last_plot(), 
       filename = here("results", "figures", "size_object_saved.png"), 
       width = 7, height = 7)








# # Test the size of the saved object  --------------------------------------------------
# # Make dataframe of saved csv file sizes for each data frame
# csv_objects <- list.files(here("proc"), full.names = T) %>% 
#   tibble(file = .) %>%
#   mutate(size = file.size(file)/1e6) %>%
#   mutate(file = basename(file))  %>%
#   filter(grepl(".csv",file)) %>%
#   mutate(file = gsub("\\..*$", "", file)) %>%
#   mutate(file = gsub("sample_", "", file)) %>% 
#   filter(file != "read_results" & file != "save_results") %>% 
#   arrange(size)
# 
# # Visualize ----------------------------------------------------------------------------
# # Plot the time to save each object
# time_saved <- bind_rows(df1_results, df2_results, df3_results, 
#                         df4_results, df5_results, df6_results, 
#                         df7_results, df8_results, df9_results) %>%
#   summary() %>% 
#   data.frame() %>%
#   transmute(expr, mean = mean/1000) %>% 
#   mutate(func = gsub("\\(d.*$|\\(dat.*$", "", expr))  %>%
#   mutate(object = str_extract(string = expr, "df[1-9]"))  %>%
#   mutate(file = object) %>%
#   left_join(csv_objects, by =  "file") %>%
#   rename("csv_size_mb" = "size")%>%
#   mutate("csv_size_mb" = log(csv_size_mb)) %>%
#   ggplot(aes(x = csv_size_mb, y = mean, group = func, color = func)) + 
#   geom_point() + 
#   geom_line() + 
#   set_theme() + 
#   ylab("Average time to save (seconds)") +
#   xlab("Log of original csv size (mb)")+ 
#   theme(legend.position = "bottom", 
#         legend.title = element_blank())
# ggsave(filename = here("results", "figures", "time_saved.png"), plot = time_saved, device = png())
# 
# 
# # Plot the size of each saved object
# size_saved <- list.files(here("proc"), full.names = T) %>% 
#   tibble(file = .) %>%
#   mutate(saved_size = file.size(file)/1e6) %>%
#   mutate(file = basename(file)) %>% 
#   filter(!grepl(".csv",file)) %>%
#   mutate(file = gsub("\\..*$", "", file)) %>% 
#   arrange(file) %>%
#   mutate(func = gsub("df[1-9]_", "", file))  %>%
#   mutate(file = gsub("_.*$", "", file)) %>%
#   left_join(csv_objects, by = "file") %>%
#   rename("csv_original_size" = "size") %>%
#   mutate(csv_original_size = log(csv_original_size)) %>%
#   ggplot(aes(x = csv_original_size, y = saved_size, group = func, color = func)) + 
#   geom_point() + 
#   geom_line() + 
#   set_theme() + 
#   xlab("Log of original csv size (mb)") +
#   ylab("Saved size of .rds or .qs object (mb)") + 
#   theme(legend.position = "bottom", 
#         legend.title = element_blank())
# ggsave(filename = here("results", "figures", "size_saved.png"), plot = size_saved, device = png())
# 
# 
# # Plot the time to read each object
# time_read <- bind_rows(df1_read_results, df2_read_results, df3_read_results, 
#                        df4_read_results, df5_read_results, df6_read_results, 
#                        df7_read_results, df8_read_results, df9_read_results) %>%
#   summary() %>% 
#   data.frame() %>%
#   transmute(expr, mean = mean/1000) %>% 
#   mutate(func = gsub("\\(p.*$|\\(f.*$", "", expr))  %>%
#   mutate(object = str_extract(string = expr, "df[1-9]"))  %>%
#   mutate(file = object) %>%
#   left_join(csv_objects, by =  "file") %>%
#   rename("csv_size_mb" = "size") %>%
#   mutate("csv_size_mb" = log(csv_size_mb)) %>%
#   ggplot(aes(x = csv_size_mb, y = mean, group = func, color = func)) + 
#   geom_point() + 
#   geom_line() + 
#   set_theme() + 
#   ylab("Average time to read (seconds)") +
#   xlab("Log of original csv size (mb)")+ 
#   theme(legend.position = "bottom", 
#         legend.title = element_blank())
# ggsave(filename = here("results", "figures", "time_read.png"), plot = time_read, device = png())# 