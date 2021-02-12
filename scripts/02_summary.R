# Test the size of the saved object  --------------------------------------------------
# Make dataframe of saved csv file sizes for each data frame
csv_objects <- list.files(here("proc"), full.names = T) %>% 
  tibble(file = .) %>%
  mutate(size = file.size(file)/1e6) %>%
  mutate(file = basename(file))  %>%
  filter(grepl(".csv",file)) %>%
  mutate(file = gsub("\\..*$", "", file)) %>%
  mutate(file = gsub("sample_", "", file)) %>% 
  filter(file != "read_results" & file != "save_results") %>% 
  arrange(size)

# Visualize ----------------------------------------------------------------------------
# Plot the time to save each object
time_saved <- bind_rows(df1_results, df2_results, df3_results, 
                        df4_results, df5_results, df6_results, 
                        df7_results, df8_results, df9_results) %>%
  summary() %>% 
  data.frame() %>%
  transmute(expr, mean = mean/1000) %>% 
  mutate(func = gsub("\\(d.*$|\\(dat.*$", "", expr))  %>%
  mutate(object = str_extract(string = expr, "df[1-9]"))  %>%
  mutate(file = object) %>%
  left_join(csv_objects, by =  "file") %>%
  rename("csv_size_mb" = "size")%>%
  mutate("csv_size_mb" = log(csv_size_mb)) %>%
  ggplot(aes(x = csv_size_mb, y = mean, group = func, color = func)) + 
  geom_point() + 
  geom_line() + 
  set_theme() + 
  ylab("Average time to save (seconds)") +
  xlab("Log of original csv size (mb)")+ 
  theme(legend.position = "bottom", 
        legend.title = element_blank())
ggsave(filename = here("results", "figures", "time_saved.png"), plot = time_saved, device = png())


# Plot the size of each saved object
size_saved <- list.files(here("proc"), full.names = T) %>% 
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
  mutate(csv_original_size = log(csv_original_size)) %>%
  ggplot(aes(x = csv_original_size, y = saved_size, group = func, color = func)) + 
  geom_point() + 
  geom_line() + 
  set_theme() + 
  xlab("Log of original csv size (mb)") +
  ylab("Saved size of .rds or .qs object (mb)") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank())
ggsave(filename = here("results", "figures", "size_saved.png"), plot = size_saved, device = png())


# Plot the time to read each object
time_read <- bind_rows(df1_read_results, df2_read_results, df3_read_results, 
                       df4_read_results, df5_read_results, df6_read_results, 
                       df7_read_results, df8_read_results, df9_read_results) %>%
  summary() %>% 
  data.frame() %>%
  transmute(expr, mean = mean/1000) %>% 
  mutate(func = gsub("\\(p.*$|\\(f.*$", "", expr))  %>%
  mutate(object = str_extract(string = expr, "df[1-9]"))  %>%
  mutate(file = object) %>%
  left_join(csv_objects, by =  "file") %>%
  rename("csv_size_mb" = "size") %>%
  mutate("csv_size_mb" = log(csv_size_mb)) %>%
  ggplot(aes(x = csv_size_mb, y = mean, group = func, color = func)) + 
  geom_point() + 
  geom_line() + 
  set_theme() + 
  ylab("Average time to read (seconds)") +
  xlab("Log of original csv size (mb)")+ 
  theme(legend.position = "bottom", 
        legend.title = element_blank())
ggsave(filename = here("results", "figures", "time_read.png"), plot = time_read, device = png())