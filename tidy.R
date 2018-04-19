library(tidyverse)
library(readxl)
library(stringr)

tidy_data <- function() {
  lp_2018_tidy <- load_data("lp_2018") %>% cut_and_bind_person_cols()
  sp_2018_tidy <- load_data("sp_2018") %>% cut_and_bind_person_cols()
  lp_2017_tidy <- load_data("lp_2017") %>% cut_and_bind_person_cols()
  sp_2017_tidy <- load_data("sp_2017") %>% cut_and_bind_person_cols()
  lp_2016_tidy <- load_data("lp_2016") %>% cut_and_bind_person_cols()
  sp_2016_tidy <- load_data("sp_2016") %>% cut_and_bind_person_cols()
  lp_2015_tidy <- load_data("lp_2015") %>% cut_and_bind_person_cols()
  sp_2015_tidy <- load_data("sp_2015") %>% cut_and_bind_person_cols()
  fl_2014_tidy <- load_data("fl_2014") %>% cut_and_bind_person_cols()
  fl_2013_tidy <- load_data("fl_2013") %>% cut_and_bind_person_cols()
  all_tidy <- lp_2018_tidy %>% 
    rbind(sp_2018_tidy) %>% 
    rbind(lp_2017_tidy) %>% 
    rbind(sp_2017_tidy) %>% 
    rbind(lp_2016_tidy) %>% 
    rbind(sp_2016_tidy) %>% 
    rbind(lp_2015_tidy) %>% 
    rbind(sp_2015_tidy) %>% 
    rbind(fl_2014_tidy) %>% 
    rbind(fl_2013_tidy)
  project_summary_table <- make_project_summary_table(all_tidy = all_tidy)
  write_csv(all_tidy, "results/all_tidy.csv")
  write_csv(project_summary_table, "results/project_summary_table.csv")
}

load_data <- function(sheet) {
  if (sheet == "lp_2018") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2018 LP",
      skip = 1,
      n_max = 9
    ) %>% 
      mutate(project_year = 2018, project_size = "large") %>% 
      select(project_title = 1, everything())
  } else if (sheet == "sp_2018") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2018 SP",
      skip = 1,
      n_max = 54
    ) %>% 
      mutate(project_year = 2018, project_size = "small") %>% 
      select(project_title = 1, everything())
  } else if (sheet == "lp_2017") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2017 LP",
      skip = 1,
      n_max = 15
    ) %>% 
      mutate(project_year = 2017, project_size = "large") %>% 
      select(project_title = 1, everything())
  } else if (sheet == "sp_2017") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2017 SP",
      skip = 1,
      n_max = 49
    ) %>% 
      mutate(project_year = 2017, project_size = "small") %>% 
      select(project_title = 1, everything())
  } else if (sheet == "lp_2016") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2016 LP",
      skip = 1,
      n_max = 16
    ) %>% 
      mutate(project_year = 2016, project_size = "large") %>% 
      select(project_title = 1, everything())
  } else if (sheet == "sp_2016") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2016 SP",
      skip = 1,
      n_max = 45
    ) %>% 
      mutate(project_year = 2016, project_size = "small") %>% 
      select(project_title = 1, everything())
  } else if (sheet == "lp_2015") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2015 LP",
      skip = 1,
      n_max = 20
    ) %>% 
      mutate(project_year = 2015, project_size = "large") %>% 
      select(project_title = 1, everything())
  } else if (sheet == "sp_2015") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2015 SP ",
      skip = 1,
      n_max = 40
    ) %>% 
      mutate(project_year = 2015, project_size = "small") %>% 
      select(project_title = 1, everything())
  } else if (sheet == "fl_2014") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2014 FL",
      skip = 1,
      n_max = 18
    ) %>% 
      mutate(project_year = 2014, project_size = NA) %>% 
      select(project_title = 1, everything())
  } else if (sheet == "fl_2013") {
    data <- read_xlsx(
      path = "data/faculty_engagement_2013-2018_feb19.xlsx", 
      sheet = "2013 FL ",
      skip = 1,
      n_max = 35
    ) %>% 
      mutate(project_year = 2014, project_size = NA) %>% 
      select(project_title = 1, everything())
  }
  return(data)
}

cut_and_bind_person_cols <- function(yearly_data) {
  yearly_data <- rename_person_cols(yearly_data)
  num_applicants <- yearly_data %>%
    names() %>% 
    str_detect("Applicant") %>% 
    sum()
  base_df <- yearly_data %>% 
    select(project_title, 
           project_year, 
           project_size, 
           Applicant__1, 
           Title__1, 
           Stream__1) %>% 
    rename(Applicant = Applicant__1,
           Title = Title__1,
           Stream = Stream__1) %>% 
    mutate(applicant_number = 1)
  for (i in 2:num_applicants) {
    this_df <- yearly_data %>% 
      select_(
        .dots = append(
          c("project_title", "project_year", "project_size"), 
          names(.)[str_detect(names(.), paste0("__", i))]
        )
      ) %>% 
      rename_("Applicant" = paste0("Applicant__", i),
              "Title" = paste0("Title__", i),
              "Stream" = paste0("Stream__", i)) %>% 
      filter(!is.na(Applicant)) %>% 
      mutate(applicant_number = i) %>% 
      select_(.dots = names(.)[!str_detect(names(.), "X")])
    base_df <- base_df %>% 
      rbind(this_df)
  }
  return(base_df)
}

rename_person_cols <- function(yearly_data) {
  names(yearly_data)[
    names(yearly_data) %in% get_person_cols(yearly_data)
    ] <- define_renamed_person_cols(yearly_data)
  return(yearly_data)
}

define_renamed_person_cols <- function(yearly_data) {
  person_cols <- get_person_cols(yearly_data)
  if ((any(str_detect(person_cols, "Status")))) {
    title <- "Status"
  } else {
    title <- "Title"
  }
  for (i in 1:length(person_cols)) {
    col <- person_cols[i]
    if (str_detect(col, "__")) {
      split_col <- str_split(col, "__")
      split_col[[1]][2] <- as.numeric(split_col[[1]][2]) + 1
      person_cols[i] <- paste(
        split_col[[1]][1], 
        split_col[[1]][2],
        sep = "__"
      )
    }
  }
  person_cols[person_cols == "PI"] <- "Applicant__1"
  person_cols[person_cols == title] <- paste0(title, "__1")
  person_cols[person_cols == "Stream"] <- "Stream__1"
  person_cols[person_cols == "Co-Applicant"] <- "Applicant__2"
  for (i in 1:length(person_cols)) {
    col <- person_cols[i]
    if (str_detect(col, "Co-Applicant")) {
      split_col <- str_split(col, "__")
      split_col[[1]][1] <- "Applicant"
      split_col[[1]][2] <- as.numeric(split_col[[1]][2]) + 1
      person_cols[i] <- paste(
        split_col[[1]][1], 
        split_col[[1]][2],
        sep = "__"
      )
    }
    if (str_detect(col, title)) {
      split_col <- str_split(col, "__")
      split_col[[1]][1] <- "Title"
      person_cols[i] <- paste(
        split_col[[1]][1], 
        split_col[[1]][2],
        sep = "__"
      )
    }
  }
  renamed_person_cols <- person_cols
  return(renamed_person_cols)
}

get_person_cols <- function(yearly_data) {
  if (any(str_detect(names(yearly_data), "Status"))) {
    pi_cols <- c("PI", "Status", "Stream")
    ca_cols <- names(yearly_data)[
      str_detect(names(yearly_data), "Co-Applicant")
      | str_detect(names(yearly_data), "Status__")
      | str_detect(names(yearly_data), "Stream__")
      ]
    person_cols <- append(pi_cols, ca_cols)
  } else {
    pi_cols <- c("PI", "Title", "Stream")
    ca_cols <- names(yearly_data)[
      str_detect(names(yearly_data), "Co-Applicant")
      | str_detect(names(yearly_data), "Title__")
      | str_detect(names(yearly_data), "Stream__")
      ]
    person_cols <- append(pi_cols, ca_cols)
  }
  return(person_cols)
}

make_project_summary_table <- function(all_tidy) {
  all_tidy %>% 
    group_by(project_title, project_year, project_size) %>% 
    summarise(PI_name = Applicant[which.min(applicant_number)],
              PI_title = Title[which.min(applicant_number)],
              PI_stream = Stream[which.min(applicant_number)],
              n_applicants = n_distinct(Applicant),
              n_research_stream = sum(str_detect(Stream, "Research")),
              n_teaching_stream = sum(str_detect(Stream, "Teaching")),
              n_admin_stream = sum(str_detect(Stream, "Admin")),
              n_student_stream = sum(str_detect(Stream, "Student")),
              n_librarian_stream = sum(str_detect(Stream, "Librarian")),
              n_other_stream = sum(str_detect(Stream, "Other"))) %>% 
    arrange(project_year, project_size, project_title)
}

tidy_data()