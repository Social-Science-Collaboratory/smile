
library(tidyverse)

openface_output_list <- Sys.glob(
  file.path("data", "gorilla_survey", "gorilla-v*-p*", "OpenFace_output", "*.csv")
)

openface_results <- lapply(
    openface_output_list, function(openface_output_file) {
        df <- read.csv(
            openface_output_file) |>
            summarise(
                success = mean(success),
                AU12_r = mean(AU12_r),
                AU12_c = mean(AU12_c)
                ) |>
            mutate(
                videoID = sub(".*/(.*)_processed\\.csv",
                "\\1", 
                openface_output_file))
        
        return(df)
    } 
) |>
bind_rows()


openface_results <- openface_results |>
separate(videoID, 
into = c(
    "Experiment_ID",
    "Experiment_Version",
    "Gorilla_ID",
    "task_pre",
    "task",
    "schedule_ID",
    "recording",
    "rep_number",
    "screen_counter"
    )) |>
select(-c(Experiment_ID,Experiment_Version, task_pre, schedule_ID, recording, screen_counter))

smile_task = c("s1vq", "21bf", "k1jv", "2dqb", "zk4a", "dbv3", "v86q", "6966")

natura_task = c("w23m", "9mm1", "1a4k", "irdf", "udup", "kluz", "45jz", "x7ha")

fill1_task = c("e2jx")

fill4_task = c("44y9")

openface_results <- openface_results |>
mutate(face = case_when(
  task %in% smile_task ~ "smile",
  task %in% natura_task ~ "natura",
  task %in% fill1_task ~ "fill1",
  task %in% fill4_task ~ "fill4",
    TRUE ~ NA_character_
))

openface_results_summ <- openface_results |>
    group_by(Gorilla_ID, face) |>
    filter(face %in% c("natura", "smile")) |>
    summarise(
        face_detection = mean(success),
        AU12_scalar = mean(AU12_r),
        AU12_binary = mean(AU12_c)) |>
    pivot_wider(
        id_cols = Gorilla_ID,
        names_from = face,
        values_from = c(face_detection, AU12_scalar, AU12_binary),
        names_glue = "{.value}_{face}") |>
    mutate(
        face_compliance_scalar = ifelse(
            (face_detection_natura > 0.8 &
            face_detection_smile > 0.8 &
            AU12_scalar_natura < 1.5 &
            AU12_scalar_smile >= 1.5),
            TRUE,
            FALSE
        ),
        face_compliance_binary = ifelse(
            (face_detection_natura > 0.8 &
            face_detection_smile > 0.8 &
            AU12_binary_natura < 0.5 &
            AU12_binary_smile >= 0.5),
            TRUE,
            FALSE
        )
    )


write_csv(openface_results_summ, "data/smile25b_openface_processed.csv")


