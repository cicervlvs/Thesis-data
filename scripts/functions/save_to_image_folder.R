save_to_image_folder <- function(name) {
  image_folder <- here("scripts", "analysis", "images")
  ggsave(paste0(name, ".png"),
    path = file.path(image_folder)
  )
}
