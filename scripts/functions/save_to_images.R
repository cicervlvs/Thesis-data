save_to_images <- function(name) {
  image_folder <- here("scripts", "analysis", "images")
  ggsave(paste0(name, ".png"),
    path = image_folder
  )
}
