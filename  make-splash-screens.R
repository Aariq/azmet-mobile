# Script to make simple splash screens according to
# https://medium.com/@applification/progressive-web-app-splash-screens-80340b45d210

# TODO: make logo larger for the largest splash screen sizes.

library(magick)
library(tibble)
library(purrr)
library(glue)

logo <- image_read("www/images/logo.png")

dims <- tribble(
  ~width, ~height,
  640, 1136,
  750, 1294,
  1242, 2148,
  1125, 2436,
  1536, 2048,
  1668, 2224,
  2048, 2732
)

pwalk(dims, \(width, height) {
  image_composite(
    image_blank(width, height, color = "white"),
    logo,
    gravity = "Center",
  ) |>
    image_write(glue("www/images/splash/launch-{width}x{height}.png"))
})
