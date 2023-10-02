library(ggplot2)
library(hexSticker)
# FROM:
# https://www.datanovia.com/en/blog/how-to-create-icon-in-r/
# Also:
# https://nelson-gon.github.io/12/06/2020/hex-sticker-creation-r/

# Helper theme for ggplot icon
theme_icon <- function () {
    theme_void() + 
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA), 
      plot.background = element_rect(fill = "transparent", colour = NA), 
      legend.background = element_rect(fill = "transparent", colour = NA), 
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    )
}

p <- ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot(color = "white", fill = "transparent") +
  theme_icon()

# SVG
ggsave(
  filename = "./man/figures/optram-hex-icon_72px.svg", p, 
  dpi=72, width = 1, height = 1
  )

# PNG
ggsave(
  filename = "./man/figures/optram-hex-icon_72px.png", p, 
  dpi=72, width = 1, height = 1, bg = "transparent"
  )


p.sticker <- sticker(
  p, package="rOPTRAM", p_size=3, 
  s_x=1, s_y=1.1, s_width=1.3, s_height=1.5,
  h_color = "#478bca", h_fill = "#478bca",
  filename="man/figures/optram-hex-icon-sticker.png"
  )
