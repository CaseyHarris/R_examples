#Animation of images

#Load libraries
library(magick) #ImageMagick package, there is probably a newer package now (gganimate?) but this one still works

#These are the names of the files that I want to combine into a single animation
filenames <- c("png20170322.png",
               "png20170330.png",
               "png20170402.png",
               "png20170406.png",
               "png20170407.png",
               "png20170421.png",
               "png20170425.png",
               "png20170503.png",
               "png20170507.png",
               "png20170511.png",
               "png20170522.png",
               "png20170526.png",
               "png20170527.png",
               "png20170530.png",
               "png20170603.png")

for(i in 1:length(filenames)) {
  
  assign(filenames[i], image_read(filenames[i])) #loads each of the desired cropped rasters into the gloabl environment, using ImageMagick's image_read function

  }

img <- lapply(ls(pattern="*.png"), function(x) get(x)) #creates a list of the names of any objects in the global environment ending in .png
img <- Reduce(c, img) #changes the list from a list of names to a vector of the objects

animation <- image_animate(image_scale(img, "500x500"), fps=1, dispose="previous") #animate
animation
image_write(animation, "Lake_George_animation.gif")


#But non-moving images are easier to look at
#Choose some images and plot in grid

#Selected days for Lake George
rm(list=ls()) #clears any objects out of the global environment

filenames <- c("png20170322.png",
               "png20170330.png",
               "png20170402.png",
               "png20170406.png",
               "png20170407.png",
               "png20170417.png",
               "png20170421.png",
               "png20170425.png",
               "png20170503.png",
               "png20170507.png",
               "png20170511.png",
               "png20170522.png",
               "png20170526.png",
               "png20170527.png",
               "png20170530.png",
               "png20170603.png")

for(i in 1:length(filenames))
  assign(filenames[i], image_read(filenames[i]))

left_to_right1 <- image_append(image_scale(c(png20170322.png, png20170330.png, png20170402.png, png20170406.png), "x600"))
left_to_right2 <- image_append(image_scale(c(png20170407.png, png20170417.png, png20170421.png, png20170425.png), "x600"))
left_to_right3 <- image_append(image_scale(c(png20170503.png, png20170507.png, png20170511.png, png20170522.png), "x600"))
left_to_right4 <- image_append(image_scale(c(png20170526.png, png20170527.png, png20170530.png, png20170603.png), "x600"))

top_to_bottom1 <- image_append(image_scale(c(left_to_right1, left_to_right2), "1000"), stack = TRUE)
top_to_bottom2 <- image_append(image_scale(c(left_to_right3, left_to_right4), "1000"), stack = TRUE)
top_to_bottom3 <- image_append(image_scale(c(top_to_bottom1, top_to_bottom2), "1000"), stack = TRUE)
top_to_bottom3
image_write(top_to_bottom3, path="Lake_George_grid.png", format = 'png')

# ...and this probably would have looked nicer using Photoshop or even MS Paint
# but with R it is easily reproducible when you have to do it again next year
