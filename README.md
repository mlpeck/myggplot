# Package myggplot

A very basic package for (mostly) ggplot2 based plots. The functions currently implemented include:

* `qplot`. Pilfered directly from the function of the same name in ggplot2. I removed the annoying _deprecated_ warnings
  and prefaced some undocumented and unexported ggplot2 function calls with `ggplot2:::`.

* `ggimage`. A simple raster image function using ggplot2's `geom_raster`.

* `greyimage` aka `grayimage`. A base graphics raster image function.

M.L. Peck
