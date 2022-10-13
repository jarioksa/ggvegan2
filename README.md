# ggvegan2

**ggvegan2** provides **ggplot2** graphics for some **vegan**
objects. Current versions of **vegan** have support of **ggplot2**
compatible `scores` of many result objects. This package provides
functions that can directly access those scores to build graphics
using **ggplot2** style `geom_` functions with full control. The
package also provides **ggplot2** alternatives for some **lattice**
panel functions.

The package is complimentary to **ggvegan** package which has larger
choice of functions and supports a wider range of **vegan**
methods. The **ggvegan** package is mainly built on `fortify` to
extract data and `autoplot` to fast and easy graphics.

Currently **ggvegan2** contains:

- Flexible and configurable plotting for most **vegan** ordination
  objects. These are an alternative with contrasting philosophy to
  `autoplot` functions in **ggvegan** package. The basic function is
  `ordiggplot` that only sets up the graphic data frame to which users
  can add ordination elements with `geom_ordipoint`, `geom_orditext`,
  `geom_ordilabel` and `geom_ordiarrow`. It also provides support
  functions for adding fitted environmental vectors in ordination.
  Moreover, it provides function `ggscores` that can be used to pass
  `ordiggplot` elements to standard **ggplot2** geometries.
  
- Alternatives to **lattice** graphics function in **vegan** using
  `autoplot`and `fortify`.  Currently implemented for Rank-Abundance
  Distributions modes (`radline`, `radfit`, `radfit.frame`),
  `permustats` diagnostic plots, and Rényi diversities (`renyi`,
  `renyiaccum`and shold also work for similar `tsallis`
  functions). The plan is to phase out **lattice** graphics from
  **vegan** in favour of these **ggplot2** alternatives.
