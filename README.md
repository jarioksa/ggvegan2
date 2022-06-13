# ggvegan2

**ggvegan2** provides **ggplot2** graphics for some **vegan**
objects. The package is complimentary to **ggvegan** package which has
larger choice of functions. Currently it contains:

- Flexible and configurable plotting for most **vegan** ordination
  objects. These are an alternative with contrasting philosophy to
  `autoplot` functions in **ggvegan** package. The basic function is
  `ordiggplot` that only sets up the graphic data frame to which users
  can add ordination elements with `geom_ordipoint`, `geom_orditext`,
  `geom_ordilabel` and `geom_ordiarrow`. It also provides support
  functions for adding fitted environmental vectors in ordination.
  Moreover, it provides function `ggscores` that can be used to pass
  `ordiggplot` elements to standard **ggplot2** geometries.
  
- `autoplot` functions for Rank-Abundance Distribution models that
  provide a **ggplot2** alternative to Lattice graphics in **vegan**.
  