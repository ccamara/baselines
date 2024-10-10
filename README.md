


# baselines - a drawing toolbox with visual overlay guides for R graphics (base)

<img width="250" align="right" src="https://github.com/GregMci/baselines/blob/master/man/figures/hexLogo.png" />


*“In order to produce graphical output, the user calls a series of graphics functions, each of which produces either a complete plot or adds some output to an existing plot."*

*"R graphics follows a 'painters model,' which means that the graphics output occurs in steps, with later output drawn on top of any previous output.”* 

Paul Murrell (2019) 

[Page 1 / Chapter 1. An Introduction to R Graphics. R Graphics Third Edition. CRC Press.](https://www.stat.auckland.ac.nz/~paul/RG3e/) <br clear="left"/> https://www.routledge.com/R-Graphics-Third-Edition/Murrell/p/book/9781498789059 
<br clear="left"/>



#### baselines follows this 'painters model' with functionality falling into five general themes:

#### *1. Drawing Graphics Within Plots*

Simple drawing tools - like a gradient filled box,`rect_fade()` - are made easily accessible through R functions. The drawing tools relate to graphics objects and methods which are commonly needed to creating and annotating bespoke visualisations. Each is based on often repeated coding tasks like creating a circle, arc, sector, or donut sector (`arc_shape()`, `arc_points()`), creating a closed shape above, below, or to the left or right, of a line (e.g. `close_shape_`). A variety of functionality is provided, including a simple `plot_text_box()`, `points_betwen_points()`) and `lines_as_segments()` to colour or format individual parts of lines according to data. Simple shape objects are provided for annotation, such as `rect_sides()` which produces a rectangle where selected sides are drawn rather than all four. Straightforward drawing tools, like drawing a rectangle for the `plot_background()` are included. 


#### *2. Drawing Graphics on the Canvas/Device*  

Drawing objects outside of plots may not always be in relation to the plot region, and instead be in relation to the device ‘canvas’ region. These drawing tools enable some similar functions to drawing graphics within plots, but given the whole device, irrespective of the layout or plot region values. The tool kit include a `text_box()`, `canvas_line()`, `canvas_rect()` and `canvas_background()`.


#### *3. Simplifying graphics components*

Quick access to components of the `axis()` and `title()` functions are made possible with `axis_labels()`, `axis_ticks()`& `axis_line()`, and `text_main_title()`, `text_sub_title()`, `text_x_label()` & `text_y_label()`. The argument names are the simplified so, for example, across all these functions the `col` argument is used beacuse the function name specifies what `col` refers to, rather than object-specific arguments.


#### *4. Visual queries* 

Many crucial features of visualisations are present but can be invisible and queried through code, such as plot margins (`overlay_margins()`, `overlay_outer_margins()`) and figure regions (`box_outer()`), or as in the case of device coordinates (`canvas_grid()`), are only made visible when used to draw as part of another function. Visual overlays are not necessarily intended to be a component of the final plot, rather as guides and measuring tools used during the design and development of visualisations. 


#### *5. Accessible Settings*

Although accessible through code, many system and device `options()`, and graphics `par`-ameters do not follow a structure allowing autocomplete in Rstudio, or may require multiple calls and priori knowledge to identify and access graphics states and values. These may be most usefully access through code and returned as data for re-use as in `par_measures()` and `par_print_summary()`. 


#### *+ Miscellaneous*

A number of other helper functions (e.g. `shape_direction()`), functions used for testing (e.g. `plot_test()`), and functions generating data used in other packages are also made available (e.g. `phyllotaxis_points()` used in the vocale R package (https://github.com/GregMci/vocale).




## installation:

```r
library( remotes )
remotes::install_github("GregMci/baselines")  
library( "baselines" )
```  



## notes:

* ***Device as a canvas*** - Commonly, a plot is the starting point for graphics in R. Rather than specifying where on a canvas the object appears, for the user, this approach specifies objects in relation to the plot. A canvas-first encourages consideration of multiple elements, rather than a single plot element, and easily enables annotations to be drawn above and around plots, even if they have intricate layouts.

* ***Canvas objects*** - In baselines, canvas objects have the `canvas_ ` prefix, e.g. `canvas_line()`, `canvas_rect()`. Because the current par settings are restored when the object is drawn to a canvas, the user can pop in and out of the current plot to draw at the canvas level. The canvas objects, currently available, become searchable as a group within R Studio.

* ***Blank Canvases*** - A blank canvas is called using `canvas_blank()`. The canvas is not a new device (which would have default par settings, Instead, the current device becomes a drawing area with coordinate ranges 0 < x < 1 & 0 < y < 1 (with x=0, y=0 as bottom left). This is simply a plot region with no margins, outer margins and no buffer to the plot region (i.e. `xaxs=”i"`, `yaxs=”i"`), and so the figure, plot and device regions are equivalent.

* ***Blank Plots*** - In contrast, `plot_blank()` produces a blank, empty plot given the current par settings (e.g. `mar`, `fig`, `oma`, etc…), and given the data supplied. Default data of x <- 0:1, y <- 0:1 are provided to support quick code sketches. The blank plot has no annotations, axes, labels, symbols or lines, allowing the user to add in the components they want. To create a blank plot themselves, a user would have to switch off a variety of arguments in the `plot()` function: e.g. `plot( 0:1, 0:1, type=”n” `, `axes=F`, `ann=F )`.

* ***Axis, Title*** - Similarly to `plot_blank()`, baselines provides direct access to components of `axis()` and `title()` functions in a simple format. For example, if a user wants to add extra labels to an axis, they would have to switch off the tick and line with the `axis()` function and use the `col.axis` and `cex.axis` arguments. In baselines, each axis component (`axis_labels()`, `axis_ticks()`, `axis_line()`) has the same formatting arguments for colour (`col`) and the standard arguments for that graphic type (e.g. `lwd` opposed to `lw.ticks`). The same framework applies to `text_main_title()`, `text_sub_title()`, `text_x_label()` & `text_y_label()`, which are aggregated in the original, single function of `title()`and so require distinct formatting arguments. Thus, `title(main=“TEXT”, col.main=”red”)` is equivalent to `title_main( “TEXT”, col =”red” )`. Because the base graphics approach to both axis and title derive settings form the exists plot (via `par()`) the baselines functions allow quick additions and alternatives using the same methods. 

* ***Defaults Draw*** - As per many point-click-drag software, selection of a new object produces that object given some default settings. So, for instance, `rect_fade()` will produce a gradient filled box without any arguments supplied.


## acknowledgments:

This work was initiated with funding from the Alan Turing Institute within the ‘Tools, Practices and Systems’ theme, under the EPSRC Cross-Discipline Interface Programme (grant number EP/I017909/1).
