# Dry Beans Dataset

A subsample of the Koklu & Ozkan (2020) dry beans dataset produced by
imaging a total of 13,611 grains from 7 varieties of dry beans. The
original dataset contains 13,611 observations, but here we include a
random subsample of 1000.

## Usage

``` r
minibeans
```

## Format

### `minibeans`

A data frame with 1000 rows and 17 columns:

- Area:

  The area of a bean zone and the number of pixels within its
  boundaries.

- Perimeter:

  Bean circumference is defined as the length of its border.

- Major axis length:

  The distance between the ends of the longest line that can be drawn
  from a bean.

- Minor axis length:

  The longest line that can be drawn from the bean while standing
  perpendicular to the main axis.

- Aspect ratio:

  Defines the relationship between L and l.

- Eccentricity:

  Eccentricity of the ellipse having the same moments as the region.

- Convex area:

  Number of pixels in the smallest convex polygon that can contain the
  area of a bean seed.

- Equivalent diameter:

  The diameter of a circle having the same area as a bean seed area.

- Extent:

  The ratio of the pixels in the bounding box to the bean area.

- Solidity:

  Also known as convexity. The ratio of the pixels in the convex shell
  to those found in beans.

- Roundness:

  Calculated with the following formula: (4piA)/(P^2).

- Compactness:

  Measures the roundness of an object: Ed/L.

- ShapeFactor1:

  Shape factor 1.

- ShapeFactor2:

  Shape factor 2.

- ShapeFactor3:

  Shape factor 3.

- ShapeFactor4:

  Shape factor 4.

- Class:

  Seker, Barbunya, Bombay, Cali, Dermosan, Horoz, and Sira.

## Source

Koklu, M, and IA Ozkan. 2020. Multiclass Classification of Dry Beans
Using Computer Vision and Machine Learning Techniques. Computers and
Electronics in Agriculture, 174: 105507. doi:
10.1016/j.compag.2020.105507, https://doi.org/10.24432/C50S4B
