Plot of some series
========================================================

Package nécessaire :

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
require(reshape)
```

```
## Loading required package: reshape
```

Chargement des données :

```r
load("vardata.R")
```



```r
varm <- melt(var[4:179,],  id = 'time', variable_name = 'series')
```

```
## Error in var[4:179, ]: objet de type 'closure' non indiçable
```

```r
ggplot(complete$YWR, aes(time,value)) + geom_line() + facet_grid(series ~ .)
```

```
## Error in ggplot(complete$YWR, aes(time, value)): objet 'complete' introuvable
```



