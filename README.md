# Land use land cover classification of Suli County

## Data sources 

**Vegetation map** was downloaded from [An updated Vegetation Map of China (1:1000000)](https://www.sciencedirect.com/science/article/pii/S2095927320302152), and was cropped to the extent of study area.
 
 **DEM** was obtained from open source [ALOS PALSAR data](https://search.asf.alaska.edu). Spatial resolution is 12.5 m, search date range is from 2007-1-1 to 2007-2-28.
 
 **Sentinel-2 satellite images** was pre-processed on Google Earth Engine with self written [code](src/NDVI-trend.ipynb).
 
 ## Data processing
 
 ### Streamline extraction
 
 I extracted streamline of Suli County following procedures provided by [ESRI hydrology tools](https://desktop.arcgis.com/zh-cn/arcmap/latest/tools/spatial-analyst-toolbox/an-overview-of-the-hydrology-tools.html). 
 
 1. Fill the sinks in above mentioned DEM products with default parameters (without specifying z value). Generally, there won't be many sinks in DEM product with ~10 m resolution. 
 
 2. Calculate flow direction, values(1,2,4,8,16,32,64,128) indicating direction.
 
 3. Calculate flow flux, values indicating how many pixels lead to this pixel(?).
 
 4. Decide a flow threshold. Generally the threshold value is the flux value of the origin of the minimal stream of interest.
 
 5. Convert the flow flux raster to binary raster based on flow threshold.
 
 6. Use stream link function to assign unique values to sections of a raster linear network between intersections in case later stream order calculation.
 
 7. Vectorize stream link raster.
 
 ### Field data collection
 
 See [Field-data-collection_Plan.pdf](Manual/Field-data-collection_Plan.pdf).
 
 ## Supervised classification
 

