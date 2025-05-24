///////////////////// CALL FUNCTIONS /////////////////////

// Functions
var landsat = require('users/cnorlen/fireDieoff/functions:landsat');
var veg_ind_calc = require('users/cnorlen/subsequent_drought/functions:veg_indices');
var composite = require('users/cnorlen/subsequent_drought/functions:composites');
var download = require('users/cnorlen/subsequent_drought/functions:download');
var join = require('users/cnorlen/subsequent_drought/functions:join');
var climate = require('users/cnorlen/subsequent_drought/functions:climate');
var resolution = require('users/cnorlen/subsequent_drought/functions:resolution');
var frap = require('users/cnorlen/fireDieoff/functions:frap');
var mask = require('users/cnorlen/subsequent_drought/functions:mask');
var ads = require('users/cnorlen/fireDieoff/functions:ads');

//Batch export functions form, GEE Tools package create by fitoprincipe GEE user.
var batch = require('users/fitoprincipe/geetools:batch');
var palettes = require('users/gena/packages:palettes');

var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

//Add a 4-km Buffer
var add_buffer = function(f) {
  return f.buffer(2000,500);
};
var region_buffer = region.map(add_buffer);

//California Perimeter
var ca = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

  // CCDC undistrubed
var undist = ee.Image('projects/ca-ecs/ca_undisturbed_nofrap');

//Add a Sierra Polygon
var full_sierra = ee.Geometry.Rectangle(-122.5, 38, -117, 32.2); //North limit was 40.9 

//USFS Eco Map Subsections, can be used for subsetting FIA data.
var usfs = ee.FeatureCollection('projects/ca-ecs/USFS/EcomapSubsections');

//Make year and fire_name into a fire_ID
var addRxid = function(feature) {
                        return feature.set('fireID', ee.String(feature.get('YEAR_')).cat(ee.String(feature.get('AGENCY'))).cat(ee.String(feature.get('TREATMENT1'))));
};

//Add the FRAP Polygons to the Script
//Added a filter to the FRAP Layer
var rx_frap = ee.FeatureCollection('projects/ca-ecs/FRAP/rxburn_20_1').filter(ee.Filter.and(ee.Filter.gte('Shape_Area', 450000),
                                                                              ee.Filter.neq('YEAR_','')));

//Make year and fire_name into a fire_ID
var addFRAPid = function(feature) {
                        return feature.set('fireID', ee.String(feature.get('YEAR_')).cat(ee.String(feature.get('FIRE_NAME'))));
};

var wild_frap = ee.FeatureCollection('projects/ca-ecs/FRAP/firep20_1').filter(ee.Filter.neq('YEAR_',''));

//Function to make FIRE_YEAR into a number
var numberParse = function(feature) {
   return feature.set('YEAR', ee.Number.parse(feature.get('YEAR_')));
};

//Convert fire year into a number
var rx_num = rx_frap.map(numberParse, true); //Second argument is drop nulls
Map.addLayer(rx_num, {}, 'Unfiltered RX layer');
var wild_num = wild_frap.map(numberParse, true);


//Create buffers
var wild_buffer_num = wild_frap.map(add_buffer).map(numberParse, true);

var rx_buffer_num = rx_frap.map(add_buffer).map(numberParse, true); //Second argument is drop nulls

//Function to create a fire image collection
var fireYrCollection = function(features, start, count, interval) {
  // Create a sequence of numbers, one for each time interval.
  var sequence = ee.List.sequence(0, ee.Number(count).subtract(1));

  return ee.ImageCollection(sequence.map(function(i) { 
    // Calculate the year in the sequence
    var startYear = ee.Number(start).add(i);
    
    //Convert the year into a date
    var startDate = ee.Date(ee.String(startYear.int()));
    
    //Select the feature for each Fire year
    var fire = features.filter(ee.Filter.eq('YEAR', startYear));
    
    //Create a rasterized version of fire year
    var fireYr = fire.reduceToImage(['YEAR'],ee.Reducer.first()).rename('fire_year');
    
    //Create a rasterized version of fire type
    var fireType = fire.reduceToImage(['YEAR'],ee.Reducer.first()).gte(1).rename('fire_type');

    
    return fireYr.addBands(fireType)
                     .set('system:time_start', startDate.millis()) //Set the date of the data
                     .reproject({crs: 'EPSG: 5070', scale: 30}); // Reproject the rasterized data
  }));
};

//Variables to include in the function
var startDate = 1910;
var count = 111; //111 does until 2020
var interval = 1; 

//Create the Rx FRAP image collection
var rxCollection = fireYrCollection(rx_num, startDate, count, interval)
                                    .map(function(img) {var type = img.select('fire_type').multiply(2);
                                                                                    return img.select('fire_year').addBands(type); //Change the fire type to equal 2
                                                        });

//Create a fire buffer layer
var rxBufferCollection = fireYrCollection(rx_buffer_num, startDate, count, interval)
                                    .map(function(img) {var type = img.select('fire_type').multiply(2);
                                                                                    return img.select('fire_year').addBands(type); //Change the fire type to equal 2
                                                        });

var wildBufferCollection = fireYrCollection(wild_buffer_num, startDate, count, interval);

//Create the Wildfire FRAP image collection
var wildCollection = fireYrCollection(wild_num, startDate, count, interval);

var frapCollection = rxCollection.merge(wildCollection);
//Map.addLayer(frapCollection, {}, 'Rx Fire Collection');

var frapBufferCollection = rxBufferCollection.merge(wildBufferCollection);

//2020 FRAP Data
var frapImage2020 = frapCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2020-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var frapImageCount2020 = frapCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2020-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//2019 FRAP Data
var frapImage2019 = frapCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2019-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var frapImageCount2019 = frapCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2019-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//2010 FRAP Data
var frapImage2010 = frapCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var frapImageCount2010 = frapCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2010-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//2000 FRAP Data
var rxImage2000 = frapCollection.filter(ee.Filter.date({start:'1921-01-01', end:'2000-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var frapImageCount2000 = frapCollection.filter(ee.Filter.date({start:'1921-01-01', end:'2000-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//1990 FRAP Data
var frapImage1990 = frapCollection.filter(ee.Filter.date({start:'1921-01-01', end:'1990-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var frapImageCount1990 = frapCollection.filter(ee.Filter.date({start:'1921-01-01', end:'1990-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//RxData
var rxImage2020 = rxCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2020-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var rxImageCount2020 = rxCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2020-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//2019 FRAP Data
var rxImage2019 = rxCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2019-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var rxImageCount2019 = rxCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2019-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//2010 FRAP Data
var rxImage2010 = rxCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var rxImageCount2010 = rxCollection.filter(ee.Filter.date({start:'1981-01-01', end:'2010-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

var frapBufferImage2010 = wildBufferCollection.filter(ee.Filter.date({start:'1921-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();

//2010 Rx Buffer Data
var rxBufferImage2010 = rxBufferCollection.filter(ee.Filter.date({start:'1921-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();

//
var frapImageList = frapCollection.map(function(image) {return image.unmask({value: -9999})}).toArrayPerBand();

//FRAP Year Exports
Export.image.toAsset({
  image: frapImage2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'frap_rx_year_2010_v3',
  assetId: 'users/cnorlen/Fire_Dieoff/frap_rx_year_2010_v3'
});

Export.image.toAsset({
  image: frapImage2019,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'frap_rx_year_2019_v3',
  assetId: 'users/cnorlen/Fire_Dieoff/frap_rx_year_2019_v3'
});

Export.image.toAsset({
  image: frapImage2020,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'frap_rx_year_2020_v3',
  assetId: 'users/cnorlen/Fire_Dieoff/frap_rx_year_2020_v3'
});

//FRAP Count Exports
Export.image.toAsset({
  image: frapImageCount2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'frap_rx_count_2010_v3',
  assetId: 'users/cnorlen/Fire_Dieoff/frap_rx_count_2010_v3'
});

Export.image.toAsset({
  image: frapImageCount2019,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'frap_rx_count_2019_v3',
  assetId: 'users/cnorlen/Fire_Dieoff/frap_rx_count_2019_v3'
});

Export.image.toAsset({
  image: frapImageCount2020,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'frap_rx_count_2020_v3',
  assetId: 'users/cnorlen/Fire_Dieoff/frap_rx_count_2020_v3'
});

//Rx Exports
Export.image.toAsset({
  image: rxImage2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'rx_year_2010',
  assetId: 'users/cnorlen/Fire_Dieoff/rx_year_2010'
});

Export.image.toAsset({
  image: rxImage2019,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'rx_year_2019',
  assetId: 'users/cnorlen/Fire_Dieoff/rx_year_2019'
});

Export.image.toAsset({
  image: rxImage2020,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'rx_year_2020',
  assetId: 'users/cnorlen/Fire_Dieoff/rx_year_2020'
});

//Rx Count Exports
Export.image.toAsset({
  image: rxImageCount2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'rx_count_2010',
  assetId: 'users/cnorlen/Fire_Dieoff/rx_count_2010'
});

Export.image.toAsset({
  image: rxImageCount2019,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'rx_count_2019',
  assetId: 'users/cnorlen/Fire_Dieoff/rx_count_2019'
});

Export.image.toAsset({
  image: rxImageCount2020,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'rx_count_2020',
  assetId: 'users/cnorlen/Fire_Dieoff/rx_count_2020'
});

//Fire Buffer Exports
Export.image.toAsset({
  image: frapBufferImage2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'buffer_frap_year_2010_2km',
  assetId: 'users/cnorlen/Fire_Dieoff/buffer_frap_year_2010_2km'
});

Export.image.toAsset({
  image: rxBufferImage2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: ca,
  maxPixels: 1e13,
  description: 'buffer_rx_year_2010_2km_v3',
  assetId: 'users/cnorlen/Fire_Dieoff/buffer_rx_year_2010_2km_v3'
});

//FRAP Wildfire Year
Export.image.toDrive({
  image: frapImage2010,
  scale: 300,
  description: 'FRAP_wildfire_2010_300m_v2',
  fileFormat: 'GeoTiff',
  maxPixels: 1e12,
  crs: 'EPSG: 5070',
  region: region,
  folder: 'Fire_Dieoff'
});

//FRAP Wildfire Buffer Year Buffer
Export.image.toDrive({
  image: frapBufferImage2010,
  scale: 300,
  description: 'FRAP_wildfire_buffer_2010_300m_v2',
  fileFormat: 'GeoTiff',
  maxPixels: 1e12,
  crs: 'EPSG: 5070',
  region: region,
  folder: 'Fire_Dieoff'
});

//FRAP Prescribed Fire Year Buffer
Export.image.toDrive({
  image: rxImage2010,
  scale: 300,
  description: 'FRAP_Rxfire_2010_300m_v2',
  fileFormat: 'GeoTiff',
  maxPixels: 1e12,
  crs: 'EPSG: 5070',
  region: region,
  folder: 'Fire_Dieoff'
});

//FRAP Prescribed Fire Year Buffer
Export.image.toDrive({
  image: rxBufferImage2010,
  scale: 300,
  description: 'FRAP_Rxfire_buffer_2010_300m_v2',
  fileFormat: 'GeoTiff',
  maxPixels: 1e12,
  crs: 'EPSG: 5070',
  region: region,
  folder: 'Fire_Dieoff'
});