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

//USFS Eco Map Subsections, can be used for subsetting FIA data.
var usfs = ee.FeatureCollection('projects/ca-ecs/USFS/EcomapSubsections');

//Select the Sierra Nevada subsections
var sierra = usfs.filter(ee.Filter.or(ee.Filter.eq('MAP_UNIT_S', 'M261Ep'),ee.Filter.eq('MAP_UNIT_S', 'M261Eq'), //South Sierra Nevada
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Es'),ee.Filter.eq('MAP_UNIT_S', 'M261Eu'), //South Sierra Nevada
                                      //Other Sierra
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Er'), 
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Eo')
                                      )); 

//North Sierra data
var north_sierra = usfs.filter(ee.Filter.or(ee.Filter.eq('MAP_UNIT_S', 'M261Ea'), ee.Filter.eq('MAP_UNIT_S', 'M261Eb'), //North Sierra
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Ec'), ee.Filter.eq('MAP_UNIT_S', 'M261Ed'), //North Sierra
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Ee'), ee.Filter.eq('MAP_UNIT_S', 'M261Ef'), //North Sierra
                                     ee.Filter.eq('MAP_UNIT_S', 'M261Eg'), ee.Filter.eq('MAP_UNIT_S', 'M261Eh'), //North Sierra
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Ei'), ee.Filter.eq('MAP_UNIT_S', 'M261Ej'), //North Sierra
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Ek'), ee.Filter.eq('MAP_UNIT_S', 'M261El'), //Added to Region
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Em'), //North Sierra
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Et')//, //Added
                                      )); 

//Add a 4-km Buffer
var add_buffer = function(f) {
  return f.buffer(2000,500);
};

var sev = ee.FeatureCollection('projects/ca-ecs/Fire/VegBurnSeverity');

//Add Burn severity data
var burn_sev = ee.FeatureCollection('projects/ca-ecs/Fire/VegBurnSeverity');

var perimeters = ee.FeatureCollection('projects/ca-ecs/Fire/veg_severity_perimeters');


var num = burn_sev.aggregate_count_distinct('VB_ID');
var fires = burn_sev.aggregate_array('VB_ID').distinct();

var fire_perim = perimeters.aggregate_array('VB_ID').distinct();
var num_list = ee.List.sequence(0, num.subtract(1), 1);
var fireDict = ee.Dictionary.fromLists(fires, num_list);

//Set the fire ID
var setID = function(f) {return f.set('fireID', fireDict.get(f.get('VB_ID'))) };

//Function to make FIRE_YEAR into a number
var numberParse = function(feature) {
   return feature.set('YEAR', ee.Number.parse(feature.get('FIRE_YEAR')));
};

//Set the fireIDs for the perimeters
var burn_sev_rename = burn_sev.map(setID);

//Define the region as California
var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));

//Convert fire year into a number
var burn_num = burn_sev_rename.map(numberParse, true); //Second argument is drop nulls

//Add buffer and convert to numbers
var burn_buffer_num = burn_sev_rename.map(add_buffer).map(numberParse, true);

//Fire perimeter export
var perimeters_buffer_num = perimeters.map(setID)
                                    .map(add_buffer)
                                    .map(numberParse, true);

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
    
    //Create a rasterized version of fire severity 
    var fireSev = fire.reduceToImage(['BURNSEV'],ee.Reducer.first()).rename('fire_sev');
    
       //Create a rasterized version of fire severity 
    var fireID = fire.reduceToImage(['fireID'],ee.Reducer.first()).rename('fire_ID');
    
    return fireYr.addBands(fireSev)
                 .addBands(fireID)
                     .set('system:time_start', startDate.millis()) //Set the date of the data
                     .reproject({crs: 'EPSG: 5070', scale: 30}); // Reproject the rasterized data
  }));
};

var bufferYrCollection = function(features, start, count, interval) {
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
    
       //Create a rasterized version of fire severity 
    var fireID = fire.reduceToImage(['fireID'],ee.Reducer.first()).rename('fire_ID');
    
    return fireYr
                 .addBands(fireID)
                     .set('system:time_start', startDate.millis()) //Set the date of the data
                     .reproject({crs: 'EPSG: 5070', scale: 30}); // Reproject the rasterized data
  }));
};

//Variables to include in the function
var features = burn_num; 
var startDate = 1985;
var count = 33;
var interval = 1; 

//Create the fire severity image collection
var fireCollection = fireYrCollection(features, startDate, count, interval);

//Reduce the fire year and severity Image Collection into an Image
var fire_year = fireCollection.reduce(ee.Reducer.lastNonNull());

//Create fire severity, 4-km buffer collection
var unchangedBufferCollection = fireYrCollection(burn_buffer_num.filter(ee.Filter.eq({name: 'BURNSEV', value: 1})), startDate, count, interval);
var lowBufferCollection = fireYrCollection(burn_buffer_num.filter(ee.Filter.eq({name: 'BURNSEV', value: 2})), startDate, count, interval);
var medBufferCollection = fireYrCollection(burn_buffer_num.filter(ee.Filter.eq({name: 'BURNSEV', value: 3})), startDate, count, interval);
var highBufferCollection = fireYrCollection(burn_buffer_num.filter(ee.Filter.eq({name: 'BURNSEV', value: 4})), startDate, count, interval);

var BufferCollection = bufferYrCollection(perimeters_buffer_num, startDate, count, interval);

//2020 FRAP Data
var fireImage2020 = fireCollection.reduce(ee.Reducer.lastNonNull()).toInt16();
var fireImageCount2020 = fireCollection.select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//2010 FRAP Data
var fireImage2019 = fireCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2019-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var fireImageCount2019 = fireCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2019-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();


//2010 FRAP Data
var fireImage2010 = fireCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var fireImageCount2010 = fireCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2010-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();
Map.addLayer(fireImage2010, {}, 'Create Fire Image');

//2000 FRAP Data
var fireImage2000 = fireCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2000-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var fireImageCount2000 = fireCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2000-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//1990 FRAP Data
var fireImage1990 = fireCollection.filter(ee.Filter.date({start:'1985-01-01', end:'1990-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var fireImageCount1990 = fireCollection.filter(ee.Filter.date({start:'1985-01-01', end:'1990-12-31'})).select('fire_year').reduce(ee.Reducer.countDistinctNonNull()).toInt16();

//Load the severity 2010 image
var sev2010 = ee.Image('users/cnorlen/Fire_Dieoff/fire_sev_year_2010');

//2010 Fire Severity Buffer
var unchangedBuffer2010 = unchangedBufferCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var lowBuffer2010 = lowBufferCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var medBuffer2010 = medBufferCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();
var highBuffer2010 = highBufferCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();

//Add the 2010 buffer collection
var Buffer2010 = BufferCollection.filter(ee.Filter.date({start:'1985-01-01', end:'2010-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();

//Export Rasterized FRAP to Assets
//Export FRAP fire year images
Export.image.toAsset({
  image: fireImage2020,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'fire_sev_year_2020',
  assetId: 'users/cnorlen/Fire_Dieoff/fire_sev_year_2020_v3'
});

Export.image.toAsset({
  image: fireImage2019,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'fire_sev_year_2019',
  assetId: 'users/cnorlen/Fire_Dieoff/fire_sev_year_2019_v3'
});

Export.image.toAsset({
  image: fireImage2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'fire_sev_year_2010',
  assetId: 'users/cnorlen/Fire_Dieoff/fire_sev_year_2010_v3'
});

//Fire Severity Buffer layers
Export.image.toAsset({
  image: unchangedBuffer2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'unchanged_buffer_year_2010_2km',
  assetId: 'users/cnorlen/Fire_Dieoff/unchanged_buffer_year_2010_2km_v2'
});

Export.image.toAsset({
  image: lowBuffer2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'low_buffer_year_2010_2km',
  assetId: 'users/cnorlen/Fire_Dieoff/low_buffer_year_2010_2km_v2'
});

Export.image.toAsset({
  image: medBuffer2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'med_buffer_year_2010_2km',
  assetId: 'users/cnorlen/Fire_Dieoff/med_buffer_year_2010_2km_v2'
});

Export.image.toAsset({
  image: highBuffer2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'high_buffer_year_2010_2km',
  assetId: 'users/cnorlen/Fire_Dieoff/high_buffer_year_2010_2km_v2'
});

Export.image.toAsset({
  image: Buffer2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'buffer_sev_year_2010_2km',
  assetId: 'users/cnorlen/Fire_Dieoff/buffer_sev_year_2010_2km'
});

//Fire Count Images
Export.image.toAsset({
  image: fireImageCount2020,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'fire_sev_count_2020',
  assetId: 'users/cnorlen/Fire_Dieoff/fire_sev_count_2020_v3'
});

Export.image.toAsset({
  image: fireImageCount2019,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'fire_sev_count_2019',
  assetId: 'users/cnorlen/Fire_Dieoff/fire_sev_count_2019_v3'
});

Export.image.toAsset({
  image: fireImageCount2010,
  scale: 30,
  crs: 'EPSG: 5070', 
  region: region,
  maxPixels: 1e13,
  description: 'fire_sev_count_2010',
  assetId: 'users/cnorlen/Fire_Dieoff/fire_sev_count_2010_v3'
});

//USFS Fire Severity Fire Year
Export.image.toDrive({
  image: ee.Image('users/cnorlen/Fire_Dieoff/fire_sev_year_2010_v2'),
  scale: 300,
  description: 'USFS_fire_severity_2010_300m',
  fileFormat: 'GeoTiff',
  maxPixels: 1e12,
  crs: 'EPSG: 5070',
  region: region,
  folder: 'Fire_Dieoff'
});

//USFS Fire Severity Fire Year Buffer
Export.image.toDrive({
  image: ee.Image('users/cnorlen/Fire_Dieoff/buffer_sev_year_2010_2km'),
  scale: 300,
  description: 'USFS_fire_severity_buffer_2010_300m',
  fileFormat: 'GeoTiff',
  maxPixels: 1e12,
  crs: 'EPSG: 5070',
  region: region,
  folder: 'Fire_Dieoff'
});
