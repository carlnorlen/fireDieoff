/* 
///////////////////// NDVI-ET SCALING /////////////////////

Landsat 5,7,8
CECS Project
KSH, Carl Norlen
Goal: Produce scaled ET monthly and annual image stack
  
*/

///////////////////// CALL FUNCTIONS /////////////////////

// Our functions
var composite = require('users/khemes/Hemesetal_2023_PNAS:functions/composite');
var Join = require('users/khemes/Hemesetal_2023_PNAS:functions/Join');
var addYear = require('users/khemes/Hemesetal_2023_PNAS:functions/addYear');
var download = require('users/khemes/Hemesetal_2023_PNAS:functions/download');
var climate = require('users/khemes/Hemesetal_2023_PNAS:functions/climate');
var landsat = require('users/khemes/Hemesetal_2023_PNAS:functions/landsat');
var veg_ind_calc = require('users/khemes/Hemesetal_2023_PNAS:functions/veg_ind_calc');
var download = require('users/khemes/Hemesetal_2023_PNAS:functions/download');

//Batch export functions form, GEE Tools package create by fitoprincipe GEE user.
var batch = require('users/fitoprincipe/geetools:batch');

///////////////////// ADD GEOMETRY /////////////////////

//Add the UCI Flux Tower Locations
var ameriflux_siteLocations = ee.FeatureCollection('projects/ca-ecs/Flux_sites/CA_fluxsites');
var UCI_upwind = ee.FeatureCollection('projects/ca-ecs/Flux_sites/UCI_LSpixellocations_siteID_2');

//Add the California Perimeter
var region = ee.FeatureCollection('TIGER/2018/States')
              .filter(ee.Filter.eq('NAME', 'California'));
var add_buffer = function(f) {
  return f.buffer(50000,2000);
};
var region_buffer = region.map(add_buffer);

///////////////////// GET NDVI /////////////////////

// Specify the start and end dates
var startDate = '1984-05-01';
var endDate   = '2018-12-31';
var reducer = ee.Reducer.mean();
var parallel = 16;
           
//Add merged Landsat 5, 7, and 8 SR data
var all_landsat = landsat.L578(startDate,endDate,region);
      // monthly composite
var byMonth_NDVI = composite.temporalReduce(all_landsat.select('NDVI'), reducer, '1984-05-01', '1984-05-31', 416, 1, 'month', parallel);
      // gap fill
var byMonth_NDVI_filled = composite.gapFillcollect(byMonth_NDVI, ['NDVI']).map(addYear.addYYYYMM);

///////////////////// ADD MET /////////////////////

// Temp
var temp_m = climate.temp_30m_m;
//print(temp_m,'temp_m')
var joinField = 'system:time_start';
var maxDiff = 15 * 24 * 60 * 60 * 1000; // 15 days in milliseconds
var NDVI_monthly_met = Join.bestJoin(byMonth_NDVI_filled, temp_m, joinField, maxDiff);

// Precip
var precip_m = climate.precip_30m_m;
//print(precip_m,'precip_m')
var joinField = 'system:time_start';
var maxDiff = 15 * 24 * 60 * 60 * 1000;
NDVI_monthly_met = Join.bestJoin(NDVI_monthly_met, precip_m, joinField, maxDiff);

// TOA Rad
var rad_m = climate.srad_30m_m;
//print(rad_m,'rad_m')
var joinField = 'system:time_start';
var maxDiff = 15 * 24 * 60 * 60 * 1000;
NDVI_monthly_met = Join.bestJoin(NDVI_monthly_met, rad_m, joinField, maxDiff);



///////////////////// EXPORT FOR UCI SITE SCALING /////////////////////


var byPixel = function(image) { // function for each annual image to extract each pixel

var sampleRegionImages = image.unmask().sampleRegions(
  {collection: UCI_upwind,
   scale: 30,
   properties:['Pixel #','Site'],
   tileScale: 8
}) ;

var mapped = sampleRegionImages.map(function(ftr) { // map over each feature and set year (why doesnt this work?)
  return ftr.set('year_month',image.get('year_month'));
});

return mapped;
};

var byPixel_NDVI_monthly_met = NDVI_monthly_met.map(byPixel).flatten();

  // export
Export.table.toDrive({
  'collection': byPixel_NDVI_monthly_met,
  'description': 'UCIupwind_pixels_NDVI_met_30m',//'spatcon_annual_L578_median_500m',
  'folder': 'CECS', //'earthEngine_outputs',
  'fileFormat': 'CSV'
});