//Created by: Carl Norlen
//Created Date: 04/16/2020
//Updated by: Carl Norlen
//Updated date: 04/16/2020

/**Functions to To get some Landsat data**/

//Add functions for creating masks
var mask = require('users/cnorlen/subsequent_drought/functions:mask');

//Add functions for calculating veg indices, and merging data L7 and L8
var veg_indices = require('users/cnorlen/subsequent_drought/functions:veg_indices');

//Get Image Collections for Landsat 5,7, and 7 data fliter. 
//Load Image Collections for Landsat 5, 7, and 8. 
//Do fmask cloud filtering.
exports.L578 = function(startDate, endDate, region) {

  var dataset_LT05 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
                  .filterDate(startDate, '2012-04-30')
                  .filter(ee.Filter.bounds(region))
                  .map(mask.cloudMaskL457)
                  .select('B1','B2','B3','B4','B5','B6','B7');

  var dataset_LE07 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
                  .filterDate('2000-01-01', endDate)
                  .filter(ee.Filter.bounds(region))
                  .map(mask.cloudMaskL457)
                  .select('B1','B2','B3','B4','B5','B6','B7');

  var dataset_LE08 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
                  .filterDate('2013-05-01', endDate)
                  .filter(ee.Filter.bounds(region))
                  .map(mask.maskL8sr)
                  .map(veg_indices.harmonizationRoy)
                  .select('B1','B2','B3','B4','B5','B6','B7');

  var L578merge = dataset_LT05.merge(dataset_LE07).merge(dataset_LE08);

  return L578merge;
};