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
var ads = require('users/cnorlen/Scripts:ads');

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
Map.addLayer(undist, {}, 'Undisturbed layer');

var full_sierra = ee.Geometry.Rectangle(-122.5, 38, -117, 32.2); //North limit was 40.9 
//Map.addLayer(full_sierra, {}, 'Full Sierra');

//USFS Eco Map Subsections, can be used for subsetting FIA data.
var usfs = ee.FeatureCollection('projects/ca-ecs/USFS/EcomapSubsections');

//Add the FRAP Polygons to the Script
var rx_frap = ee.FeatureCollection('projects/ca-ecs/FRAP/rxburn_20_1').filter(ee.Filter.neq('YEAR_',''));
//Map.addLayer(rx_frap, {}, 'Rx FRAP');
//print(rx_frap.aggregate_array('YEAR_'), 'FRAP Rx Fire Years');

var wild_frap = ee.FeatureCollection('projects/ca-ecs/FRAP/firep20_1').filter(ee.Filter.neq('YEAR_',''));
//print(wild_frap.aggregate_array('YEAR_'), 'FRAP Wild Fire Years');

//Function to make FIRE_YEAR into a number
var numberParse = function(feature) {
   return feature.set('YEAR', ee.Number.parse(feature.get('YEAR_')));
};

//Convert fire year into a number
var rx_num = rx_frap.map(numberParse, true); //Second argument is drop nulls

var wild_num = wild_frap.map(numberParse, true);
//Map.addLayer(burn_num, {}, "New Num Column");

//Create buffers
var wild_buffer_num = wild_frap.map(add_buffer).map(numberParse, true);

var rx_buffer_num = rx_frap.map(add_buffer).map(numberParse, true); //Second argument is drop nulls


//Select the Sierra Nevada subsections
var sierra = usfs.filter(ee.Filter.or(
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Ep'),
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Eq'), //South Sierra Nevada
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Es'),
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Eu'), //South Sierra Nevada
                                    //Other Sierra
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Er'), 
                                      ee.Filter.eq('MAP_UNIT_S', 'M261Eo')
                                      )
                                      ); 

  ///Create two sections : 1.) with forest drought (2012-2015) response (NDMI) with Years since fire (Fires are set to 2010), 
                    //     2.) forest without drought (2001-2004) with Years since Fire (Fires are set to 1998)

var reproject = function(image) {
                 return image.reproject({crs: 'EPSG: 5070', scale: 30});
};
                                                      
 // NDMI
var byYear = ee.ImageCollection('projects/ca-ecs/L578_Composites/byYear_NDMI_v2').map(join.addYear).select('NDMI');
//print(byYear_VIs_LST);

///////////////////// INTEGRATE ATTRIBUTE/PREDICTOR LAYERS /////////////////////

    /// Land Cover
var emapr_lc = ee.Image('projects/ca-ecs/eMapR/eMapR_land_cover_CA_ARD_all');
//Function to simplify landcover bands into nine categories.
var remap_lc = function(image) {
                                var landcover = image.remap([31,21,22,23,24,41,42,43,51,52,71,72,73,74,81,82,90,95,11,12],
                                   [1,2,2,2,2,3,4,5,6,6,7,7,7,7,8,8,9,9,10,10]); // 1: NoVeg (water/ice/barren); 2: Urban; 3: Decid F, 4: Everg F, 5: Mixed F
                                return landcover; // 6: Shrub; 7: Herbaceous; 8: Crop; 9: Wetland
};
var emapr_lc_annual = join.unStack(emapr_lc, '1990-06-01', '1990-09-30', 'landcover').map(join.addYear); // remapped version doesnt work here
var emapr_lc_annual_remap = emapr_lc_annual.map(remap_lc);
var rename_band = function(img){
  return img.rename('emaprlc');
};

      ///eMapr Land Cover
emapr_lc_annual_remap = emapr_lc_annual_remap.map(rename_band);


      ///eMapR Biomass
var emapr_bm = ee.Image('projects/ca-ecs/eMapR/eMapR_biomass_CA_ARD_all');
var emapr_bm_annual = join.unStack(emapr_bm, '1984-06-01', '1984-09-30', 'biomass').map(join.addYear);
var rename_band = function(img){
  return img.rename('emapr_biomass');
};
emapr_bm_annual = emapr_bm_annual.map(rename_band);

  
    /// LEMMA Biomass
var lemma_bm = ee.ImageCollection('projects/ca-ecs/LEMMA/Biomass1986_2017').map(join.addYear); // units of kg/ha
var rename_band = function(img){ // lemma non-forest indicated in raw data as -1
  return img.updateMask(img.gte(0)).rename('lemma_biomass'); //unmask(ee.Image(0))
};
lemma_bm = lemma_bm.map(rename_band);


   /// Bring in water year NDVI and apply addET Goulden et al scaling function
var ETwy = ee.ImageCollection('projects/ca-ecs/L578_Composites/byYear_NDVI_v2') // water year NDVI
      .map(veg_ind_calc.addET) // from Goulden et al
      .select('ET')
      .map(join.addEndYear);



//Function to mask CASA data
var maskCASA = function(img) {
                              var mask = img.neq(-9999);
                              return img.updateMask(mask);
};

//Add ADS Data
var adsTS = ads.TS;//.map(maskCASA);

//Function that parses CASA image names and turns them into start and end dates
var addDate = function(image) { //Get the length of the image name string
                                var num = ee.Number(ee.String(image.id()).length()); //Get the Image ID
                                //Get the image name and convert the year at the end into a start date
                                var dateStart = ee.Date(ee.Number(ee.String(image.id()).slice(-4,num))).advance(5, 'months');
                                //Get the image name and convert the year at the end into an end date
                                var dateEnd = ee.Date(ee.Number(ee.String(image.id()).slice(-4,num))).advance(7, 'months');
                                //Return the image with the dates added
                                return image.set('system:time_start', dateStart.millis())
                                            .set('system:time_end', dateEnd.millis());
};

//Annual GPP image Collection
var GPP = ee.ImageCollection('projects/ca-ecs/CASA/CFlux_GPP').map(maskCASA)
                                                                  .map(addDate)
                                                                  .map(join.addYear)
                                                                  .select([0],['GPP']);

//Water year ET image collection
var AET = ee.ImageCollection('projects/ca-ecs/CASA/WaterFlux_AET_v2').map(maskCASA)
                                                                  .map(addDate)
                                                                  .map(join.addYear)
                                                                  .select([0],['AET']);

//Water year Soil moisture image collection
var SoilMoisture = ee.ImageCollection('projects/ca-ecs/CASA/WaterFlux_Soilmoisture').map(maskCASA)
                                                                                    .map(addDate)
                                                                                    .map(join.addYear)
                                                                                    .select([0],['Soil_Moisture']);

//Water year Soil moisture image collection
var Stress = ee.ImageCollection('projects/ca-ecs/CASA/WaterFlux_Stress_v2').map(maskCASA)
                                                                        .map(addDate)
                                                                        .map(join.addYear)
                                                                        .select([0],['Water_Stress']);


//Shrub cover data set
var shrub = ee.ImageCollection('projects/ca-ecs/fractionalCover/VegType_shrub_v2').map(maskCASA)
                                                                               .map(addDate)
                                                                               .map(join.addYear)
                                                                               .select([0],['Shrub_Cover']);
   
//Bare cover data set                                                                               
var bare = ee.ImageCollection('projects/ca-ecs/fractionalCover/VegType_bare').map(maskCASA)
                                                                               .map(addDate)
                                                                               .map(join.addYear)
                                                                               .select([0],['Bare_Cover']);                                                                               

//Herb cover data set                                                                               
var herb = ee.ImageCollection('projects/ca-ecs/fractionalCover/VegType_herb').map(maskCASA)
                                                                               .map(addDate)
                                                                               .map(join.addYear)
                                                                               .select([0],['Herb_Cover']);  
                                                                               
//Tree cover data set                                                                               
var tree = ee.ImageCollection('projects/ca-ecs/fractionalCover/VegType_tree_v2').map(maskCASA)
                                                                               .map(addDate)
                                                                               .map(join.addYear)
                                                                               .select([0],['Tree_Cover']);  

//Add the Montana Vegetation data sets
var all_veg = ee.ImageCollection("projects/rangeland-analysis-platform/vegetation-cover-v3")
                .map(join.addYear);


//SPI48 Data
var spi48 = ee.ImageCollection('users/cnorlen/California_Data/SPI48_9').map(resolution.down_30m) // //Downscale by bilinear interpolation
                                                                       .map(join.addEndYear) //function(img) { return img.set('year', ee.Date(img.get('system:time_end')).get('year')); })
                                                                       .select([0],['SPI48']);
//print(spi48);

//Annual Precip and Temp
//Add the temperature time series and add a year property for joining
var temp = climate.tempTS.map(join.addEndYear);

//Add the precip time series and add a year property for joining
var precip = climate.precipTS.map(join.addEndYear);

///////////////////// Gap Fill and JOIN /////////////////////

var addFireYear = function(image) {
                  return image.addBands(frapImage2020); //Add years since fire to the Image
};

var addFireYearList = function(image) {
                      return image.addBands(frapImageList);
};

var addLatLong = function(image) {
                 return image.addBands(ee.Image.pixelLonLat()); //Add Lat Long to the image
};

var lf_evt_2001 = ee.ImageCollection('users/cnorlen/California_Data/LANDFIRE/EV_Type')
                                  .filter(ee.Filter.date('2001-01-01')).max()
                                  .rename(['lf_evt_2001']);

var byYear_filled = ee.ImageCollection(byYear)  //ee.ImageCollection(byYear.map(composite.gapFill) // need to ensure composite.gapfill has the right # of bands
                         .map(function(image) {return image.addBands(lf_evt_2001) })
                         .map(veg_ind_calc.addElev) // slope, aspect, elevation, time invariant
                         .map(climate.addPRISMnormals) // climate normals, time invariant
                         //.map(addFireYear) //Years Since Fire (in 2008), time invariant
                         //.add(addFireYearList) //Add arrays of fire years and fire types //There is a problem with this
                         .map(addLatLong); //Add Lat Long to the Image

//Combine the different data layers together
var joinField = 'system:time_start';
var maxDiff = 7 * 365 * 24 * 60 * 60 * 1000; // 7 yrs difference max
byYear_filled = join.bestJoin(byYear_filled, emapr_bm_annual, joinField, maxDiff); // Join with eMapr into property   
byYear_filled = join.bestJoin(byYear_filled, emapr_lc_annual_remap, joinField, maxDiff).map(join.addYear);
byYear_filled = join.bestJoin(byYear_filled, lemma_bm, joinField, maxDiff);
byYear_filled = join.simpleJoin(byYear_filled, ETwy, 'year'); //join by wy end year

//Add the ADS Time Series to the image stack
byYear_filled = join.simpleJoin(byYear_filled, adsTS, 'year');

//Add AET to the Time Series State
byYear_filled = join.simpleJoin(byYear_filled, AET, 'year');

//Add Soil Moisture to the Time Series Stack
byYear_filled = join.simpleJoin(byYear_filled, SoilMoisture, 'year');

//Add Water Stress to the Time Series Stack
byYear_filled = join.simpleJoin(byYear_filled, Stress, 'year');

//Add GPP to the Time Series Stack
byYear_filled = join.simpleJoin(byYear_filled, GPP, 'year');

//Add SPI48 to the Time Series Stack
byYear_filled = join.simpleJoin(byYear_filled, spi48, 'year');

//Add Herb, Tree, Shrub, and Bare cover to the time series stack
byYear_filled = join.simpleJoin(byYear_filled, herb, 'year');
byYear_filled = join.simpleJoin(byYear_filled, tree, 'year');
byYear_filled = join.simpleJoin(byYear_filled, bare, 'year');
byYear_filled = join.simpleJoin(byYear_filled, shrub, 'year');

//Add Montanta Vegetation
byYear_filled = join.simpleJoin(byYear_filled, all_veg, 'year');

//Add annual temperature and precip layers
byYear_filled = join.simpleJoin(byYear_filled, temp, 'year');
byYear_filled = join.simpleJoin(byYear_filled, precip, 'year');

//Figure out what EVT groups are present
var evt_values = byYear_filled.select('lf_evt_2001').first().reduceRegion({reducer: ee.Reducer.frequencyHistogram(), geometry: sierra.union().geometry(), scale: 30, tileScale: 4, maxPixels: 1e8});

//Get EVT frequency histogram
print(evt_values);

//Add the Asset Rasterized FRAP Layers
var frap2010 = ee.Image('users/cnorlen/Fire_Dieoff/frap_rx_year_2010_v3');
var frap2019 = ee.Image('users/cnorlen/Fire_Dieoff/frap_rx_year_2019_v3');
var frap2020 = ee.Image('users/cnorlen/Fire_Dieoff/frap_rx_year_2020_v3');
var frapCount2010 = ee.Image('users/cnorlen/Fire_Dieoff/frap_rx_count_2010_v3');
var frapCount2019 = ee.Image('users/cnorlen/Fire_Dieoff/frap_rx_count_2019_v3');
var frapCount2020 = ee.Image('users/cnorlen/Fire_Dieoff/frap_rx_count_2020_v3');

//Add the Asset Rasterized Rx Layers
var rx2010 = ee.Image('users/cnorlen/Fire_Dieoff/rx_year_2010');
var rx2019 = ee.Image('users/cnorlen/Fire_Dieoff/rx_year_2019');
var rx2020 = ee.Image('users/cnorlen/Fire_Dieoff/rx_year_2020');
var rxCount2010 = ee.Image('users/cnorlen/Fire_Dieoff/rx_count_2010');
var rxCount2019 = ee.Image('users/cnorlen/Fire_Dieoff/rx_count_2019');
var rxCount2020 = ee.Image('users/cnorlen/Fire_Dieoff/rx_count_2020');

//Map.addLayer(frap2019.select('fire_type_last').eq(1), {}, 'FRAP Image (');

//Add the Assett Rasterized Fire Severity Layers
var sev2010 = ee.Image('users/cnorlen/Fire_Dieoff/fire_sev_year_2010_v2');
var sev2019 = ee.Image('users/cnorlen/Fire_Dieoff/fire_sev_year_2019_v2');
var sev2020 = ee.Image('users/cnorlen/Fire_Dieoff/fire_sev_year_2020_v2');
var sevCount2010 = ee.Image('users/cnorlen/Fire_Dieoff/fire_sev_count_2010_v2');
var sevCount2019 = ee.Image('users/cnorlen/Fire_Dieoff/fire_sev_count_2019_v2');
var sevCount2020 = ee.Image('users/cnorlen/Fire_Dieoff/fire_sev_count_2020_v2');

//Visualization parameters
var fire2010Vis = {min: 1980,
               max: 2010,
               palette: ['yellow', 'orange', 'red']
};
var sev2010Vis = {min: 1,
               max: 4,
               palette: ['green','yellow', 'orange', 'red']
};

//Add the Assett Buffer Fire layers
//FRAP Fire Buffer
var frapBuffer2010 = ee.Image('users/cnorlen/Fire_Dieoff/buffer_frap_year_2010_2km');

//Rx Fire Buffer
var rxBuffer2010 = ee.Image('users/cnorlen/Fire_Dieoff/buffer_rx_year_2010_2km_v3');

//USFS Fire Severity Category Buffer
var unchangedBuffer2010 = ee.Image('users/cnorlen/Fire_Dieoff/unchanged_buffer_year_2010_2km');
var lowBuffer2010 = ee.Image('users/cnorlen/Fire_Dieoff/low_buffer_year_2010_2km');
var medBuffer2010 = ee.Image('users/cnorlen/Fire_Dieoff/med_buffer_year_2010_2km');
var highBuffer2010 = ee.Image('users/cnorlen/Fire_Dieoff/high_buffer_year_2010_2km');

//USFS Overall Fire Severity Buffer
var sevBuffer2010 = ee.Image('users/cnorlen/Fire_Dieoff/buffer_sev_year_2010_2km');
///////////////////// STRAT SAMPLE CONTROL PIXELS /////////////////////


///////////////////// STRAT SAMPLE CONTROL PIXELS /////////////////////

// toBands for the byYear veg bands
var byYear_BANDS = byYear_filled.select(['NDMI','Tree_Cover','Shrub_Cover', 'Herb_Cover', 'Bare_Cover', 'tpa_max', 'AET', 'ppt', 'Water_Stress', 'Soil_Moisture', 'GPP', 'SPI48', 'tmax', 'TRE', 'PFG', 'AFG', 'BGR', 'LTR', 'SHR'])//select(['NBR','GPPgf','GPPls','emapr_biomass','lemma_biomass','emaprlc','ET'])
  .toBands(); // ImageCollection to Image, with bands for each YearxBand

// create bins for elevation, precip, temp, lat
var byYear_STRAT = ee.Image(byYear_filled.select(['elevation','clm_precip_sum','clm_temp_mean', 'lf_evt_2001', 'latitude', 'longitude']).first());

var elevation_bin = byYear_STRAT.select('elevation').divide(ee.Image(100)).ceil().toInt().multiply(ee.Image(1000000000)); //.multiply(ee.Image(100))//divide into 100m bins, multiply by 100000 for spacing for strat_layer unique code
var clm_precip_sum_bin = byYear_STRAT.select('clm_precip_sum').divide(ee.Image(200)).ceil().toInt().multiply(ee.Image(1000000000000)); //divide into 200mm bins, multiply by 1000
var clm_temp_mean_bin = byYear_STRAT.select('clm_temp_mean').divide(ee.Image(5)).ceil().toInt().multiply(ee.Image(1000000)); // divide into 2degC bins, make integer
var lf_evt_bin = byYear_STRAT.select('lf_evt_2001').multiply(ee.Image(10000));

//var lat_bins = ee.List.sequence({start:32.5, end:42.25, step:0.25});
var lat_bin = byYear_STRAT.select('latitude').divide(ee.Image(0.25)).ceil().toInt(); //Round Latitude
Map.addLayer(lat_bin, {}, 'Image Lat bins');

//Create fire year bins
var fire_sev_year_bin = sev2010.select('fire_year_last')
                               .subtract(ee.Image(1979))
                               .divide(ee.Image(5))
                               .ceil()
                               .toInt()
                               .multiply(ee.Image(10000));

var buffer_sev_year_bin = sevBuffer2010.select('fire_year_last')
                                       .subtract(ee.Image(1979))
                                       .divide(ee.Image(5))
                                       .ceil()
                                       .toInt()
                                      .multiply(ee.Image(10000));

var fire_frap_year_bin = frap2010.select('fire_year_last')
                                 .subtract(ee.Image(1979))
                                       .divide(ee.Image(5))
                                       .ceil()
                                       .toInt()
                                 .multiply(ee.Image(10000));

var buffer_frap_year_bin = frapBuffer2010.select('fire_year_last')
                                         .subtract(ee.Image(1979))
                                       .divide(ee.Image(5))
                                       .ceil()
                                       .toInt()
                                         .multiply(ee.Image(10000));

var fire_rx_year_bin = rx2010.select('fire_year_last')
                             .subtract(ee.Image(1979))
                                       .divide(ee.Image(5))
                                       .ceil()
                                       .toInt()
                             .multiply(ee.Image(10000));

var buffer_rx_year_bin = rxBuffer2010.select('fire_year_last')
                                     .subtract(ee.Image(1979))
                                       .divide(ee.Image(5))
                                       .ceil()
                                       .toInt()
                                     .multiply(ee.Image(10000));

//Tree Cover bins for each Fire type, severity and for burned versus un-burned
var tree_strat_sev = all_veg.select(['TRE'],['Tree_Cover']).map(function(image){
  var dateBand = ee.Image(ee.Number(image.get('year'))).toShort();
  image = image.updateMask(dateBand.eq(sev2010.select('fire_year_last').subtract(ee.Image(2))));
  return image.addBands(dateBand.toShort().rename('vi_year'));
})
                              .reduce(ee.Reducer.lastNonNull())
                              .select('Tree_Cover_last')
                              .divide(ee.Image(20))
                              .ceil()
                              .toInt()
                              .multiply(ee.Image(100))
                              ;

var tree_strat_sev_buffer = all_veg.select(['TRE'],['Tree_Cover']).map(function(image){
  var dateBand = ee.Image(ee.Number(image.get('year'))).toShort();
  image = image.updateMask(dateBand.eq(sevBuffer2010.select('fire_year_last').subtract(ee.Image(2))));
  return image.addBands(dateBand.toShort().rename('vi_year'));
})
                              .reduce(ee.Reducer.lastNonNull())
                              .select('Tree_Cover_last')
                              .divide(ee.Image(20))
                              .ceil()
                              .toInt()
                              .multiply(ee.Image(100))
                              ;

var tree_strat_frap = all_veg.select(['TRE'],['Tree_Cover']).map(function(image){
  var dateBand = ee.Image(ee.Number(image.get('year'))).toShort();
  image = image.updateMask(dateBand.eq(frap2010.select('fire_year_last').subtract(ee.Image(2))));
  return image.addBands(dateBand.toShort().rename('vi_year'));
})
                              .reduce(ee.Reducer.lastNonNull())
                              .select('Tree_Cover_last')
                              .divide(ee.Image(20))
                              .ceil()
                              .toInt()
                              .multiply(ee.Image(100))
                              ;
Map.addLayer(tree_strat_frap, {}, '5% Tree Stratification');

var tree_strat_frap_buffer = all_veg.select(['TRE'],['Tree_Cover']).map(function(image){
  var dateBand = ee.Image(ee.Number(image.get('year'))).toShort();
  image = image.updateMask(dateBand.eq(frapBuffer2010.select('fire_year_last').subtract(ee.Image(2))));
  return image.addBands(dateBand.toShort().rename('vi_year'));
})
                              .reduce(ee.Reducer.lastNonNull())
                              .select('Tree_Cover_last')
                              .divide(ee.Image(20))
                              .ceil()
                              .toInt()
                              .multiply(ee.Image(100))
                              ;

var tree_strat_rx = all_veg.select(['TRE'],['Tree_Cover']).map(function(image){
  var dateBand = ee.Image(ee.Number(image.get('year'))).toShort();
  image = image.updateMask(dateBand.eq(rx2010.select('fire_year_last').subtract(ee.Image(2))));
  return image.addBands(dateBand.toShort().rename('vi_year'));
})
                              .reduce(ee.Reducer.lastNonNull())
                              .select('Tree_Cover_last')
                              .divide(ee.Image(20))
                              .ceil()
                              .toInt()
                              .multiply(ee.Image(100))
                              ;

var tree_strat_rx_buffer = all_veg.select(['TRE'],['Tree_Cover']).map(function(image){
  var dateBand = ee.Image(ee.Number(image.get('year'))).toShort();
  image = image.updateMask(dateBand.eq(rxBuffer2010.select('fire_year_last').subtract(ee.Image(2))));
  return image.addBands(dateBand.toShort().rename('vi_year'));
})
                              .reduce(ee.Reducer.lastNonNull())
                              .select('Tree_Cover_last')
                              .divide(ee.Image(20))
                              .ceil()
                              .toInt()
                              .multiply(ee.Image(100))
                              ;

// elevation, precip, and temp bins will be concatenated (added up) to form a unique stratlayer id:
//FRAP
var STRATfrap = 
                           fire_frap_year_bin
                                           .add(
                                           tree_strat_frap)
                                      .rename(['stratlayer']);

//FRAP Buffer                                    
var STRATfrapBuffer = 
                           buffer_frap_year_bin
                                           .add(
                                           tree_strat_frap_buffer)
                                      .rename(['stratlayer']);

//Rx Fire                                      
var STRATrx = 
                           fire_rx_year_bin
                                           .add(
                                           tree_strat_rx)
                                      .rename(['stratlayer']);

//Rx Fire Buffer                                      
var STRATrxBuffer = 
                           buffer_rx_year_bin
                                           .add(
                                           tree_strat_rx_buffer)
                                      .rename(['stratlayer']);

//Fire Severity
var STRATsev = 
                          fire_sev_year_bin
                         .add(
                         tree_strat_sev)
                                      .rename(['stratlayer']);

//Fire Severity Buffer
var STRATsevBuffer = 
                           buffer_sev_year_bin
                .add(
                tree_strat_sev_buffer)
                                      .rename(['stratlayer']);

//Create Masks
var nofrap = frap2019.select('fire_year_last').gte(1980).unmask(-9999).eq(-9999); //No FRAP mask

//Get FRAP Layers
var frap = frap2019.select('fire_year_last').gte(1980); // FRAP Mask

//FRAP Buffer Mask
var frap_buffer = frapBuffer2010.select('fire_year_last').gte(1980); //FRAP Buffer Mask

//Rx Buffer Mask
var rx_buffer = rxBuffer2010.select('fire_year_last').gte(1980); //FRAP Buffer Mask

//USFS Fire Severity Buffer Mask
var un_buffer = unchangedBuffer2010.select('fire_year_last').gte(1984);
var low_buffer = lowBuffer2010.select('fire_year_last').gte(1984);
var med_buffer = medBuffer2010.select('fire_year_last').gte(1984);
var high_buffer = highBuffer2010.select('fire_year_last').gte(1984);
var sev_buffer = sevBuffer2010.select('fire_year_last').gte(1984);

//Select Fire types
var sev = sev2010.select('fire_year_last').gte(1984);
var un = sev2010.select('fire_sev_last').eq(1);
var low = sev2010.select('fire_sev_last').eq(2);
var med = sev2010.select('fire_sev_last').eq(3);
var hi = sev2010.select('fire_sev_last').eq(4);
var dist = undist.unmask(-9999).eq(-9999); //Disturbed mask from CCDC
var wild = frap2010.select('fire_type_last').eq(1); //Mask for Wildfires
var rx = rx2010.select('fire_type_last'); //Mask for Prescribed Fires

// now combine STRAT and BANDS
var byYear_Wildfire_Control_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS.addBands(byYear_STRAT) //Add the Conifer Mask
                                   .addBands(STRATfrapBuffer) //Add the stratification layer
                                   .updateMask(nofrap)) // masks for all non-FRAP pixels
                                   .updateMask(frap_buffer) //Masks for FRAP plus 4-km buffer
                                   //Buffer Fires Years
                                   .addBands(frapBuffer2010.select(['fire_year_last','fire_type_last'], ['fire_year_2010','fire_type_2010']).unmask(-9999));


var byYear_Rxfire_Control_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS.addBands(byYear_STRAT)
                                   .addBands(STRATrxBuffer) //Add the Conifer Mask
                                   .updateMask(nofrap)) // masks for all non-FRAP pixels
                                   .updateMask(rx_buffer) //Masks for Rx plus 4-km buffer
                                   //Buffer Fires Years
                                   .addBands(rxBuffer2010.select(['fire_year_last','fire_type_last'], ['fire_year_2010','fire_type_2010']).unmask(-9999));

var byYear_sev_Control_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS.addBands(byYear_STRAT)
                                   .addBands(STRATsevBuffer) //Add the Conifer Mask
                                   .updateMask(nofrap)) // masks for all non-FRAP pixels
                                   .updateMask(sev_buffer) //Masks for USFS Fire Severity plus 4-km buffer
                                   //Buffer Fires Years
                                   .addBands(sevBuffer2010.select(['fire_year_last','fire_ID_last'], ['fire_year_2010','fire_ID_2010'])
                                   .unmask(-9999));

//Do individual samples from fire severity buffers
var byYear_un_Control_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS.addBands(byYear_STRAT)
                                   .addBands(STRATsevBuffer) //Add the Conifer Mask
                                   .updateMask(nofrap)) // masks for all non-FRAP pixels
                                   .updateMask(un_buffer) //Masks for USFS Fire Severity plus 4-km buffer
                                   //Buffer Fires Years
                                   //.addBands(rxBuffer2019.select(['fire_year_last','fire_type_last'], ['buffer_year_2019','buffer_type_2019']).unmask(-9999))
                                   .addBands(unchangedBuffer2010.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2010','fire_sev_2010','fire_ID_2010'])
                                   .unmask(-9999));

var byYear_low_Control_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS.addBands(byYear_STRAT)
                                   .addBands(STRATsevBuffer) //Add the Conifer Mask
                                   .updateMask(nofrap)) // masks for all non-FRAP pixels
                                   .updateMask(low_buffer) //Masks for USFS Fire Severity plus 4-km buffer
                                   //Buffer Fires Years
                                   .addBands(lowBuffer2010.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2010','fire_sev_2010','fire_ID_2010'])
                                   .unmask(-9999));

var byYear_med_Control_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS.addBands(byYear_STRAT)
                                   .addBands(STRATsevBuffer) //Add the Conifer Mask
                                   .updateMask(nofrap)) // masks for all non-FRAP pixels
                                   .updateMask(med_buffer) //Masks for USFS Fire Severity plus 4-km buffer
                                   //Buffer Fires Years
                                   .addBands(medBuffer2010.select(['fire_year_last','fire_sev_last', 'fire_ID_last'], ['fire_year_2010','fire_sev_2010', 'fire_ID_2010'])
                                   .unmask(-9999));

var byYear_hi_Control_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS.addBands(byYear_STRAT)
                                 .addBands(STRATsevBuffer) //Add the Conifer Mask
                                   .updateMask(nofrap)) // masks for all non-FRAP pixels
                                   .updateMask(high_buffer) //Masks for USFS Fire Severity plus 4-km buffer
                                   //Buffer Fires Years
                                   .addBands(highBuffer2010.select(['fire_year_last','fire_sev_last', 'fire_ID_last'], ['fire_year_2010','fire_sev_2010', 'fire_ID_2010'])
                                   .unmask(-9999));
                                   
//Mask for disturbed pixels
var byYear_Wildfire_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS //Add the Conifer Mask
                                   .addBands(byYear_STRAT)
                                   .addBands(STRATfrap) 
                                   //Fire years
                                   .addBands(frap2020.select(['fire_year_last','fire_type_last'], ['fire_year_2020','fire_type_2020']).unmask(-9999))
                                   .addBands(frap2019.select(['fire_year_last','fire_type_last'], ['fire_year_2019','fire_type_2019']).unmask(-9999))
                                   .addBands(frap2010.select(['fire_year_last','fire_type_last'], ['fire_year_2010','fire_type_2010']).unmask(-9999))
                                   //Fire counts
                                   .addBands(frapCount2020.select(['fire_year_count'], ['fire_count_2020']).unmask(-9999))
                                   .addBands(frapCount2019.select(['fire_year_count'], ['fire_count_2019']).unmask(-9999))
                                   .addBands(frapCount2010.select(['fire_year_count'], ['fire_count_2010']).unmask(-9999))
                                   //Add the Mask
                                   .updateMask(frap) //Mask for Frap Fires 
                                   .updateMask(wild) //Mask for wildfires in FRAP
                                   );

var byYear_Rxfire_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS //Add the Conifer Mask
                                   .addBands(byYear_STRAT)
                                   .addBands(STRATrx) 
                                   //Fire years
                                   .addBands(frap2020.select(['fire_year_last','fire_type_last'], ['fire_year_2020','fire_type_2020']).unmask(-9999))
                                   .addBands(frap2019.select(['fire_year_last','fire_type_last'], ['fire_year_2019','fire_type_2019']).unmask(-9999))
                                   .addBands(frap2010.select(['fire_year_last','fire_type_last'], ['fire_year_2010','fire_type_2010']).unmask(-9999))
                                   //Fire counts
                                   .addBands(frapCount2020.select(['fire_year_count'], ['fire_count_2020']).unmask(-9999))
                                   .addBands(frapCount2019.select(['fire_year_count'], ['fire_count_2019']).unmask(-9999))
                                   .addBands(frapCount2010.select(['fire_year_count'], ['fire_count_2010']).unmask(-9999))
                                   //Add the Mask
                                   .updateMask(frap) //Mask for Frap Fires 
                                   .updateMask(rx) //Mask for prescribed fires in FRAP
                                   );

//Mask for disturbed pixels
var byYear_Sevfire_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS //Add the Conifer Mask
                                   .addBands(byYear_STRAT)
                                   .addBands(STRATsev) 
                                   //Fire years
                                   .addBands(sev2020.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2020','fire_sev_2020','fire_ID_2020']).unmask(-9999))
                                   .addBands(sev2019.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2019','fire_sev_2019','fire_ID_2019']).unmask(-9999))
                                   .addBands(sev2010.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2010','fire_sev_2010','fire_ID_2010']).unmask(-9999))
                                   //Fire counts
                                   .addBands(sevCount2020.select(['fire_year_count'], ['fire_count_2020']).unmask(-9999))
                                   .addBands(sevCount2019.select(['fire_year_count'], ['fire_count_2019']).unmask(-9999))
                                   .addBands(sevCount2010.select(['fire_year_count'], ['fire_count_2010']).unmask(-9999))
                                   //Add the Mask
                                   .updateMask(frap) //Mask out the FRAP fires
                                   .updateMask(sev) //Mask for USFS Fires with Severity
                                   );
                                   
//do individual fire severity samples
var byYear_un_Sevfire_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS //Add the Conifer Mask
                                   .addBands(byYear_STRAT)
                                   .addBands(STRATsev) 
                                   //Fire years
                                   .addBands(sev2020.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2020','fire_sev_2020','fire_ID_2020']).unmask(-9999))
                                   .addBands(sev2019.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2019','fire_sev_2019','fire_ID_2019']).unmask(-9999))
                                   .addBands(sev2010.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2010','fire_sev_2010','fire_ID_2010']).unmask(-9999))
                                   //Fire counts
                                   .addBands(sevCount2020.select(['fire_year_count'], ['fire_count_2020']).unmask(-9999))
                                   .addBands(sevCount2019.select(['fire_year_count'], ['fire_count_2019']).unmask(-9999))
                                   .addBands(sevCount2010.select(['fire_year_count'], ['fire_count_2010']).unmask(-9999))
                                   //Add the Mask
                                   .updateMask(frap) //Mask out the FRAP fires
                                   .updateMask(un) //Mask for USFS Fires with Severity
                                   );

var byYear_low_Sevfire_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS //Add the Conifer Mask
                                   .addBands(byYear_STRAT)
                                   .addBands(STRATsev) 
                                   //Fire years
                                   .addBands(sev2020.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2020','fire_sev_2020','fire_ID_2020']).unmask(-9999))
                                   .addBands(sev2019.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2019','fire_sev_2019','fire_ID_2019']).unmask(-9999))
                                   .addBands(sev2010.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2010','fire_sev_2010','fire_ID_2010']).unmask(-9999))
                                   //Fire counts
                                   .addBands(sevCount2020.select(['fire_year_count'], ['fire_count_2020']).unmask(-9999))
                                   .addBands(sevCount2019.select(['fire_year_count'], ['fire_count_2019']).unmask(-9999))
                                   .addBands(sevCount2010.select(['fire_year_count'], ['fire_count_2010']).unmask(-9999))
                                   //Add the Mask
                                   .updateMask(frap) //Mask out the FRAP fires
                                   .updateMask(low) //Mask for USFS Fires with Severity
                                   );

var byYear_med_Sevfire_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS //Add the Conifer Mask
                                   .addBands(byYear_STRAT)
                                   .addBands(STRATsev) 
                                   //Fire years
                                   .addBands(sev2020.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2020','fire_sev_2020','fire_ID_2020']).unmask(-9999))
                                   .addBands(sev2019.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2019','fire_sev_2019','fire_ID_2019']).unmask(-9999))
                                   .addBands(sev2010.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2010','fire_sev_2010','fire_ID_2010']).unmask(-9999))
                                   //Fire counts
                                   .addBands(sevCount2020.select(['fire_year_count'], ['fire_count_2020']).unmask(-9999))
                                   .addBands(sevCount2019.select(['fire_year_count'], ['fire_count_2019']).unmask(-9999))
                                   .addBands(sevCount2010.select(['fire_year_count'], ['fire_count_2010']).unmask(-9999))
                                   //Add the Mask
                                   .updateMask(frap) //Mask out the FRAP fires
                                   .updateMask(med) //Mask for USFS Fires with Severity
                                   );

var byYear_hi_Sevfire_BANDSTRAT = mask.addConiferMask_LF(byYear_BANDS //Add the Conifer Mask
                                   .addBands(byYear_STRAT)
                                   .addBands(STRATsev) 
                                   //Fire years
                                   .addBands(sev2020.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2020','fire_sev_2020','fire_ID_2020']).unmask(-9999))
                                   .addBands(sev2019.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2019','fire_sev_2019','fire_ID_2019']).unmask(-9999))
                                   .addBands(sev2010.select(['fire_year_last','fire_sev_last','fire_ID_last'], ['fire_year_2010','fire_sev_2010','fire_ID_2010']).unmask(-9999))
                                   //Fire counts
                                   .addBands(sevCount2020.select(['fire_year_count'], ['fire_count_2020']).unmask(-9999))
                                   .addBands(sevCount2019.select(['fire_year_count'], ['fire_count_2019']).unmask(-9999))
                                   .addBands(sevCount2010.select(['fire_year_count'], ['fire_count_2010']).unmask(-9999))
                                   //Add the Mask
                                   .updateMask(frap) //Mask out the FRAP fires
                                   .updateMask(hi) //Mask for USFS Fires with Severity
                                   );

// now export stratified control sample for South Sierra
var sierraWildfireControlSamples = byYear_Wildfire_Control_BANDSTRAT.stratifiedSample({
    numPoints: 600, // 100 pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, //300,
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });

var sierraRxfireControlSamples = byYear_Rxfire_Control_BANDSTRAT.stratifiedSample({
    numPoints: 600, // 100 pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, //300,
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });

//Fire Severity Buffers
var sierraSevControlSamples = byYear_sev_Control_BANDSTRAT.stratifiedSample({
    numPoints: 600, // pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, //scale in meters
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });

// now export stratified wildfire samples for South Sierra
var sierraWildfireSamples = byYear_Wildfire_BANDSTRAT.stratifiedSample({
    numPoints: 600, // pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });
  
// now export stratified prescribed fire samples for South Sierra
var sierraRxfireSamples = byYear_Rxfire_BANDSTRAT.stratifiedSample({
    numPoints: 600, // pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });

// now export stratified USFS fire severity samples for South Sierra
var sierraSevfireSamples = byYear_Sevfire_BANDSTRAT.stratifiedSample({
    numPoints: 1200, 
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });

//Individual Sevfire control samples
var sierraUnControlSamples = byYear_un_Control_BANDSTRAT.stratifiedSample({
    numPoints: 600, 
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });

var sierraLowControlSamples = byYear_low_Control_BANDSTRAT.stratifiedSample({
    numPoints: 600, 
    seed: 123, 
    scale: 30, //300,
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });
  
  var sierraMedControlSamples = byYear_med_Control_BANDSTRAT.stratifiedSample({
    numPoints: 600, 
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });
  
  var sierraHiControlSamples = byYear_hi_Control_BANDSTRAT.stratifiedSample({
    numPoints: 600, // pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });
  
//Individual Sevfire sample 
var sierraUnSevfireSamples = byYear_un_Sevfire_BANDSTRAT.stratifiedSample({
    numPoints: 600, // pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });

var sierraLowSevfireSamples = byYear_low_Sevfire_BANDSTRAT.stratifiedSample({
    numPoints: 600, // pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });

var sierraMedSevfireSamples = byYear_med_Sevfire_BANDSTRAT.stratifiedSample({
    numPoints: 600, // pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });
  
var sierraHiSevfireSamples = byYear_hi_Sevfire_BANDSTRAT.stratifiedSample({
    numPoints: 600, // pixels from each unique strat_sample bin
    seed: 123, 
    scale: 30, 
    classBand: "stratlayer",
    region: sierra.union().geometry(),
    geometries: false,
    tileScale: 4,
    dropNulls: true
  });
  
// exports
// Wildfire stratified sample exports
Export.table.toDrive({
  collection:sierraWildfireSamples,
  description: 'fire_south_sierra_FRAP_wildfire_600pt_5_fire_year_20tree_ts4_30m_20231204',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

// Prescribed Fire stratified sample exports
Export.table.toDrive({
  collection:sierraRxfireSamples,
  description: 'fire_south_sierra_FRAP_rxfire_600pt_5_fire_year_20tree_ts4_30m_20231204',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

// Severity Fire stratified sample exports
Export.table.toDrive({
  collection:sierraSevfireSamples,
  description: 'fire_south_sierra_USFS_sevfire_1200pt_5_fire_year_20tree_ts4_30m_20231207',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

// Unburned FRAP Buffer (2-km) stratified sample exports
Export.table.toDrive({
  collection:sierraWildfireControlSamples,
  description: 'control_south_sierra_FRAP_2km_buffer_600pt_5_fire_year_20tree_ts4_30m_20231204',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

// Unburned Rx Buffer (2-km) stratified sample exports
Export.table.toDrive({
  collection:sierraRxfireControlSamples,
  description: 'control_south_sierra_Rx_2km_buffer_2400pt_5_fire_year_20tree_ts4_30m_20231204',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});


//USFS Fire Severity Samples

// USFS Fire Severity Buffers (2-km) stratified sample exports
Export.table.toDrive({
  collection:sierraSevControlSamples,
  description: 'control_south_sierra_sev_2km_buffer_2400pt_5_fire_year_20tree_ts4_30m_20231204',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

// Severity Fire stratified sample exports
Export.table.toDrive({
  collection:sierraUnSevfireSamples,
  description: 'fire_south_sierra_un_sev_600pt_5_fire_year_20tree_ts4_30m_20231206',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection:sierraLowSevfireSamples,
  description: 'fire_south_sierra_low_sev_600pt_5_fire_year_20tree_ts4_30m_20231206',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection:sierraMedSevfireSamples,
  description: 'fire_south_sierra_med_sev_600pt_5_fire_year_20tree_ts4_30m_20231206',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection:sierraHiSevfireSamples,
  description: 'fire_south_sierra_hi_sev_600pt_5_fire_year_20tree_ts4_30m_20231206',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

// Unburned USFS Fire Severity Buffers (2-km) stratified sample exports
Export.table.toDrive({
  collection:sierraUnControlSamples,
  description: 'control_south_sierra_un_sev_2km_buffer_600pt_5_fire_year_20tree_ts4_30m_20231206',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection:sierraLowControlSamples,
  description: 'control_south_sierra_low_sev_2km_buffer_600pt_5_fire_year_20tree_ts4_30m_20231206',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection:sierraMedControlSamples,
  description: 'control_south_sierra_med_sev_2km_buffer_600pt_5_fire_year_20tree_ts4_30m_20231206',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection:sierraHiControlSamples,
  description: 'control_south_sierra_hi_sev_2km_buffer_600pt_5_fire_year_20tree_ts4_30m_20231206',
  folder: 'Fire_Dieoff',
  fileFormat: 'CSV'
});