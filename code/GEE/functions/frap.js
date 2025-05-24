//Import annual rasterized FRAP data
var frap = ee.ImageCollection('projects/ca-ecs/Rasterized/frap_18');

var wild_frap = ee.FeatureCollection('projects/ca-ecs/FRAP/firep20_1').filter(ee.Filter.neq('YEAR_',''));

//Parse number from text
var numberParse = function(feature) {
   return feature.set('YEAR', ee.Number.parse(feature.get('YEAR_')));
};

var wild_num = wild_frap.map(numberParse, true);

//Function to date FRAP file names and convert them into a system:time_start property
var remapFRAP = function(image) {
    var name = ee.String(image.get('system:index'));
    var year = ee.Number.parse(name.slice(5));
    return image.remap([1],[year]).toInt()
                .set('system:time_start', ee.Date.parse('YYYY', name.slice(5)));
};

//Export the FRAP Image Collection with the new system:time_start property.
exports.frap_date = frap.map(remapFRAP);

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
                     .set('system:time_start', startDate.millis()); //Set the date of the data
  }));
};

//Variables to include in the function
var startDate = 2011;
var count = 10; 
var interval = 1;

//Create the Wildfire FRAP image collection
var wildCollection = fireYrCollection(wild_num, startDate, count, interval);

//Select an Image of Fires from 2011 to 2019
var frapImage2019 = wildCollection.filter(ee.Filter.date({start:'2011-01-01', end:'2019-12-31'})).reduce(ee.Reducer.lastNonNull()).toInt16();

//Select just the fire year layer
var firedata = frapImage2019.select('fire_year_last');//.reproject({crs: 'EPSG: 5070', scale: 30});

//Create a fire mask
var firemask = (firedata.unmask()).eq(0);
