//Load the disturbance layer
var dist = ee.Image('projects/ca-ecs/ca_firstdisturbed_neg');

//Create a visualization paletter for the disturbance layer.
var visDist = {min: 1980,
               max: 2010,
               palette: ['yellow', 'orange', 'red']
};

//Create a Disturbance Mask for only disturbances in 2010 and earlier
var maskDist = dist.select('S1_tBreak').lte(2010);

//Define the visualization colors
var visRx = {min: 1980,
               max: 2020,
               palette: ['yellow', 'orange', 'red']
};

//Create a function to parse numbers from text
var numberParse = function(feature) {
   return feature.set('YEAR', ee.Number.parse(feature.get('YEAR_')));
};

//Added a filter to the FRAP Layer
var rx_frap = ee.FeatureCollection('projects/ca-ecs/FRAP/rxburn_20_1').filter(ee.Filter.neq('YEAR_',''));

//Add the disturbance number
var rx_num = rx_frap.map(numberParse, true).filter(ee.Filter.and(ee.Filter.gte('YEAR', 1987), ee.Filter.lte('YEAR', 2010)));