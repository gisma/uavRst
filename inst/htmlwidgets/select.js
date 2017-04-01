// arbitrary projected local tiles as leaflet map

HTMLWidgets.widget({

  name: 'select',

  type: 'output',

  initialize: function(el, width, height) {

    // we need a not htmlwidget div in the widget container
    addElement("lnlt");
    addElement("coords");
    //addElement("download");
    addElement("export");
    //addElement("grabber");
    // initialize the leaflet map staticly at the "el" object
    // hard-coding center/zoom here for a non-empty initial view, since there
    // is no way for htmlwidgets to pass initial params to initialize()
    // so we set maxbounds to the world and center somewhat at 0 Lat 0 Lon
    var southWest = L.latLng(-90, -180),
    northEast = L.latLng(90, 180),
    bounds = L.latLngBounds(southWest, northEast);
    var map = new L.map(el, {
      center: mapCenter,
      maxBounds: bounds,
      zoom: initialZoom
    });

    // we could add more (static) leaflet stuff here ;-)

    // The map is rendered staticly => so there would be no output binding
    // for the further handling we generate the binding to el this.getId(el)
    if (typeof this.getId === 'undefined') return map;
    map.id = this.getId(el);

    // Store the map on the element so we could find it later by ID
    $(el).data("leaflet-map", map);


    //return the initial mapsetup to the renderValue function
    return map;
     
  },

  renderValue: function(el, x, map) {
      return this.doRenderValue(el, x, map);
    },

  doRenderValue: function(el, x, map) {

      
    
   
   // we define the first layer of the list to be the default one
    var defaultLayer = L.tileLayer.provider(x.layer[0]).addTo(map);
    var baseLayers = {};
    for (var i = 0; i < x.layer.length;  i++) {
      baseLayers[x.layer[i] ] = L.tileLayer.provider(x.layer[i]);
      }
      
 // check if an array of colors (palette) or a single color is provided
   if (x.color.length <= 7 ) {
       if (x.color[1].substring(0,1) != "#" ) {
            var col =  x.color;
       }
    }
    else
    {
        var col =  x.color[x.color.length-1];
    }
    var cex = x.cex
    var color = col;
    var opacity = x.opacity;
    var lnWidth = 1;
  // style for polygons
   var polyStyle = {
     "color": col,
     "weight": 1,
     "opacity": x.opacity
   };
   // define a dummy layer for the geojson data
    //var myLayer = L.geoJson(undefined,{style:style,onEachFeature:onEachFeature}).addTo(map);
    
    
	       // JSON to CSV Converter
function iterateObject(obj) {
  var value = '', header = '';
          for (name in obj) {
            if (obj.hasOwnProperty(name)) {
              if (isObject(obj[name])) {
                var out = iterateObject(obj[name]);
                value += out.value;
                header += out.header;
              } else {
                value += removeNewLine(obj[name]) + '; ';
                header += name + '; ';
              }
            }
          }
  return {
    "value":value,
    "header":header
  };
}
function isObject(obj) {
  return (typeof obj === 'object');
}
function removeNewLine(item) {
  return item.toString().replace(/(\r\n|\n|\r)/gm,"");
}

       
    
    
	  function onEachFeature(feature, layer) {
                 layer.on({
                    click: function(e) {
                        //var layer = e.target;
                        if (layer.options.weight != 3) {
						                            layer.setStyle({
                                weight: 3,
                                color: col,
                                opacity: 1,
                                fillOpacity: 0.1
                            });
                            
							
							e.target.feature.properties.selected = true;
							
							//console.log("deselected " + feature.properties.name);
							
                        } else {
						
						                            layer.setStyle({
                                weight: 1,
                                color: col,
                                opacity: 1,
                                fillOpacity: 0.01});
							//console.log("selected " + feature.properties.name);
							
							e.target.feature.properties.selected = false;
							//$('#selectedFeatures').text( feature.properties.name );
							
							
                        }
					
											
						getAllElements();
					
                        if (!L.Browser.ie && !L.Browser.opera) {
                            layer.bringToFront();
                        }
                    }
                });
	  }
  
  function getAllElements() {
			var allMarkersObjArray = [];
			var allMarkersGeoJsonArray = [];
			var selectedFeatureName = [];
            $.each(map._layers, function (ml) {
                if (map._layers[ml].feature && map._layers[ml].feature.properties.selected === true) {
        					selectedFeatureName.push(map._layers[ml].feature.properties.name);
                }
                  if (map._layers[ml].feature && map._layers[ml].feature.properties.selected === true) {
  
                      allMarkersObjArray.push(this)
                      allMarkersGeoJsonArray.push(JSON.stringify(this.toGeoJSON()))
                      
                    //var data = drawnItems.toGeoJSON();
                   // Stringify the GeoJson
                   //var convertedData = JSON.stringify(data);
                  // Create ajax export using download.js
          
                }
            });
                      


            //console.log(selectedFeatureName);
            //var results = iterateObject(JSON.stringify(seldata));
            //$('#coords').text(results);    
            //$('#coords').html(results.value);
            //$('#coords').html(results.header);
			$('#coords').text( allMarkersGeoJsonArray );
			$('#export').text("selected items: # " + selectedFeatureName.length );
		}
	


		
	// The styles of the layer
	function style(feature) {
	       
	            return {
	                   color: col,
                    fill: true,
                    opacity: 1,
                    fillOpacity: 0.01,
                    weight: 1
	            }
	    }
	var geojsonMarkerOptions = {
    radius: x.cex,
    fillColor: x.color,
    color: x.color,
    weight: 1,
    opacity: x.opacity
};
   // create geojsonlayer
   if (x.overlay == 1) {
   var polyLayer = L.Proj.geoJson(jsondata,{ pointToLayer: function (feature, latlng) {
        return L.circleMarker(latlng, geojsonMarkerOptions);
    },style:style,onEachFeature:onEachFeature})
    
       var overlayLayers = {};
      overlayLayers['data'] = polyLayer;

	 map.addLayer(polyLayer);

  // layer control
  var layerControl = L.control.layers(baseLayers,overlayLayers).addTo(map);
   } 
  else  {
  var layerControl = L.control.layers(baseLayers).addTo(map);  
  }
   // create draw layer
    var drawnItems = new L.FeatureGroup();
    map.addLayer(drawnItems);

  // init draw control     
  var drawControl = new L.Control.Draw({
          position: x.position,
                 draw: {
                polyline: {
            shapeOptions: {
                color: '#FFFF00',
                weight: 1
            }}, polyline: x.line,
                rectangle:x.rectangle,
                polygon: x.poly, 
                circle: x.circle,
                marker: x.point
            },
            edit: {
                featureGroup: drawnItems,
                remove: x.remove,
            }
        }).addTo(map);
    if (x.hidemenu) {
      map.removeControl(drawControl);
    }
    // Each time a feaute is created, it's added to the over arching feature group
     map.on('draw:created', function(e) {
            drawnItems.addLayer(e.layer);
        });
        


/*
    // create "save" link
        var b = document.createElement('a');
        var linkText = document.createTextNode("Save");
        b.appendChild(linkText);
        b.title = "save geoJson";
        b.href = "#";
        b.id = 'export';
        el.appendChild(b);
    
    // create "grab" link
        var a = document.createElement('a');
        var linkText = document.createTextNode("Grab");
        a.appendChild(linkText);
        a.title = "grab geoJson";
        a.href = "#";
        a.id = 'grabber';
        el.appendChild(a);
    // create "save" div container
        divInfo = document.createElement("div");
        divInfo.id ='export';
        el.appendChild(divInfo);
    // create "grab" div container
        divInfo = document.createElement("div");
        divInfo.id ='grabber';
        el.appendChild(divInfo);


        // Extract GeoJson from featureGroup
        var data = drawnItems.toGeoJSON();
        // Stringify the GeoJson
        var convertedData = JSON.stringify(data);
        var kml = tokml(data);
        var grabstring = "c(" + kml.replace(/ /g, ",") +")";
  */

    //  <span style="font: 15px " class="star" >&#x2704;</span>
    //'<span style="font-size: 20px" class="glyphicon glyphicon-download"></span>'
    
    
          


            L.easyButton(  '<span class="star">&check;</span>',function(){
                       var allMarkersObjArray = [];//new Array();
            var allMarkersGeoJsonArray = [];//new Array();

    $.each(map._layers, function (ml) {
                //console.log(map._layers)
                  if (map._layers[ml].feature && map._layers[ml].feature.properties.selected === true) {
  
                      allMarkersObjArray.push(this);
                      allMarkersGeoJsonArray.push(JSON.stringify(this.toGeoJSON()));

                    //var data = drawnItems.toGeoJSON();
                   // Stringify the GeoJson
                   //var convertedData = JSON.stringify(data);
                      var seldata = (' {' +
'"type": "FeatureCollection",' + 
' "crs": { "type": "name", "properties": { "name": "EPSG:4326" } }, ' +
' "features": [ ' + allMarkersGeoJsonArray + ']}');

                  //var kml = tokml(allMarkersGeoJsonArray);
          
                }
            });
        //   var file = new File([allMarkersGeoJsonArray], "trdz.txt", {type: "text/plain;charset=utf-8"});
          // saveAs(file);}).addTo(map);
           //download("hello world", "dlText.txt", "text/plain");
           //download(allMarkersGeoJsonArray, "download.txt", "text/plain;charset=utf-8");}).addTo(map);
        //var file = new File([allMarkersGeoJsonArray], "hello world.txt", {type: "text/plain;charset=utf-8"});
 //       saveAs(file);}).addTo(map);
        

//var blob = new Blob([allMarkersGeoJsonArray], {
// type: "text/plain;charset=utf-8"
//});

// download(blob,"test","");}).addTo(map);
//$('#coords').text(iterateObject(seldata));  
download(new Blob([seldata]), "download.txt", "text/plain;charset=utf-8");}).addTo(map);

//  uriContent = "data:text/plain;charset=utf-8," + encodeURIComponent(allMarkersGeoJsonArray);
//window.save(blob, filename)
//  window.save(uriContent, "width=200,height=100");}).addTo(map);

// generic button save
/* var data = {a:1, b:2, c:3};
var json = JSON.stringify(data);
var blob = new Blob([json], {type: "application/json"});
var url  = URL.createObjectURL(blob);

var a = document.createElement('a');
a.download    = "backup.json";
a.href        = url;
a.textContent = "Download backup.json";

document.getElementById('export').appendChild(a);
*/

  // grab the lnlt div and put the mousmove output there
  lnlt = document.getElementById('lnlt');
  map.on('mousemove', function (e) {
        lnlt.textContent =
                " Latitude: " + (e.latlng.lat).toFixed(5)
                + " | Longitude: " + (e.latlng.lng).toFixed(5)
                + " | Zoom: " + map.getZoom() + " ";
  });

},


resize: function(el, width, height, instance) {
}
});

  // get the files and returns them as text stream
  function wget(urls, fn) {
        var results = [],
            lookup = {},
            complete = 0,
            total = urls.length;

        urls.forEach(function(url) {
            var i = lookup[url] = results.length,
                request = new XMLHttpRequest();
            results.push(null);
            request.open('GET', url, true);
            request.onload = function () {
                if (request.status < 200 && request.status > 400) return;
                results[i] = request.responseText;
                complete++;
                if (complete === total) fn.apply(null, results);
            };
            request.send();
        });
    }


// we need a new div element because we have to handle
// the mouseover output seperatly
function addElement(id) {
  // generate new div Element
  var newDiv = document.createElement("div");
  // insert to DOM
  document.body.insertBefore(newDiv, null);
      //provide ID and style
      newDiv.id = id;
      //newDiv.style.cssText = css;
}

function isEven(n) {
   return n % 2 == 0;
}

