function getTextualLoc(long, lat) {
  
  var data = null;
  var finalVal;

  var xhr = new XMLHttpRequest();
  xhr.withCredentials = true;
  
  xhr.addEventListener("readystatechange", function () {
    
  	if (this.readyState == this.DONE) {
  	  result = JSON.parse(this.responseText);
  		Shiny.setInputValue("test", result[0].City + ", " + result[0].CountryId);
  		document.getElementById("test").value = result[0].City + ", " + result[0].CountryId;
  	}
  	
  });
  
  xhr.open("GET", "https://geocodeapi.p.rapidapi.com/GetNearestCities?latitude=" + lat + "&longitude=" + long + "&range=0", true);
  xhr.setRequestHeader("x-rapidapi-host", "geocodeapi.p.rapidapi.com");
  xhr.setRequestHeader("x-rapidapi-key", "438ba18331msh6814de6ff9c6756p12b320jsn292463a78e9f");
  
  xhr.send(data);
  
}

function showPosition(position) {
  getTextualLoc(position.coords.longitude, position.coords.latitude);
}

function getLoc() {
  
  if (navigator.geolocation) {
    navigator.geolocation.getCurrentPosition(showPosition);
  } else {
    alert("Geolocation is not supported by this browser.");
  }
  
}

$(document).on("click", ".add-panel", function(evt) {
  
  $(this).children().toggle();
  $(evt.target).data("value", $(evt.target).data("value") + 1);
  $(evt.target).trigger("change");
  
});

var incrementBinding = new Shiny.InputBinding();
  
  $.extend(incrementBinding, {
    
    find: function(scope) {
      return $(scope).find('.add-panel');
    },
    
    getValue: function(el) {
      return($(el).data("value"));
    },
    
    setValue: function(el, value) {
      $(el).data("value", $(evt.target).data("value") + 1);
    },
    
    subscribe: function(el, callback) {
      $(el).on('change.add-panel',function(event) {
        callback();
      });
    },
    
    unsubscribe: function(el) {
      $(el).off('.add-panel');
    },
    
    receiveMessage: function(el, data) {
      this.setValue(el, data.value);
      $(el).trigger('change');
    },
    
    getType: function(el) {
      return "addGlancePanel";
    }
    
  });
  
  Shiny.inputBindings.register(incrementBinding);

window.onload = function () {
  
  getLoc();
  
}