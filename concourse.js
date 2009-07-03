// concourse.js - functions needed across the application

var dbname = "concourse"; // TODO: get dynamically?

var url = function() {
  // Wow, Javascript is horrifyingly bad. Somehow this keeps surprising me.
  var parts = Array.prototype.slice.call(arguments);
  return "/" + [dbname].concat(parts).join("/");
};

var current_user = function() {
  return "technomancy@gmail.com";
};

var insert_description = function(name, description, length) {
  $("h1#name").html(name);
  $("#gathering-description p:first").html(description);
  $("#length").html("<b>Length</b>: " + length + " hours");
};
