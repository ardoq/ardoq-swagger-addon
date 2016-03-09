window.onload = form;
//window.onload = socket;

function form() {
  var exampleSocket = new WebSocket("ws://localhost:4000/socket");
  exampleSocket.onmessage = function (event) {
    console.log(event.data);
  };
  var vers = true;
  var imp = "/import";
  $("#fillerDiv").hide();
  $("#selector").click(function(){
    if(!vers) {
      $("#fillerDiv").hide();
      $("#urldiv").show();
      $("#Head").show();
      $("#url").prop('required', true);
      $("#swag").prop('required', false);
      $("#swag").val("");
      $("#Boxer").addClass("disabled")
      vers = true;
    } else {
      $("#fillerDiv").show();
      $("#urldiv").hide();
      $("#Head").hide();
      $("#url").prop('required', false);
      $("#swag").prop('required', true);
      $("#url").val("");
      $("#headers").val("");
      $("#Boxer").removeClass("disabled");
      vers = false;
    }
  });
  $("#clear-form").click(function() {
    $("form").find("input#url, input#wsname, textarea").val("");
    $("form").show();
    $("#error-dialog").hide();
    $("#progress-dialog").hide();
  });
  $("form#swagger").on("submit", function(e) {
    e.preventDefault();
    $("form").hide();
    $("#progress-dialog").show();
    $.ajax({
      type: "POST",  
      url: imp,
      data: $("form#swagger").serialize(),
      success: function(data) {
        window.top.location.href = data;
      },
      error: function() {
        $("#progress-dialog").hide();
        $("#error-dialog p").empty().html("<p>An unexpected error occurred! Please help us make this addon better by submitting an issue.</p>").parent().show();
      },
      statusCode: {
        406: function(msg) {
          $("#progress-dialog").hide();
          msg = JSON.parse(msg.responseJSON.error);
          var result = "<p> ERROR: Input file does not conform to Swagger specifications</p>";
          for (var i = 0; i < msg.length; i++) {
            result += "<p>";
            result += msg[i].message;
            result += "</p>";
          }
          $("#error-dialog p").empty().html(result).parent().show();
        },
        404: function(msg) {
          $("#error-dialog p").empty().html(msg.responseJSON.error).parent().show();
        }
      }
    });
  });
  var exampleSocket = new WebSocket("ws://localhost:4000/socket");
  exampleSocket.onmessage = function (event) {
    console.log(event.data);
  };
}
