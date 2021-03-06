window.onload = form;

function form() {
  var imp = "/swagger/import";
  $("#fillerDiv").hide();
  $("#overview-sub-form").hide();

  $("#use-url").click(function () {
      $("#paste-spec").addClass('btn-default').removeClass('btn-info');
      $("#use-url").addClass('btn-info').removeClass('btn-default');
      $("#fillerDiv").hide();
      $("#urldiv").show();
      $("#Head").show();
      $("#url").prop('required', true);
      $("#swag").prop('required', false);
      $("#swag").val("");
  });

  $("#paste-spec").click(function () {
      $("#use-url").addClass('btn-default').removeClass('btn-info');
      $("#paste-spec").addClass('btn-info').removeClass('btn-default');
      $("#Boxer").text('Use URL to specification')
      $("#fillerDiv").show();
      $("#urldiv").hide();
      $("#Head").hide();
      $("#url").prop('required', false);
      $("#swag").prop('required', true);
      $("#url").val("");
      $("#headers").val("");
  });

  $("#overview-copy-checkbox").change(function(){
  console.log("checkbox change")
    $("#overview-sub-form").toggle();
  });
  $("#clear-form").click(function() {
    $("form").find("input#url, input#wsname, textarea").val("");
    $("form").show();
    $("#error-dialog").hide();
    $("#progress-dialog").hide();
    $("#socket-area").hide();
  });
  $("form#swagger").on("submit", function(e) {
    e.preventDefault();
    $("form").hide();
    $("#progress-dialog").show();
    $("#socket-area").show();
    var swaggerSocket = new WebSocket(getSocketAddress());
    swaggerSocket.onmessage = function (event) {
      $("#socket-area").val($("#socket-area").val() + event.data + "\n");
      $('#socket-area').scrollTop($('#socket-area')[0].scrollHeight);
    };
    $.ajax({
      type: "POST",  
      url: imp,
      timeout: 600000,
      data: $("form#swagger").serialize(),
      success: function(data) {
        window.top.location.href = data;
      },
      error: function() {
        $("#socket-area").hide();
        $("#progress-dialog").hide();
        $("#error-dialog p").empty().html("<p>An unexpected error occurred! Please help us make this addon better by submitting an issue.</p>").parent().show();
      },
      statusCode: {
        422: function(msg) {
          $("#socket-area").hide();
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
          $("#socket-area").hide();
          $("#progress-dialog").hide();
          $("#error-dialog p").empty().html(msg.responseJSON.error).parent().show();
        }
      }
    });
  });
}

function getSocketAddress() {
  if(location.protocol === 'https:') {
    return "wss://" + location.host +"/swagger/socket";
  } else {
    return "ws://" + location.host + "/swagger/socket";
  }
}
