var loc = "http://127.0.0.1:8080/board";
var COLS = 10;

$(document).ready(function() {
  $.get(loc, function(data) {
    var parsed_array = JSON.parse(data);
    updateBoard(parsed_array);
  });
});

function updateBoard(board_data) {
  var table = $(".board").find("table")[0];

  for (var i = 0; i < COLS; ++i) {
    for (var j = 0; j < COLS; ++j) {
      if (board_data[i * COLS + j] == 1) {
        table.rows[i].children[j].innerHTML = '<div class="white_piece"></div>';
      } else if (board_data[i * COLS + j] == 2) {
        table.rows[i].children[j].innerHTML = '<div class="black_piece"></div>';
      }
    }
  }
}
