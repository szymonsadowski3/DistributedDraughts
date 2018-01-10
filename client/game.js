var loc = "http://127.0.0.1:8080/board";
var sendLoc = "http://127.0.0.1:8080/move/";
var COLS = 10;

BOARDSTATE = null;
WHOSEMOVE = 2;

function invertMove(move) {
    if(move==2){
        return 1;
    } else {
        return 2;
    }
}

function playerVsCpu() {
    $.get(loc, function (data) {
        var parsed_array = JSON.parse(data);
        visualizeBoard(parsed_array);

        $.get(sendLoc + WHOSEMOVE, parsed_array);
    })

    // To Be Continued
}

function initBoard() {
    $.get(loc, function (data) {
        BOARDSTATE = JSON.parse(data);
        visualizeBoard();
    })
}

$(document).ready(function () {
    initBoard();
    $("#nextMove").click(function () {
        var joinedStr = BOARDSTATE.join(" ");

        $.post(sendLoc + WHOSEMOVE, joinedStr).done(function (data) {
            BOARDSTATE = JSON.parse(data);
            WHOSEMOVE = invertMove(WHOSEMOVE);
            visualizeBoard();
        });
    });
});


function visualizeBoard() {
    var table = $(".board").find("table")[0];

    for (var i = 0; i < COLS; ++i) {
        for (var j = 0; j < COLS; ++j) {
            if (BOARDSTATE[i * COLS + j] == 0) {
                table.rows[i].children[j].innerHTML = '';
            } else if (BOARDSTATE[i * COLS + j] == 1) {
                table.rows[i].children[j].innerHTML = '<div class="white_piece"></div>';
            } else if (BOARDSTATE[i * COLS + j] == 2) {
                table.rows[i].children[j].innerHTML = '<div class="black_piece"></div>';
            }
        }
    }
}
