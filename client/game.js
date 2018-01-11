var loc = "http://127.0.0.1:8080/board";
var sendLoc = "http://127.0.0.1:8080/move/";
var COLS = 10;

BOARDSTATE = null;
WHOSEMOVE = 2;
HISTORY = [];
CURRENT_SIZE = 40;

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
    BOARDSTATE = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
        1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0,
        1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0,
        1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,
        0, 2, 0, 2, 0, 2, 0, 2, 2, 0, 2, 0, 2,
        0, 2, 0, 2, 0, 0, 2, 0, 2, 0, 2, 0, 2,
        0, 2, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0];
    visualizeBoard();
}

function handleRestartGame() {
    WHOSEMOVE = 2;
    HISTORY = [];
    initBoard();
}

function handleRollbackMove() {
    if(HISTORY.length == 0) {
        alert('No history!');
    } else {
        BOARDSTATE = HISTORY.pop();
        WHOSEMOVE = invertMove(WHOSEMOVE);
        visualizeBoard();
    }
}

function handleNextMove() {
    var joinedStr = BOARDSTATE.join(" ");

    $.post(sendLoc + WHOSEMOVE, joinedStr).done(function (data) {
        HISTORY.push(BOARDSTATE);
        BOARDSTATE = JSON.parse(data);
        WHOSEMOVE = invertMove(WHOSEMOVE);
        visualizeBoard();
    });
}

$(document).ready(function () {
    initBoard();
    $("#nextMove").click(function () {
        handleNextMove();
    });
    $("#rollbackMove").click(function () {
        handleRollbackMove();
    });
    $("#restartGame").click(function () {
        handleRestartGame();
    });
    $("#cellSize").change(function () {
        CURRENT_SIZE = $(this).val();
        updateSize(".white_piece");
        updateSize(".black_piece");
        updateSize(".white_king");
        updateSize(".black_king");
        updateSize("td");
    });
});

function updateSize(selector) {
    $(selector).width(CURRENT_SIZE);
    $(selector).height(CURRENT_SIZE);
}


function visualizeBoard() {
    var table = $(".board").find("table")[0];

    for (var i = 0; i < COLS; ++i) {
        for (var j = 0; j < COLS; ++j) {
            if (BOARDSTATE[i * COLS + j] == 0) {
                table.rows[i].children[j].innerHTML = '';
            } else if (BOARDSTATE[i * COLS + j] == 1) {
                table.rows[i].children[j].innerHTML = '<div class="white_piece" style="width: '+CURRENT_SIZE+'px; height: '+CURRENT_SIZE+'px;"></div>';
            } else if (BOARDSTATE[i * COLS + j] == 2) {
                table.rows[i].children[j].innerHTML = '<div class="black_piece" style="width: '+CURRENT_SIZE+'px; height: '+CURRENT_SIZE+'px;"></div>';
            } else if (BOARDSTATE[i * COLS + j] == 3) {
                table.rows[i].children[j].innerHTML = '<div class="white_king" style="width: '+CURRENT_SIZE+'px; height: '+CURRENT_SIZE+'px;"></div>';
            } else if (BOARDSTATE[i * COLS + j] == 4) {
                table.rows[i].children[j].innerHTML = '<div class="black_king" style="width: '+CURRENT_SIZE+'px; height: '+CURRENT_SIZE+'px;"></div>';
            }
        }
    }
}
