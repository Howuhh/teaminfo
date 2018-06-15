// Copy from https://stackoverflow.com/a/21065846/1248256
var _table_ = document.createElement('table'),
_tr_ = document.createElement('tr'),
_th_ = document.createElement('th'),
_td_ = document.createElement('td');

_table_.className = "table table-striped"

function buildHtmlTable(arr) {
    var table = _table_.cloneNode(false),
    columns = addAllColumnHeaders(arr, table);
    for (var i=0, maxi=arr.length; i < maxi; ++i) {
        var tr = _tr_.cloneNode(false);
        for (var j=0, maxj=columns.length; j < maxj ; ++j) {
            var td = _td_.cloneNode(false);
            cellValue = arr[i][columns[j]];
            td.appendChild(document.createTextNode(arr[i][columns[j]] || ''));
            tr.appendChild(td);
        }
        table.appendChild(tr);
    }
    return table;
}

// Adds a header row to the table and returns the set of columns.
// Need to do union of keys from all records as some records may not contain
// all records
function addAllColumnHeaders(arr, table)
{
    var columnSet = [],
    tr = _tr_.cloneNode(false);
    for (var i=0, l=arr.length; i < l; i++) {
        for (var key in arr[i]) {
            if (arr[i].hasOwnProperty(key) && columnSet.indexOf(key)===-1) {
                columnSet.push(key);
                var th = _th_.cloneNode(false);
                th.appendChild(document.createTextNode(key));
                tr.appendChild(th);
            }
        }
    }
    table.appendChild(tr);
    return columnSet;
}


function onReceived(obj) {
    var url = 'http://127.0.0.1:8120/GetTeamStruct?team_name=' + obj.selectedText;

    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
        if (xhr.readyState == 4) {

            var data = JSON.parse(xhr.responseText)[0]
            var table = buildHtmlTable(data)
            document.getElementById("output").appendChild(table);
        }
    };
    xhr.open("GET", url, true);
    xhr.send();
}

// When the popup HTML has loaded
window.addEventListener('load', function(evt) {
// Get the event page
    chrome.runtime.getBackgroundPage(function(eventPage) {

      eventPage.getPageDetails(onReceived);
    });
});
