$(document).on("click.dt", "input", function() {
    var checkboxes = document.getElementsByName("row_selected");
    var checkboxesChecked = ["dummy"];
    for (var i = 0; i < checkboxes.length; i++) {

        if (checkboxes[i].checked) {
            checkboxesChecked.push(checkboxes[i].value);
        }
    }
    Shiny.onInputChange("checked_rows", checkboxesChecked);
})
