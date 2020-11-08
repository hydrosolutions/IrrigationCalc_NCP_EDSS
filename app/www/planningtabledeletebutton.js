$(document).on('click', '#planningtable button', function () {
 Shiny.onInputChange('lastClickId',this.id);
 Shiny.onInputChange('lastClick', Math.random())
 });
