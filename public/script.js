$(document).ready(function() {
    $('#render-button').click(function(e) {
        e.preventDefault()
        $("#svg").load("/svg", {drawingText: $('#textarea').val()});
    })
})
