JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.round((Math.pow(10, num) + Number.EPSILON) * 1000) / 1000); } #3 decimal places
    })
}"
