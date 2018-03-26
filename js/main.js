function showChooseVars(){
	$('#choose_vars').removeClass('hidden');
	$('#choose_vars').addClass('visible');
};

function reset(){
	if(visible=1) {
		$('#census_iframe').removeClass('iframe_visible');
		$('#census_iframe').addClass('hidden');
		$('#elections_iframe').removeClass('iframe_visible');
		$('#elections_iframe').addClass('hidden');
		$('#flowers_iframe').removeClass('iframe_visible');
		$('#flowers_iframe').addClass('hidden');
		visible=0;
	}
}


$('#census').on('click',function(e){
	e.preventDefault();
	reset();
	showChooseVars();
	$('#census_iframe').removeClass('hidden');
	$('#census_iframe').addClass('iframe_visible');
	var visible=1;
});

$('#flowers').on('click',function(e){
	e.preventDefault();
	reset();
	showChooseVars();
	$('#flowers_iframe').removeClass('hidden');
	$('#flowers_iframe').addClass('iframe_visible');
	visible=1;
});

$('#elections').on('click',function(e){
	e.preventDefault();
	reset();
	showChooseVars();
	$('#elections_iframe').removeClass('hidden');
	$('#elections_iframe').addClass('iframe_visible');
	visible=1;
});











/*function initMap() {
	var map = new google.maps.Map( document.getElementById('map'), {
      		center:{lat: 40.8054491, lng: -73.9654415}, 
  			zoom: 10,
    		scrollwheel: false});
    };
    
};
*/


/*initMap();







// Step 1: When the window is resized, add the snazzy class to the body.

		$(window).on('resize', function() {
			$('body').addClass('snazzy');
			console.log('hi!');
		});

// STEP 2. When the user's mouse enters #trigger (the yellow square)
	// a) Add a <li> to the beginning of #myList that contains the text "1" (Hint: you'll need to use the prepend jquery method)
	// b) Add a <li> to the end of #myList that contains the text "5" (Hint: you'll need to use the append jquery method)

		$('#trigger').on('mouseover', function(a) {
			$('#myList').prepend('1'); 
			$('#myList').append('5');
		});

	// b) Update the src for the img to 'images/outdoors.jpeg'*/
