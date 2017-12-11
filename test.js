var slideIndex = 1;
showSlides(slideIndex);
showMSlides(slideIndex);
showTSlides(slideIndex);

function currentSlide(n) {
  showSlides(slideIndex = n);
}
function currentMSlide(n) {
  showMSlides(slideIndex = n);
}
function showMSlides(n) {
  var i;
  var slides = document.getElementsByClassName("Mboxes");
  var dots = document.getElementsByClassName("dot");
  if (n > slides.length) {slideIndex = 1}    
  if (n < 1) {slideIndex = slides.length}
  for (i = 0; i < slides.length; i++) {
      slides[i].style.display = "none";  
  }
  for (i = 0; i < dots.length; i++) {
      dots[i].className = dots[i].className.replace(" active", "");
  }
  slides[slideIndex-1].style.display = "block";  
  dots[slideIndex-1].className += " active";
}

function currentTSlide(n) {
  showTSlides(slideIndex = n);
}
function showTSlides(n) {
  var i;
  var slides = document.getElementsByClassName("Tboxes");
  var dots = document.getElementsByClassName("dot");
  if (n > slides.length) {slideIndex = 1}    
  if (n < 1) {slideIndex = slides.length}
  for (i = 0; i < slides.length; i++) {
      slides[i].style.display = "none";  
  }
  for (i = 0; i < dots.length; i++) {
      dots[i].className = dots[i].className.replace(" active", "");
  }
  slides[slideIndex-1].style.display = "block";  
  dots[slideIndex-1].className += " active";
}


function showSlides(n) {
  var i;
  var slides = document.getElementsByClassName("boxes");
  var dots = document.getElementsByClassName("dot");
  if (n > slides.length) {slideIndex = 1}    
  if (n < 1) {slideIndex = slides.length}
  for (i = 0; i < slides.length; i++) {
      slides[i].style.display = "none";  
  }
  for (i = 0; i < dots.length; i++) {
      dots[i].className = dots[i].className.replace(" active", "");
  }
  slides[slideIndex-1].style.display = "block";  
  dots[slideIndex-1].className += " active";
}
function delhiVis(){
    var DSlide=document.getElementById("DelhiSmog");
    DSlide.style.width="100%";
    document.getElementById("DDots").style.visibility= "visible";
    var elements = document.getElementsByClassName("contents");
    for (var i = 0; i < elements.length; i++) {
        elements[i].style.visibility= "visible";
    }
}
function closeDel(){
   var DSlide=document.getElementById("DelhiSmog");
    DSlide.style.width="0%";
    document.getElementById("DDots").style.visibility= "hidden";
    var elements = document.getElementsByClassName("contents");
    for (var i = 0; i < elements.length; i++) {
        elements[i].style.visibility= "hidden";
    }
}

function mumbaiVis(){
    var DSlide=document.getElementById("MumRain");
    DSlide.style.width="100%";
    document.getElementById("MDots").style.visibility= "visible";
    var elements = document.getElementsByClassName("Mcontents");
    for (var i = 0; i < elements.length; i++) {
        elements[i].style.visibility= "visible";
    }
    document.getElementById("clM").style.visibility="visible";
}
function closeMum(){
   var DSlide=document.getElementById("MumRain");
    DSlide.style.width="0%";
    document.getElementById("MDots").style.visibility= "hidden";
    var elements = document.getElementsByClassName("Mcontents");
    for (var i = 0; i < elements.length; i++) {
        elements[i].style.visibility= "hidden";
    }
    document.getElementById("clM").style.visibility="hidden";
}

function TweetVis(){
    var DSlide=document.getElementById("Tweet");
    DSlide.style.height="100%";
    document.getElementById("TDots").style.visibility= "visible";
    var elements = document.getElementsByClassName("Tcontents");
    for (var i = 0; i < elements.length; i++) {
        elements[i].style.visibility= "visible";
    }
    document.getElementById("clT").style.visibility="visible";
}
function closeTwitt(){
   var DSlide=document.getElementById("Tweet");
    DSlide.style.height="0%";
    document.getElementById("TDots").style.visibility= "hidden";
    var elements = document.getElementsByClassName("Tcontents");
    for (var i = 0; i < elements.length; i++) {
        elements[i].style.visibility= "hidden";
    }
    document.getElementById("clT").style.visibility="hidden";
}