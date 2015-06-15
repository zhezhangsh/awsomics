# Add HTML tag and color to a message
geex.html.msg<-function(msg, color='#8888FF') {
  paste('<font color="', color, '">', msg, '</font>', sep='');
}