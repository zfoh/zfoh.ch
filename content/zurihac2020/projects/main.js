var storedState = localStorage.getItem('save');
var startingState = storedState ? JSON.parse(storedState) : [];
var app = Elm.Projects.init({
  node: document.getElementById('elm'),
  flags: startingState
});
app.ports.save.subscribe(function(data) {
  localStorage.setItem('save', JSON.stringify(data));
});
