$( document ).ready(function() {

});

Shiny.addCustomMessageHandler('fun', function(arg) {

})


var test = new Shiny.InputBinding();
$.extend(test, {
  find: function(scope) {
    // JS logic $(scope).find('whatever')
  },
  getValue: function(el) {
    // JS code to get value
  },
  setValue: function(el, value) {
    // JS code to set value
  },
  receiveMessage: function(el, data) {
    // this.setValue(el, data);
  },
  subscribe: function(el, callback) {
    $(el).on('click.test', function(e) {
      callback();
    });

  },
  unsubscribe: function(el) {
    $(el).off('.test');
  }
});
Shiny.inputBindings.register(test, 'shiny.whatever');
