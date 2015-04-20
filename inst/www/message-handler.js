// This recieves messages of type "testmessage" from the server.
Shiny.addCustomMessageHandler("testmessage",
  function(message) {
     $('.box').find('[data-widget="collapse"]').click();
  }
);