require("normalize.css");
require("./styles/main.scss");

import Audio from "./audio";

const App = require("../elm/Main");

document.addEventListener("DOMContentLoaded", () => {
  const elmApp = App.Elm.Main.init({
    node: document.getElementById("elm")
  });

  const audio = new Audio();

  elmApp.ports.playNote.subscribe(note => {
    audio.playNote(note);
  });
});
