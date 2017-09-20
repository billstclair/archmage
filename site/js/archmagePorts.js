//////////////////////////////////////////////////////////////////////
//
// archmagePorts.js
// JavaScript side of ports from ArchmagePorts.elm
// Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

var archmagePorts = {};

(function () {

  var storageName = 'archmage';

  archmagePorts.init = init;
  archmagePorts.storageName = storageName;

  function attachFastClick() {
    // https://github.com/ftlabs/fastclick/blob/master/README.md
    if (Origami && CustomEvent) { // old browsers don't support CustomEvent
      var attachFastClick = Origami.fastclick;
      if (attachFastClick) {
        attachFastClick(document.body);
      }
    }
  }

  function init() {
    attachFastClick();
    var storedState = localStorage.getItem(storageName);

    //log("storedState: " + storedState + "\n")

    var archmage = Elm.ArchmagePorts.fullscreen(storedState);
    archmagePorts.archmage = archmage;

    archmage.ports.setStorage.subscribe(function(json) {
      //log("setStorage: " + json + "\n")
      localStorage.setItem(storageName, json);
    });
  }

})();
