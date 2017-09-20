The game of Archmage

Invented by Chris St. Clair.

Copyright 2017 Bill St. Clair &lt;<billstclair@gmail.com>&gt;<br/>
MIT License

Archmage is a board game, played on a seven-by-seven square board. Rules are in [site/docs/rules.html](site/docs/rules.html). Instructions are in [site/docs/help.html](site/docs/help.html). They are displayed by the "Rules" and "Help" links at the top and bottom of the game screen.

[src/Archmage.elm](src/Archmage.elm) is an `elm-reactor` top-level for development.

src/ArchmagePorts.elm is a port version that saves game state in the browser's local storage on every update. It works with site/index.html, which implements the port.
