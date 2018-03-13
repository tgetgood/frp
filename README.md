# FRP Demo

Some interactive animations demonstrating the core ideas of reactive
programming, and particularly FRP.

### Usage

Go to [the demo](https://tgetgood.github.io/frp/resources/public/index.html).

Keep in mind that this is an experiment and lots of stuff is broken. For
instance; the canvas doesn't resize with the browser window, so you have to
refresh. And you can't refresh without clicking on the address bar because all
keystrokes on the DOM are redirected...

Have fun!

### Development

To view the presentation run

```sh
lein figwheel
```

And when it's loaded point your browser to

```
localhost:3449/index.html
```

## License

Copyright © 2018 Thomas Getgood

Distributed under the Eclipse Public License either version 1.0 or (at your
option) any later version.
