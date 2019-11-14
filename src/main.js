import './main.scss';

import { Elm } from './elm/Main.elm';

const app = Elm.Main.init({
  node: document.getElementById('elm-main')
});

console.log('Elm is here?');

if (module.hot) {
  module.hot.accept()
}