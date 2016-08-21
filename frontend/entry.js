import h from "virtual-dom/h";
import diff from "virtual-dom/diff";
import createElement from "virtual-dom/create-element";
import patch from "virtual-dom/patch";

const convertHTML = require('html-to-vdom')({
    VNode: require('virtual-dom/vnode/vnode'),
    VText: require('virtual-dom/vnode/vtext')
});

const renderLocal = () => h("h3", "Loading...");

let tree = renderLocal();
let root = createElement(tree);
document.body.appendChild(root);

var socket;
const connect = () => {
  console.log("connecting");
  socket = new WebSocket('ws://localhost:3000');
  window.socket = socket

  socket.onopen = function(){
    console.log("open", arguments)
  };

  socket.onclose = function(){
    console.log("close", arguments);
    setTimeout(connect, 1000);
  };

  socket.onmessage = event => {
     const newTree = convertHTML(event.data);
     const patches = diff(tree, newTree);
     root = patch(root, patches);
     tree = newTree;
  };
};
connect();


window.emit = function(data) {
  data = JSON.stringify(data);
  socket.send(data);
  console.log('emit', data);
}
