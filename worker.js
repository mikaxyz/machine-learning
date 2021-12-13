var version = "1";
importScripts('dist/worker.js');
var app = Elm.Worker.init({ flags: { version: version}});

app.ports && app.ports.workerTaskComplete && app.ports.workerTaskComplete.subscribe(function(task) {
	console.log("workerTaskComplete", task);
	postMessage(task);
});

onmessage = function(e) {
	console.log("workerTaskOnStart", e.data);
	app.ports && app.ports.workerTaskOnStart && app.ports.workerTaskOnStart.send(e.data);
}