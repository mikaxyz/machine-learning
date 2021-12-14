var version = "1";
importScripts('dist/worker.js');
var app = Elm.Worker.init({ flags: { version: version}});

app.ports && app.ports.workerTaskComplete && app.ports.workerTaskComplete.subscribe(function(task) {
	console.log("workerTaskComplete", task);
	postMessage(task);
});

app.ports && app.ports.workerTaskProgress && app.ports.workerTaskProgress.subscribe(function(data) {
	console.log("workerTaskProgress", data);
	postMessage(data);
});

onmessage = function(e) {
	console.log("workerTaskOnStart", e.data);
	app.ports && app.ports.workerTaskOnStart && app.ports.workerTaskOnStart.send(e.data);
}