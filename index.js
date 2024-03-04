#! /usr/bin/env node

const fs = require("fs");
const ConcurrentTask = require("@andrewmacmurray/elm-concurrent-task");


console.time("total");
process.on("beforeExit", () => {
    console.timeEnd("total");
});

const argv = require("yargs")
	.usage("Usage: $0 data")
	.example("$0 /mnist", "TODO")
	.demandCommand(1, 1, "Need a filename to training data")
	.argv;

const [fileName] = argv._;

if (!fs.existsSync(fileName)) {
    return console.error(`File (${fileName}) not found`);
}

const {Elm} = require("./dist/main.js");

const app = Elm.Train.init({flags: {fileName: fileName}});

ConcurrentTask.register({
    tasks: {
        "cli:readFile": readFile,
        "cli:saveModel": saveModel
    },
    ports: {
        send: app.ports.send,
        receive: app.ports.receive,
    },
});

function readFile({fileName}) {
    console.log("cli:readFile", fileName)
    try {
        const content = fs.readFileSync(fileName, "utf8");
        let documents = content.split("\n");
        documents = documents.slice(0, 100);

        console.log("documents.length", documents.length)

        return JSON.stringify(documents.join("\n"));
    } catch (e) {
        console.error("cli:readFile", e)
        return { error: "IOError" };
    }
}

function saveModel(model) {
    console.log("cli:saveModel", model)
    const json = JSON.stringify(model);

    if (!fs.existsSync(".models")){
        fs.mkdirSync(".models");
    }

    const path = `.models/${Date.now()}.json`;
    try {
        fs.writeFileSync(path, json);
        return path;
    } catch (e) {
        console.error("cli:saveModel", e)
        return { error: "IOError" };
    }
}