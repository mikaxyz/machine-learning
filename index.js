#! /usr/bin/env node

const fs = require("fs");
const yargs = require("yargs");
const ConcurrentTask = require("@andrewmacmurray/elm-concurrent-task");

console.time("total");
process.on("beforeExit", () => {
    console.timeEnd("total");
});

yargs
    .command({
        command: 'train',
        describe: 'Trains a model with some cvs training data',
        builder: {
            path: {
                describe: 'File name',
                demandOption: false,
                default: ".data/mnist_train.csv",
                type: 'string'
            },
            limit: {
                describe: 'Limit',
                demandOption: false,
                type: 'int'
            }
        },
        handler({path, limit}) {
            console.log("Training: ", path, limit);
            train({path, limit});
        }
    })
    .parse();


function train({path, limit}) {
    if (!fs.existsSync(path)) {
        return console.error(`File (${path}) not found`);
    }

    const {Elm} = require("./dist/main.js");
    const app = Elm.Train.init({flags: {fileName: path}});

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

            if (limit) {
                documents = documents.slice(0, limit);
            }

            console.log("documents.length", documents.length)

            return JSON.stringify(documents.join("\n"));
        } catch (e) {
            console.error("cli:readFile", e)
            return {error: "IOError"};
        }
    }

    function saveModel(model) {
        console.log("cli:saveModel", model)
        const json = JSON.stringify(model);

        if (!fs.existsSync(".models")) {
            fs.mkdirSync(".models");
        }

        const size = limit || "ALL";
        const path = `.models/${Date.now()}_${size}.json`;

        try {
            fs.writeFileSync(path, json);
            return path;
        } catch (e) {
            console.error("cli:saveModel", e)
            return {error: "IOError"};
        }
    }
}