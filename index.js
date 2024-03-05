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
        describe: 'Trains a model with some csv training data',
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
    .command({
        command: 'test',
        describe: 'Tests a model with some csv testing data',
        builder: {
            model: {
                describe: 'Model path',
                demandOption: true,
                type: 'string'
            },
            data: {
                describe: 'Test data path',
                demandOption: false,
                default: ".data/mnist_test.csv",
                type: 'string'
            },
            limit: {
                describe: 'Limit',
                demandOption: false,
                type: 'int'
            }
        },
        handler({model, data, limit}) {
            console.log("Testing: ", model, data, limit);
            test({model, data, limit});
        }
    })
    .parse();


function train({path, limit}) {
    if (!fs.existsSync(path)) {
        return console.error(`File (${path}) not found`);
    }

    const {Elm} = require("./dist/main.js");
    const app = Elm.Main.init({flags: {command: "train", fileName: path}});

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

function test({model, data, limit}) {
    if (!fs.existsSync(model)) {
        return console.error(`Model file (${model}) not found`);
    }
    if (!fs.existsSync(data)) {
        return console.error(`Test data file (${data}) not found`);
    }

    const {Elm} = require("./dist/main.js");
    const modelJson = fs.readFileSync(model, "utf8");

    const app = Elm.Main.init({flags: {
            command: "test", model: JSON.parse(modelJson),
            testDataPath: data
        }
    });

    ConcurrentTask.register({
        tasks: {
            "cli:readModel": readModel,
            "cli:readFile": readFile
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

            return JSON.stringify(documents.join("\n"));
        } catch (e) {
            console.error("cli:readFile", e)
            return {error: "IOError"};
        }
    }

    function readModel(modelPath) {
        console.log("cli:readModel", fileName)
        try {
            const data = fs.readFileSync(modelPath, "utf8");
            return data;
        } catch (e) {
            console.error("cli:readModel", e)
            return {error: "IOError"};
        }
    }
}