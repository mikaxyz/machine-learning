#! /usr/bin/env node

const fs = require("fs");
const yargs = require("yargs");
const ConcurrentTask = require("@andrewmacmurray/elm-concurrent-task");
const {Elm} = require("./dist/main.js");

console.time("total");
process.on("beforeExit", () => {
    console.timeEnd("total");
});

const readMnistCsv = (limit) => ({fileName}) => {
    console.log("cli:readMnistCsv", fileName)
    try {
        const content = fs.readFileSync(fileName, "utf8");
        let documents = content.split("\n");

        if (limit) {
            documents = documents.slice(0, limit);
        }

        return JSON.stringify(documents.join("\n"));
    } catch (e) {
        console.error("cli:readMnistCsv", e)
        return {error: "IOError"};
    }
}

const readModel = (modelPath) => {
    console.log("cli:readModel", fileName)
    try {
        return fs.readFileSync(modelPath, "utf8");
    } catch (e) {
        console.error("cli:readModel", e)
        return {error: "IOError"};
    }
}


const saveModel = (limit) => (model) => {
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

yargs
    .command({
        command: 'train',
        describe: 'Trains a model with some csv training data',
        builder: {
            dataPath: {
                describe: 'File name',
                demandOption: false,
                default: ".data/mnist_train.csv",
                type: 'string'
            },
            dataLimit: {
                describe: 'Limit',
                demandOption: false,
                type: 'int'
            }
        },
        handler({dataPath, dataLimit}) {
            console.log("Training: ", dataPath, dataLimit);
            train({dataPath, dataLimit});
        }
    })
    .command({
        command: 'test',
        describe: 'Tests a model with some csv testing data',
        builder: {
            modelPath: {
                describe: 'Model path',
                demandOption: true,
                type: 'string'
            },
            dataPath: {
                describe: 'Test data path',
                demandOption: false,
                default: ".data/mnist_test.csv",
                type: 'string'
            },
            dataLimit: {
                describe: 'Limit',
                demandOption: false,
                type: 'int'
            }
        },
        handler({modelPath, dataPath, dataLimit}) {
            console.log("Testing: ", modelPath, dataPath, dataLimit);
            test({modelPath, dataPath, dataLimit});
        }
    })
    .parse();


function train({dataPath, dataLimit}) {
    if (!fs.existsSync(dataPath)) {
        return console.error(`Training data (${dataPath}) not found`);
    }

    const app = Elm.Main.init({flags: {command: "train", fileName: dataPath}});

    ConcurrentTask.register({
        tasks: {
            "cli:readMnistCsv": readMnistCsv(dataLimit),
            "cli:saveModel": saveModel(dataLimit)
        },
        ports: {
            send: app.ports.send,
            receive: app.ports.receive,
        },
    });
}

function test({modelPath, dataPath, dataLimit}) {
    if (!fs.existsSync(modelPath)) {
        return console.error(`Model (${modelPath}) not found`);
    }
    if (!fs.existsSync(dataPath)) {
        return console.error(`Test data (${dataPath}) not found`);
    }

    const model = fs.readFileSync(modelPath, "utf8");

    const app = Elm.Main.init({flags: {
            command: "test", model: JSON.parse(model),
            testDataPath: dataPath
        }
    });

    ConcurrentTask.register({
        tasks: {
            "cli:readModel": readModel,
            "cli:readMnistCsv": readMnistCsv(dataLimit)
        },
        ports: {
            send: app.ports.send,
            receive: app.ports.receive,
        },
    });
}