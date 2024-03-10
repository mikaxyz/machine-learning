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


const saveModel = (dataPath) => ({fileName, neuralNetwork}) => {
    console.log("cli:saveModel", neuralNetwork)
    const json = JSON.stringify(neuralNetwork);

    if (!fs.existsSync(".models")) {
        fs.mkdirSync(".models");
    }

    const dataFileName = dataPath.split("/").at(-1).replace(".", "_");
    const path = `.models/${fileName}.${dataFileName}.json`;

    try {
        fs.writeFileSync(path, json);
        console.log(`Saved model: : "${path}"`);
        return path;
    } catch (e) {
        console.error("cli:saveModel", e)
        return {error: "IOError"};
    }
}

const saveReport = (dataPath, dataLimit = "ALL") => ({fileName, content}) => {
    console.log("saveReport", fileName, content)

    if (!fs.existsSync(".reports")) {
        fs.mkdirSync(".reports");
    }

    const dataFileName = dataPath.split("/").at(-1).replace(".", "_");
    const path = `.reports/${fileName}.${dataFileName}_${dataLimit}.txt`;

    try {
        fs.writeFileSync(path, content);
        return path;
    } catch (e) {
        console.error("cli:saveReport", e)
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
            },
            learningRate: {
                describe: 'Learning rate',
                demandOption: false,
                default: 0.01,
                type: 'float'
            },
            activationFunction: {
                describe: 'Activation function',
                demandOption: false,
                default: "tanh",
                type: 'string'
            },
            seed: {
                describe: 'Random seed',
                demandOption: false,
                default: 42,
                type: 'int'
            },
            neurons: {
                describe: 'A layer of neurons (can be used multiple times)',
                demandOption: false,
                default: [100],
                type: 'int'
            }
        },
        handler(args) {
            console.log("Training: ", args);
            train(args);
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


function train({dataPath, dataLimit, learningRate, activationFunction, neurons, seed}) {
    if (!fs.existsSync(dataPath)) {
        return console.error(`Training data (${dataPath}) not found`);
    }

    console.log('dataPath', dataPath);

    const layers = Array.isArray(neurons) ? neurons : [neurons];

    const app = Elm.Main.init({
        flags: {
            command: "train",
            fileName: dataPath,
            learningRate: learningRate,
            activationFunction: activationFunction,
            seed: seed,
            layers: layers
        }
    });

    ConcurrentTask.register({
        tasks: {
            "cli:readMnistCsv": readMnistCsv(dataLimit),
            "cli:saveModel": saveModel(dataPath)
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
            "cli:readMnistCsv": readMnistCsv(dataLimit),
            "cli:saveReport": saveReport(dataPath, dataLimit)
        },
        ports: {
            send: app.ports.send,
            receive: app.ports.receive,
        },
    });
}