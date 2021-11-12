"use scrict";

const os = require("os");
const readline = require("readline");
const { stdout } = require("process");

exports.getCols = () => stdout.columns;

exports.getRows = () => stdout.rows;

exports.getEol = () => os.EOL;

exports.write = str => () => stdout.write(str);

exports.cursorTo = ({ x, y }) => () => stdout.cursorTo(x, y);

exports.moveCursor = ({ x, y }) => () => stdout.moveCursor(x, y);

exports.clearScreenDown = callback => () =>
  stdout.clearScreenDown(() => callback());

exports.emitKeypressEvents = readable => () => {
  readline.emitKeypressEvents(readable);
};

exports.setRawMode = isRaw => readable => () => {
  readable.setRawMode(isRaw);

};

exports._onKeypress = just => nothing => callback => readable => () => {
  readable.on("keypress", (ch, { sequence, name, ctrl, meta, shift }) => {
    callback({
      sequence: sequence === undefined ? nothing : just(sequence),
      name: name === undefined ? nothing : just(name),
      ctrl,
      meta,
      shift
    })();
  });
}
