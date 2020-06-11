import ColorScheme from 'color-scheme';

import { Elm } from './Main.elm';

var app = Elm.Main.init({ node: document.querySelector('main') });

app.ports.generateModel.subscribe(function ({ style, width, height }) {
    var startColor = getRandomBetween(50, 200);
    var colorScheme = new ColorScheme;
    colorScheme.from_hue(startColor)
        .scheme('triade')
        .variation(style);
    var colors = colorScheme.colors().map(function(color) {
        var alpha = getRandomIntBetween(50, 200).toString(16);
        return color + alpha;
    });
    console.log('AL: colors', colors);
    var background = getRandomElement(colors);
    var filters = generateFilters(width, height, colors);

    app.ports.receiveModel.send({
        background,
        filters,
        style,
        width,
        height,
    });
});

function generateFilters(modelWidth, modelHeight, colors) {
    var filters = [];
    for (var i = 0; i < 20; i++) {
        filters.push(generateFilter(modelWidth, modelHeight, colors));
    }
    console.log('AL: filters', filters)
    return filters;
}

function generateFilter(modelWidth, modelHeight, colors) {
    var x = getRandomBetween(0, modelWidth);
    var y = getRandomBetween(0, modelHeight);
    var width = getRandomBetween(20, 80);
    var height = width * getRandomBetween(0.8, 1.2);
    var color = getRandomElement(colors);
    return { x, y, width, height, color };
}

function getRandomBetween(min, max) {
    return Math.random() * (max - min) + min;
}

function getRandomIntBetween(min, max) {
    return Math.floor(getRandomBetween(min, max));
}

function getRandomElement(array) {
    if (array.length === 0) {
        return undefined;
    } else {
        return array[getRandomIntBetween(0, array.length)];
    }
}