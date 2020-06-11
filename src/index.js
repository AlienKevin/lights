import ColorScheme from 'color-scheme';

import { Elm } from './Main.elm';

var app = Elm.Main.init({ node: document.querySelector('main') });

app.ports.generateModelPort.subscribe(function ({ scheme, style, width, height }) {
    var startColor = getRandomBetween(50, 200);
    var colorScheme = new ColorScheme;
    colorScheme.from_hue(startColor)
        .scheme(scheme.scheme)
        .distance(withDefault(0, scheme.distance))
        .add_complement(withDefault(false, scheme.complemented))
        .variation(style);
    
    var colors = colorScheme.colors().map(function(color) {
        var alpha = getRandomIntBetween(50, 200).toString(16);
        return color + alpha;
    });
    var background = getRandomElement(colors);
    var lights = generateLights(width, height, colors);

    app.ports.receivedModelPort.send({
        background,
        lights,
        scheme,
        style,
        width,
        height,
    });
});

function withDefault(defaultValue, value) {
    if (value === undefined) {
        return defaultValue;
    }
    return value;
}

function generateLights(modelWidth, modelHeight, colors) {
    var lights = [];
    for (var i = 0; i < 20; i++) {
        lights.push(generateLight(modelWidth, modelHeight, colors));
    }
    return lights;
}

function generateLight(modelWidth, modelHeight, colors) {
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