import ColorScheme from 'color-scheme';

import { Elm } from './Main.elm';

var app = Elm.Main.init({ node: document.querySelector('main') });

app.ports.generateModelPort.subscribe(function ({ numberOfLights, scheme, style, width, height, skew }) {
    var startColor = getRandomBetween(50, 200);
    var colorScheme = new ColorScheme;
    colorScheme.from_hue(startColor)
        .scheme(scheme.scheme)
        .distance(withDefault(0, scheme.distance))
        .add_complement(withDefault(false, scheme.complemented))
        .variation(style);

    var colors = colorScheme.colors().map(function (color) {
        var alpha = getRandomIntBetween(50, 200).toString(16);
        return color + alpha;
    });
    var background = getRandomElement(colors);
    var lights = generateLights({
        numberOfLights,
        modelWidth: width,
        modelHeight: height,
        colors,
        skew
    });

    app.ports.receivedModelPort.send({
        background,
        numberOfLights,
        lights,
        scheme,
        style,
        width,
        height,
        skew,
    });
});

function withDefault(defaultValue, value) {
    if (value === undefined) {
        return defaultValue;
    }
    return value;
}

function generateLights({ numberOfLights, modelWidth, modelHeight, colors, skew }) {
    var lights = [];
    for (var i = 0; i < numberOfLights; i++) {
        lights.push(generateLight({modelWidth, modelHeight, colors, skew}));
    }
    return lights;
}

function generateLight({modelWidth, modelHeight, colors, skew}) {
    var x = getRandomBetween(0, modelWidth);
    var y = getRandomBetween(0, modelHeight);
    var width = getRandomBetween(20, 80);
    var height = width * getRandomBetween(skew[0], skew[1]);
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