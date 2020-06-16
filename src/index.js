import ColorScheme from 'color-scheme';
import { Noise } from 'noisejs';

import { Elm } from './Main.elm';

var app = Elm.Main.init({ node: document.querySelector('main') });

app.ports.generateModelPort.subscribe(function ({ step, sparsity, scheme, style, width, height, skew, sizeRange }) {
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
    var lightConfigs = {
        modelWidth: width,
        modelHeight: height,
        step,
        sparsity,
        colors,
        skew,
        sizeRange,
    };
    var lights = generateLightsWithNoise(lightConfigs);

    app.ports.receivedModelPort.send({
        background,
        lights,
        step,
        sparsity,
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

function generateLightsWithNoise({ step, sparsity, modelWidth, modelHeight, colors, skew, sizeRange }) {
    var lights = [];
    var density = 1;
    var noise = new Noise(Math.random());
    for (var x = 0; x < modelWidth; x += step) {
        for (var y = 0; y < modelHeight; y += step) {
            var light = generateLightWithNoise({ modelWidth, modelHeight, colors, skew, sizeRange, noise, density, sparsity });
            if (light !== undefined) {
                lights.push(light);
            }
        }
    }
    return lights;
}

function generateLightWithNoise({ modelWidth, modelHeight, colors, skew, sizeRange, noise, density, sparsity }) {
    var x = getRandomBetween(0, modelWidth);
    var y = getRandomBetween(0, modelHeight);
    var nRange = 6;
    var nx = lerp(x, 0, modelWidth, 0, nRange);
    var ny = lerp(y, 0, modelWidth, 0, nRange);
    var nValue = lerp(noise.perlin2(nx, ny), -1, 1, 0, 1);
    var prob = lerp(nValue, sparsity, 1, 0, density);
    if (prob > Math.random()) {
        var width = lerp(prob, 0, density, sizeRange[0], sizeRange[1]);
        var height = width * getRandomBetween(skew[0], skew[1]);
        var color = getRandomElement(colors);
        return { x, y, width, height, color };
    } else {
        return undefined;
    }
}

function generateLightsWithRandom({ numberOfLights, modelWidth, modelHeight, colors, skew }) {
    var lights = [];
    for (var i = 0; i < numberOfLights; i++) {
        lights.push(generateLight({ modelWidth, modelHeight, colors, skew }));
    }
    return lights;
}

function generateLightWithRandom({ modelWidth, modelHeight, colors, skew, simplex }) {
    var x = getRandomBetween(0, modelWidth);
    var y = getRandomBetween(0, modelHeight);
    var width = getRandomBetween(20, 80);
    var height = width * getRandomBetween(skew[0], skew[1]);
    var color = getRandomElement(colors);
    return { x, y, width, height, color };
}

function lerp(source, sMin, sMax, tMin, tMax) {
    return tMin + (source - sMin) / (sMax - sMin) * (tMax - tMin);
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