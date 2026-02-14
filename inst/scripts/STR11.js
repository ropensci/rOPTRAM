//VERSION=3

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B11"],
            units: "DN"
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "FLOAT32",
        }
    };
}

function evaluatePixel(sample) {
    var value = sample.B11;
    if (value != 0) {
      var v = value / 10000.0;
      str = ((1 - v)**2) / (2*v);
    } else {
      str = 0;
    };
    return [ str ];
}
