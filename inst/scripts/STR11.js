//VERSION=3

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B11"]
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "INT16",
        }
    };
}

function evaluatePixel(sample) {
    var value = sample.B11;
    if (value != 0) {
      v = value / 10000
      str = ((1 - v)**2) / (2*v);
    } else {
      str = 0
    };
    return [ str ];
}
