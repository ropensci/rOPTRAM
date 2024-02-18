//VERSION=3

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B02","B04", "B08"]
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "FLOAT32"
        }
    };
}

function evaluatePixel(sample) {
  // this computes the CI value
    let ci = 1 - (sample.B04 - sample.B02) / (sample.B04 + sample.B02);
    return [ ci ];
}

