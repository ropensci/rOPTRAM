//VERSION=3

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B04", "B08"]
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "UINT8"
        }
    };
}

function evaluatePixel(sample) {
  // this computes the MSAVI value
    let msavi = (2 * sample.B08 + 1 - Math.sqrt(Math.pow(2 * sample.B08 + 1, 2)
                - 8 * (sample.B08 - sample.B04))) / 2;
    return [ msavi ];
}

