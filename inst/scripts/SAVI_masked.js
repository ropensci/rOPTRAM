//VERSION=3

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B04", "B08"]
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "FLOAT32"
        }
    };
}

function evaluatePixel(sample) {
  // this computes the SAVI value
    let savi = 1.5 * (sample.B08 - sample.B04) /
              (sample.B08 + sample.B04 + 0.5);
    if ([2, 4, 5, 10].includes(sample.SCL)) {
      // mask out all cloud, cloud shadow, water, snow
      return [ savi ];
    } else {
      return [NaN];
    };
}

