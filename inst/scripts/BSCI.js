//VERSION=3

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B03","B04", "B08"]
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "UINT8"
        }
    };
}

function evaluatePixel(sample) {
  // this computes the BSCI value
  function mean() {
        var sum = 0;
        var count = arguments.length;
        for (var i = 0; i < count; i++) {
            sum += arguments[i];
        }
        return sum / count;
    }

    let bsci = (1 - 2 * (sample.B04 - sample.B03)) / 
               mean(sample.B03, sample.B04, sample.B08);
    return [ bsci ];
}

