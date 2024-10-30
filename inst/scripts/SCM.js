 //VERSION=3

function setup() {
  return {
    input: [{
      bands: ["SCL"],
    }],
    output: [{
      bands: 1, sampleType: SampleType.UINT8
    }],
  }
}

function evaluatePixel(sample) {
  var scl = [sample.SCL]
  return [ scl ]
}

