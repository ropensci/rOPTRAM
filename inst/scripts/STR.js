//VERSION=3

function setup() {
    return {
        input: [{ // this sets which bands to use
            bands: ["B11"],
            units: "DN"
            }],
        output: { // this defines the output image type
            bands: 1,
            sampleType: "UINT8"
        }
    };
}

function evaluatePixel(sample) {
  // this computes the STR value
    SWIR_DN = sample.B11;
    SWIR =  SWIR_DN / 10000;
    // Convert from Solar irradiance
    // solar_irradiance_12 <- 87.25
    // SWIR <- (SWIR_irr/10) * solar_irradiance_12
    
    let str = (1 - SWIR)**2 / (2*SWIR);
    return [ str ];
}

