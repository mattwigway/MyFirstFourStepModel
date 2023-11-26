# Topcodes are used when creating the IPF seed matrix to avoid very small cells
VEHICLES_TOPCODE = 3
HHSIZE_TOPCODE = 4
WORKER_TOPCODE = 3

MILES_TO_KILOMETERS = 1.609

# How much of a time period's traffic occurs during the busiest hour?
PEAKING_FACTORS = list(
    `AM Peak`=0.35,
    `PM Peak`=0.45,
    Midday=0.2,
    Overnight=0.12
)