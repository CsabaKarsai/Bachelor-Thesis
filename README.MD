# Sudoku

### Table of Contents

- [Description](#description)
- [Requirements](#requirements)
- [Usage](#usage)
- [Notes](#notes)
- [Project Status](#project-status)
- [License](#license)

## Description

The idea behind the Thesis was to simulate a VNF processing telecommunications signaling traffic. The simulation supports Gamma distributed processing times and a Poisson arrival process.
During a simulation, incoming traffic for the VNF is generated via the Gatling Load Generator. After the simulation is complete, the resulting datasets can be analyzed by executing parts of the R script.
Due to their size, the datasets analyzed for the thesis are not included.

## Requirements

- JDK version 1.8.0_251 installed
- Scala version 2.11.11 installed
- R version 4.0.2 installed
- Maven version 3.6.3 installed

## Usage

1. Close this repository (only needed once initially).
2. Edit the `setup()` section in `MySimulation.scala` to customize simulation parameters.
2. Run the `main()` function inside `RunMySimulation.scala` to execute the simulation defined under `MySimulation`.
3. Wait for the simulation to complete (you should see a lot of log messages).
4. Manually stop the process. You should now see a new folder prefixed with `mysimulation-`, as well as some datasets for `Workers` and `Supervisors` under `results`.
6. Analyze the datasets by executing the functions provided in the R script under `analysis`.

## Notes

- The `Parameters.txt` file exists to provide an overview of the currently configured simulation. It is not necessary for a functional simulation and is of no further use.

## Project Status

This project is complete and will not be developed any further.
However, if you have suggestions for improvements or feel that something is missing, do not hesitate to reach out!

## License

This project is licensed under the GNU General Public License v3.0.