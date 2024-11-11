# A script to generate ansatz circuits in OpenQASM 3 format from qiskit.
#
# Usage: ansatz_gen [number_of_qubits]

import os
import sys

from qiskit import transpile
from qiskit.circuit.library import RealAmplitudes
from qiskit.circuit.library import EfficientSU2
from qiskit.circuit.library import TwoLocal
from qiskit.circuit.library import ExcitationPreserving
from qiskit.providers.fake_provider import GenericBackendV2
from qiskit.qasm3 import dumps

# Bounds for the ansatz repetitions.
MIN_REPS = 1
MAX_REPS = 4

# List of analyzed ansatz circuits.
ansatz = [RealAmplitudes, EfficientSU2, TwoLocal, ExcitationPreserving]
names  = ["ra", "esu2", "tl", "ep"]

# List of entanglement schemes.
entanglement = ["full", "reverse_linear", "sca"]

# If low-level registers are used (i.e., $n), then introduces an array of qubits of the
# same size, and uses these qubits in place of registers. The number of qubits is taken
# as an input.
def lift_registers(qnum, text):
    qvar = "qs"

    # Checks if low-level registers are in use.
    if "$" in text:
        # Replaces low-level registers with high-level registers.
        for i in range(0, qnum):
            oldvar = "$" + str(i)
            newvar = qvar + "[" + str(i) + "]"
            text = text.replace(oldvar, newvar)

        # Adds a statement to define the high-level register.
        lines = text.splitlines()
        lines.insert(2, "qubit[" + str(qnum) + "] " + qvar + ";")
        text = "\n".join(lines)

    return text

# Replaces all float[64] parameters with a single array of angle parameters.
def fix_parameters(text):
    pvar = "ps"
    pcount = text.count("float[64]")

    # Merges the parameters into a single array, of the expected type (angle).
    lines = text.splitlines()
    lines = list(filter(lambda x: "float[64]" not in x, lines))
    lines.insert(2, "input array[angle," + str(pcount) + "] " + pvar + ";")
    text = "\n".join(lines)

    # Updates all parameter references to point to the new array.
    for i in range(0, pcount):
        oldvar = "_θ_" + str(i) + "_"
        newvar = pvar + "[" + str(i) + "]"
        text = text.replace(oldvar, newvar)

    return text

# Converts a quantum circuit to an OpenQASM 3 program. The number of qubits are taken as
# an input, to deal with low-level circuits where the number of qubits implemented on the
# hardware might exceed the number of qubits in the circuits. All parameters are merged
# into a single array of angles, and low-level qubits are lifted to variables.
def pecac_dumps(qnum, circ):
    text = dumps(circ)
    text = lift_registers(qnum, text)
    text = fix_parameters(text)
    return text + "\n"

# Argument parsing.
if len(sys.argv) != 2:
    print("Expected number of qubits as argument.")
    exit()
qnum = int(sys.argv[1])

# Generates fake backend.
sz = qnum
if sz == 1: sz = 2
backend = GenericBackendV2(sz)

# Creates the test directory.
dest = "ansatz"
if not os.path.exists(dest):
    os.makedirs(dest)

# Restricts the entanglement strategies based on the qubit count.
strategies = entanglement
if qnum <= 2:
    strategies = [entanglement[0]]

# Generates all circuts with the specified number of qubits.
for i in range(0, len(ansatz)):
    for e in strategies:
        for r in range(MIN_REPS, MAX_REPS):
            # Prepares the ansatz parameters, including ansatz specific parameters.
            params = dict()
            params["num_qubits"]   = qnum
            params["entanglement"] = e
            params["reps"]         = r
            params["flatten"]      = True
            if ansatz[i] == TwoLocal:
                params["rotation_blocks"] = ["ry"]

            # Generates the circuit, and its hardware transpiled representation.
            qc1 = ansatz[i](**params)
            qc2 = transpile(qc1, backend)

            # Logs the circuits to the specified files.
            bname = names[i] + "_" + e + "_q" + str(qnum) + "_r" + str(r)
            with open(dest + "/" + bname + ".original.qasm", "w") as f:
                f.write(pecac_dumps(qnum, qc1))
            with open(dest + "/" + bname + ".transpile.qasm", "w") as f:
                f.write(pecac_dumps(qnum, qc2))
