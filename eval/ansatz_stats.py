# A script to generate summary statistics about ansatz circuits in qiskit.

import xlsxwriter

from qiskit import transpile
from qiskit.circuit.library import RealAmplitudes
from qiskit.circuit.library import EfficientSU2
from qiskit.circuit.library import TwoLocal
from qiskit.circuit.library import ExcitationPreserving
from qiskit.providers.fake_provider import GenericBackendV2

# Bounds for the ansatz summary.
MIN_QUBITS = 1
MAX_QUBITS = 5
MIN_REPS   = 1
MAX_REPS   = 4

# List of analyzed ansatz circuits.
ansatz = [RealAmplitudes, EfficientSU2, TwoLocal, ExcitationPreserving]
names  = ["RA", "ESU2", "TL", "EP"]

# List of entanglement schemes.
entanglement = ["full", "linear", "reverse_linear", "circular", "sca"]

# Creates a workbook to log the summary to.
workbook = xlsxwriter.Workbook("AnsatzSummary.xlsx")
worksheet = workbook.add_worksheet()

# Adds a header to the workbook.
bold = workbook.add_format({"bold": True})
worksheet.write(0, 0, "Ansats", bold)
worksheet.write(0, 1, "Qubits", bold)
worksheet.write(0, 2, "Scheme", bold)
worksheet.write(0, 3, "Reps", bold)
worksheet.write(0, 4, "Parameters", bold)
worksheet.write(0, 5, "Gates (hl)", bold)
worksheet.write(0, 6, "Gates (ll)", bold)

# Generates a backend with the maximum number of qubits.
backend = GenericBackendV2(MAX_QUBITS)

# Formatting to highlight large values.
alert = workbook.add_format({"bg_color": "yellow"})
excpt = workbook.add_format({"bg_color": "red"})

# Populates the table with ansatz statistics.
row = 1
for i in range(0, len(ansatz)):
    print("Summarizing " + names[i] + ".")
    for n in range(MIN_QUBITS, MAX_QUBITS):
        for e in entanglement:
            for r in range(MIN_REPS, MAX_REPS):
                # Prepares the ansatz parameters, including ansatz specific parameters.
                params = dict()
                params["num_qubits"]   = n
                params["entanglement"] = e
                params["reps"]         = r
                params["flatten"]      = True
                if ansatz[i] == TwoLocal:
                    params["rotation_blocks"] = ["ry"]

                # Generates the circuit, and its hardware transpiled representation.
                qc1 = ansatz[i](**params)
                qc2 = transpile(qc1, backend)

                # Logs the circuit metrics to the workbook.
                worksheet.write(row, 0, names[i])
                worksheet.write(row, 1, n)
                worksheet.write(row, 2, e)
                worksheet.write(row, 3, r)
                if qc1.num_parameters < 6:
                    worksheet.write(row, 4, qc1.num_parameters)
                elif qc1.num_parameters < 9:
                    worksheet.write(row, 4, qc1.num_parameters, alert)
                else:
                    worksheet.write(row, 4, qc1.num_parameters, excpt)
                worksheet.write(row, 5, qc1.size())
                worksheet.write(row, 6, qc2.size())
                row += 1

# Closes the workbook.
workbook.close()
