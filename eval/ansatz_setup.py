from qiskit import transpile
from qiskit.circuit.library import TwoLocal

# Helper method for consistent generation of test circuits for evaluation.
def make_ansatz(builder, qnum, entanglement, reps, backend):
    # Prepares the ansatz parameters, including ansatz specific parameters.
    params = dict()
    params["num_qubits"]   = qnum
    params["entanglement"] = entanglement
    params["reps"]         = reps
    params["flatten"]      = True
    if builder == TwoLocal:
        params["rotation_blocks"] = ["ry"]

    # Generates the circuit, and its hardware transpiled representation.
    qc1 = builder(**params)
    qc2 = transpile(qc1, backend)
    return qc1, qc2
