import numpy as np

from qiskit import transpile
from qiskit.circuit import QuantumCircuit
from qiskit.circuit.library import TwoLocal
from qiskit.circuit.library import ExcitationPreserving
from qiskit.converters import circuit_to_dag
from qiskit.transpiler import TransformationPass

# Pass to replace rxx and ryy gates with their implementation in OpenQASM.
# This is intended for the ExcitationPreserving ansatz circuit.
#
# Based on: https://quantumcomputing.stackexchange.com/questions/22149/
class EPTranslator(TransformationPass):
    def run(self, dag):
        # Searches for rxx and ryy gates.
        for node in dag.op_nodes():
            # Determines if this gate is a match.
            if node.op.name in ["rxx", "ryy"]:
                angle = node.op.params[0]
                replacement = QuantumCircuit(2)

                # Determines which replacement this should be.
                if node.op.name == "rxx":
                    replacement.h([0, 1])
                    replacement.cx(0, 1)
                    replacement.rz(angle, 1)
                    replacement.cx(0, 1)
                    replacement.h([0, 1])
                if node.op.name == "ryy":
                    replacement.rx(np.pi / 2, [0, 1])
                    replacement.cx(0, 1)
                    replacement.rz(angle, 1)
                    replacement.cx(0, 1)
                    replacement.rx(-np.pi / 2, [0, 1])

                dag.substitute_node_with_dag(node, circuit_to_dag(replacement))

        return dag

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

    # Inlines the rxx and ryy gates in ExcitationPreserving ansatz circuits.
    if qnum > 1 and builder == ExcitationPreserving:
        qc1 = EPTranslator()(qc1)
    
    return qc1, qc2
