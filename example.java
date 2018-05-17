if (t instanceof VirtualMachineError) {
    throw (VirtualMachineError) t;
} else if (t instanceof ThreadDeath) {
    throw (ThreadDeath) t;
} else if (t instanceof LinkageError) {
    throw (LinkageError) t;
}
