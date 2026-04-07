class TestbenchError(Exception):
    """Base error for the testbench package."""


class AddressError(TestbenchError):
    """Raised when an address access is invalid."""


class PageFaultError(TestbenchError):
    """Raised when page translation fails."""


class ProtocolError(TestbenchError):
    """Raised when protocol behavior is inconsistent."""


class DeadlockError(TestbenchError):
    """Raised when no forward progress is observed."""
