"""Repository-level pytest plugin registration for Frontend tests."""

import sys
from pathlib import Path

_FRONTEND_ROOT = Path(__file__).resolve().parent / "src" / "test" / "python" / "Frontend"
if str(_FRONTEND_ROOT) not in sys.path:
    sys.path.insert(0, str(_FRONTEND_ROOT))

pytest_plugins = [
    "Frontend_api",
    "Frontend_env",
]
