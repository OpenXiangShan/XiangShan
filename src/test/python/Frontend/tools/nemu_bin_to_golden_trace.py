from pathlib import Path
import sys


_TOOLS_ROOT = Path(__file__).resolve().parent
_FRONTEND_ROOT = _TOOLS_ROOT.parent
_REPO_ROOT = _FRONTEND_ROOT.parents[3]
_PYLIB = _REPO_ROOT / "build-frontend" / "pylib"

for _path in (str(_FRONTEND_ROOT), str(_PYLIB)):
    if _path not in sys.path:
        sys.path.insert(0, _path)

from env.nemu_trace_pipeline import main


if __name__ == "__main__":
    raise SystemExit(main())
