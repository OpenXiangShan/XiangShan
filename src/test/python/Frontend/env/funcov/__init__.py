"""Compatibility exports for frontend functional coverage samplers."""

from .py.ftq import sampler as _ftq
from .py.ifu import sampler as _ifu

for _sampler in (_ifu, _ftq):
    globals().update(
        {name: getattr(_sampler, name) for name in dir(_sampler) if not name.startswith("__")}
    )
