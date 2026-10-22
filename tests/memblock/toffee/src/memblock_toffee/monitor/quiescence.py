from __future__ import annotations

from memblock_toffee.bundle import IdleBundle


class QuiescenceChecker:
    """Check the externally visible producer valids required to be idle."""

    def __init__(self, bundle: IdleBundle) -> None:
        self.bundle = bundle
        self.observations = 0

    def active_outputs(self) -> tuple[str, ...]:
        self.observations += 1
        active = []
        for name in self.bundle.current_level_signals:
            if bool(getattr(self.bundle, name).value):
                active.append(name)
        return tuple(active)

    def require_idle(self) -> None:
        active = self.active_outputs()
        if active:
            raise AssertionError(f"non-quiescent outputs: {', '.join(active)}")
